-----------------------------------------------------------------------
--  asf.server -- ASF Server for AWS
--  Copyright (C) 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with AWS.Templates;
with AWS.MIME;
with AWS.Messages;
with AWS.Services.Web_Block.Registry;

with Ada.Strings.Fixed;
with Ada.Exceptions;

with EL.Beans;
with ASF.Beans;
with ASF.Applications.Views;
with ASF.Components.Core;
with ASF.Contexts.Faces;
with ASF.Contexts.Writer.String;

with EL.Objects;
with EL.Contexts;
with EL.Contexts.Default;
with EL.Variables;
with EL.Variables.Default;

with Ada.Strings.Unbounded;
with Util.Log.Loggers;
with Ada.Containers.Vectors;
package body ASF.Server.Web is

   use Util.Log;
   use ASF.Contexts;
   use Ada.Exceptions;
   use AWS.Templates;
   use ASF.Beans;
   use Ada.Strings.Unbounded;

   Log : constant Loggers.Logger := Loggers.Create ("ASF.Server.Web");
   --  The logger

   function Reply_Page_500 (Request : in AWS.Status.Data;
                            E       : in Exception_Occurrence)
                            return AWS.Response.Data;

   function Dispatch (App  : Main.Application_Access;
                      Page : String;
                      Request : in AWS.Status.Data) return AWS.Response.Data;

   --  Binding to record the ASF applications and bind them to URI prefixes.
   type Binding is record
      Application : Main.Application_Access;
      Base_URI    : access String;
   end record;
   type Binding_Array is array (Natural range <>) of Binding;
   type Binding_Array_Access is access all Binding_Array;

   Nb_Bindings  : Natural := 0;
   Applications : Binding_Array_Access := null;

   type Bean_Object is record
      Bean : EL.Beans.Readonly_Bean_Access;
      Free : ASF.Beans.Free_Bean_Access;
   end record;

   package Bean_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Bean_Object);

   type Bean_Vector_Access is access all Bean_Vectors.Vector;

   --  ------------------------------
   --  Default Resolver
   --  ------------------------------
   type Web_ELResolver is new EL.Contexts.ELResolver with record
      Request     : EL.Contexts.Default.Default_ELResolver_Access;
      Application : Main.Application_Access;
      Beans       : Bean_Vector_Access;
   end record;

   overriding
   function Get_Value (Resolver : Web_ELResolver;
                       Context  : EL.Contexts.ELContext'Class;
                       Base     : access EL.Beans.Readonly_Bean'Class;
                       Name     : Unbounded_String) return EL.Objects.Object;
   overriding
   procedure Set_Value (Resolver : in Web_ELResolver;
                        Context  : in EL.Contexts.ELContext'Class;
                        Base     : access EL.Beans.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in EL.Objects.Object);

   --  Get the value associated with a base object and a given property.
   overriding
   function Get_Value (Resolver : Web_ELResolver;
                       Context  : EL.Contexts.ELContext'Class;
                       Base     : access EL.Beans.Readonly_Bean'Class;
                       Name     : Unbounded_String) return EL.Objects.Object is
      use EL.Objects;
      use EL.Beans;
      use EL.Variables;

      Result : Object := Resolver.Request.Get_Value (Context, Base, Name);
      Bean   : EL.Beans.Readonly_Bean_Access;
      Free   : ASF.Beans.Free_Bean_Access;
      Scope  : Scope_Type;
   begin
      if not EL.Objects.Is_Null (Result) then
         return Result;
      end if;
      if Name = "contextPath" then
         return EL.Objects.To_Object (String '("/am"));
      end if;
      Resolver.Application.Create (Name, Bean, Free, Scope);
      if Bean = null then
         raise No_Variable
           with "Bean not found: '" & To_String (Name) & "'";
      end if;
      Resolver.Beans.Append (Bean_Object '(Bean, Free));
      Result := To_Object (Bean);
      Resolver.Request.Register (Name, Result);
      return Result;
   end Get_Value;

   --  Set the value associated with a base object and a given property.
   overriding
   procedure Set_Value (Resolver : in Web_ELResolver;
                        Context  : in EL.Contexts.ELContext'Class;
                        Base     : access EL.Beans.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in EL.Objects.Object) is
   begin
      Resolver.Request.Set_Value (Context, Base, Name, Value);
   end Set_Value;


   --  Register the application to serve requests
   procedure Register_Application (URI    : in String;
                                   App    : in Main.Application_Access) is

      Apps : constant Binding_Array_Access := new Binding_Array (1 .. Nb_Bindings + 1);
   begin
      if Applications /= null then
         Apps (1 .. Nb_Bindings) := Applications (1 .. Nb_Bindings);
      end if;
      Nb_Bindings := Nb_Bindings + 1;
      Apps (Apps'Last).Application := App;
      Apps (Apps'Last).Base_URI := new String '(URI);
      Applications := Apps;
   end Register_Application;

   function Dispatch (App  : Main.Application_Access;
                      Page : String;
                      Request : in AWS.Status.Data) return AWS.Response.Data is

      use ASF;
      use ASF.Contexts.Faces;
      use ASF.Applications.Views;

      use EL.Contexts.Default;
      use EL.Variables;
      use EL.Variables.Default;
      use EL.Contexts;
      use EL.Objects;
      use EL.Beans;

      Writer   : aliased Contexts.Writer.String.String_Writer;
      Context  : aliased Faces_Context;
      View     : Components.Core.UIViewRoot;
      ELContext : aliased EL.Contexts.Default.Default_Context;
      Variables : aliased Default_Variable_Mapper;
      Req_Resolver   : aliased Default_ELResolver;
      Root_Resolver  : aliased Web_ELResolver;

      Beans    : aliased Bean_Vectors.Vector;
      --  Get the view handler
      Handler   : constant access View_Handler'Class := App.Get_View_Handler;
   begin
      Root_Resolver.Application := App;
      Root_Resolver.Request := Req_Resolver'Unchecked_Access;
      Root_Resolver.Beans := Beans'Unchecked_Access;
      ELContext.Set_Resolver (Root_Resolver'Unchecked_Access);
      ELContext.Set_Variable_Mapper (Variables'Unchecked_Access);

      Context.Set_Response_Writer (Writer'Unchecked_Access);
      Context.Set_ELContext (ELContext'Unchecked_Access);
      Writer.Initialize ("text/xml", "UTF-8", 8192);

      Context.Set_Request (Request'Unrestricted_Access);
      Set_Current (Context'Unchecked_Access);
      begin
         Handler.Restore_View (Page, Context, View);

      exception
         when E : others =>
            Log.Error ("Error when restoring view {0}: {1}: {2}", Page,
                       Exception_Name (E), Exception_Message (E));
            raise;
      end;

      begin
         Handler.Render_View (Context, View);

      exception
         when E: others =>
            Log.Error ("Error when restoring view {0}: {1}: {2}", Page,
                       Exception_Name (E), Exception_Message (E));
            raise;
      end;
      Writer.Flush;

      declare
         C : Bean_Vectors.Cursor := Beans.First;
      begin
         while Bean_Vectors.Has_Element (C) loop
            declare
               Bean : Bean_Object := Bean_Vectors.Element (C);
            begin
               if Bean.Bean /= null then
                  Bean.Free (Bean.Bean);
               end if;
            end;
            Bean_Vectors.Next (C);
         end loop;
      end;
      return AWS.Response.Build (Content_Type => Writer.Get_Content_Type,
                                 UString_Message => Writer.Get_Response);
   end Dispatch;

   ----------------------
   --  Main server callback
   ----------------------
   function Server_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      use Ada.Strings.Fixed;

      URI        : constant String := AWS.Status.URI (Request);
      Slash_Pos  : constant Natural := Index (URI, "/", URI'First + 1);
      Prefix_End : Natural;
   begin
      --  Find the module and action to invoke
      if Slash_Pos > 1 then
         Prefix_End := Slash_Pos - 1;
      else
         Prefix_End := URI'Last;
      end if;
      declare
         Prefix : constant String := URI (URI'First .. Prefix_End);
         Page   : constant String := URI (Prefix_End + 1 .. URI'Last);
      begin
         for I in Applications.all'Range loop
            if Applications (I).Base_URI.all = Prefix then
               return Dispatch (Applications (I).Application, Page, Request);
            end if;
         end loop;
      end;
      return AWS.Response.Build ("text/html", "<p>Unknown application</p>");
   exception
      when E : others =>
         return Reply_Page_500 (Request, E);
   end Server_Callback;


   --  Return a 500 error
   function Reply_Page_500 (Request : in AWS.Status.Data;
                            E       : in Exception_Occurrence)
                            return AWS.Response.Data is
      use type AWS.Messages.Status_Code;
      use AWS.Services;
      use AWS;

      Translations : Templates.Translate_Set;
      URI          : constant String := AWS.Status.URI (Request);
      Name         : constant String := Exception_Name (E);
      Message      : constant String := Exception_Message (E);
      ContentType  : constant String := Web_Block.Registry.Content_Type (URI);
   begin
      Templates.Insert (Translations,
                        Templates.Assoc ("URI", URI));
      Templates.Insert (Translations,
                        Templates.Assoc ("EXCEPTION_NAME", Name));
      Templates.Insert (Translations,
                        Templates.Assoc ("EXCEPTION_MESSAGE", Message));

      Log.Error ("Default_Callback exception for URI '{0}': {1}: {2}",
                 URI, Name, Message);

      if ContentType = AWS.MIME.Text_HTML then
         return AWS.Response.Build
           (Content_Type => AWS.MIME.Text_HTML,
            Message_Body => String'(Templates.Parse
              ("errors/exception.thtml",
                 Translations)));
      else
         return AWS.Response.Build
           (Content_Type => AWS.MIME.Text_XML,
            Message_Body => String'(Templates.Parse
              ("errors/exception.txml",
                 Translations)));
      end if;
   end Reply_Page_500;

end ASF.Server.Web;
