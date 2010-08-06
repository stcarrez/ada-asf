-----------------------------------------------------------------------
--  applications -- Ada Web Application
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

with Util.Log.Loggers;

with ASF.Contexts.Faces;
with ASF.Components;
with ASF.Components.Core;

with EL.Expressions;
with EL.Contexts.Default;

with Ada.Exceptions;
with Ada.Containers.Vectors;
package body ASF.Applications.Main is

   use Util.Log;
   use Ada.Strings.Unbounded;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Applications.Main");

   --  ------------------------------
   --  Get the application view handler.
   --  ------------------------------
   function Get_View_Handler (App : access Application)
                              return access Views.View_Handler'Class is
   begin
      return App.View'Unchecked_Access;
   end Get_View_Handler;

   --  ------------------------------
   --  Initialize the application
   --  ------------------------------
   procedure Initialize (App  : in out Application;
                         Conf : in Config) is
   begin
      App.Conf := Conf;
      App.View.Initialize (Conf);
      ASF.Modules.Initialize (App.Modules, Conf);
      ASF.Locales.Initialize (App.Locales, App.Factory, Conf);
   end Initialize;

   --  ------------------------------
   --  Get the configuration parameter;
   --  ------------------------------
   function Get_Config (App   : Application;
                        Param : Config_Param) return String is
   begin
      return App.Conf.Get (Param);
   end Get_Config;

   --  ------------------------------
   --  Set a global variable in the global EL contexts.
   --  ------------------------------
   procedure Set_Global (App     : in out Application;
                         Name    : in String;
                         Value   : in String) is
   begin
      App.Set_Global (Name, EL.Objects.To_Object (Value));
   end Set_Global;

   procedure Set_Global (App     : in out Application;
                         Name    : in String;
                         Value   : in EL.Objects.Object) is
   begin
      App.Globals.Bind (Name, Value);
   end Set_Global;

   --  ------------------------------
   --  Resolve a global variable and return its value.
   --  Raises the <b>EL.Functions.No_Variable</b> exception if the variable does not exist.
   --  ------------------------------
   function Get_Global (App : in Application;
                        Name : in Ada.Strings.Unbounded.Unbounded_String;
                        Context : in EL.Contexts.ELContext'Class)
                        return EL.Objects.Object is
      Value : constant EL.Expressions.ValueExpression := App.Globals.Get_Variable (Name);
   begin
      return Value.Get_Value (Context);
   end Get_Global;

   --  ------------------------------
   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
   --  ------------------------------
   procedure Register (App     : in out Application;
                       Name    : in String;
                       Handler : in Create_Bean_Access;
                       Free    : in Free_Bean_Access := null;
                       Scope   : in Scope_Type := REQUEST_SCOPE) is
   begin
      ASF.Beans.Register (App.Factory, Name, Handler, Free, Scope);
   end Register;

   --  ------------------------------
   --  Create a bean by using the create operation registered for the name
   --  ------------------------------
   procedure Create (App     : in Application;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Result  : out EL.Beans.Readonly_Bean_Access;
                     Free    : out Free_Bean_Access;
                     Scope   : out Scope_Type) is
   begin
      ASF.Beans.Create (App.Factory, Name, Result, Free, Scope);
   end Create;

   --  ------------------------------
   --  Register the module in the application
   --  ------------------------------
   procedure Register (App     : in out Application;
                       Module  : in ASF.Modules.Module_Access;
                       Name    : in String;
                       URI     : in String := "") is
   begin
      ASF.Modules.Register (App.Modules'Unchecked_Access, Module, Name, URI);
      Module.Register_Factory (App.Factory);

      App.View.Register_Module (Module);
   end Register;

   --  ------------------------------
   --  Register a bundle and bind it to a facelet variable.
   --  ------------------------------
   procedure Register (App    : in out Application;
                       Name   : in String;
                       Bundle : in String) is
   begin
      ASF.Locales.Register (App.Locales, App.Factory, Name, Bundle);
   end Register;

   --  ------------------------------
   --  Closes the application
   --  ------------------------------
   procedure Close (App : in out Application) is
   begin
      ASF.Applications.Views.Close (App.View);
   end Close;

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
      Free   : ASF.Beans.Free_Bean_Access := null;
      Scope  : Scope_Type;
   begin
      if not EL.Objects.Is_Null (Result) then
         return Result;
      end if;
      Resolver.Application.Create (Name, Bean, Free, Scope);
      if Bean = null then
         return Resolver.Application.Get_Global (Name, Context);
         --           raise No_Variable
         --             with "Bean not found: '" & To_String (Name) & "'";
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

   --  ------------------------------
   --  Dispatch the request received on a page.
   --  ------------------------------
   procedure Dispatch (App     : in out Application;
                       Page    : String;
                       Writer  : in ASF.Contexts.Writer.ResponseWriter_Access;
                       Request : in ASF.Requests.Request_Access) is

      use EL.Contexts.Default;
      use EL.Variables;
      use EL.Variables.Default;
      use EL.Contexts;
      use EL.Objects;
      use EL.Beans;
      use ASF.Applications.Views;
      use Ada.Exceptions;

      Context        : aliased ASF.Contexts.Faces.Faces_Context;
      View           : Components.Core.UIViewRoot;
      ELContext      : aliased EL.Contexts.Default.Default_Context;
      Variables      : aliased Default_Variable_Mapper;
      Req_Resolver   : aliased Default_ELResolver;
      Root_Resolver  : aliased Web_ELResolver;

      Beans          : aliased Bean_Vectors.Vector;
      --  Get the view handler
      Handler   : constant access View_Handler'Class := App.Get_View_Handler;
   begin
      Log.Info ("Dispatch {0}", Page);

      Root_Resolver.Application := App'Unchecked_Access;
      Root_Resolver.Request := Req_Resolver'Unchecked_Access;
      Root_Resolver.Beans := Beans'Unchecked_Access;
      ELContext.Set_Resolver (Root_Resolver'Unchecked_Access);
      ELContext.Set_Variable_Mapper (Variables'Unchecked_Access);

      Context.Set_ELContext (ELContext'Unchecked_Access);
      Context.Set_Response_Writer (Writer);
--        Writer.Initialize ("text/html", "UTF-8", 8192);

      Context.Set_Request (Request);
      Handler.Set_Context (Context'Unchecked_Access);
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
               if Bean.Bean /= null and then Bean.Free /= null then
                  Bean.Free (Bean.Bean);
               end if;
            end;
            Bean_Vectors.Next (C);
         end loop;
      end;
--        return AWS.Response.Build (Content_Type    => Writer.Get_Content_Type,
--                                   UString_Message => Writer.Get_Response);
   end Dispatch;

end ASF.Applications.Main;
