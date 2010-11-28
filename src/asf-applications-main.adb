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

with ASF.Streams;
with ASF.Contexts.Writer;
with ASF.Components.Root;
with ASF.Components.Core;
with ASF.Components.Base;
with ASF.Components.Core.Factory;
with ASF.Components.Html.Factory;
with ASF.Components.Utils.Factory;
with ASF.Views.Nodes.Core;
with ASF.Views.Nodes.Facelets;

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
      use ASF.Components;
      use ASF.Views;
   begin
      App.Conf := Conf;
      App.Set_Init_Parameters (Params => Conf);

      ASF.Factory.Register (Factory  => App.Components,
                            Bindings => Core.Factory.Definition);
      ASF.Factory.Register (Factory  => App.Components,
                            Bindings => Html.Factory.Definition);
      ASF.Factory.Register (Factory  => App.Components,
                            Bindings => Nodes.Core.Definition);
      ASF.Factory.Register (Factory  => App.Components,
                            Bindings => Nodes.Facelets.Definition);

      ASF.Components.Utils.Factory.Set_Functions (App.Functions);
      ASF.Views.Nodes.Core.Set_Functions (App.Functions);

      App.View.Initialize (App.Components'Unchecked_Access, Conf);
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
      Value : constant EL.Expressions.Value_Expression := App.Globals.Get_Variable (Name);
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
   --  Add a converter in the application.  The converter is referenced by
   --  the specified name in the XHTML files.
   --  ------------------------------
   procedure Add_Converter (App       : in out Application;
                            Name      : in String;
                            Converter : access ASF.Converters.Converter'Class) is
   begin
      ASF.Factory.Register (Factory   => App.Components,
                            Name      => Name,
                            Converter => Converter.all'Unchecked_Access);
   end Add_Converter;

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
      Request     : ASF.Requests.Request_Access;
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

      Result : Object;
      Bean   : EL.Beans.Readonly_Bean_Access;
      Free   : ASF.Beans.Free_Bean_Access := null;
      Scope  : Scope_Type;
      Key    : constant String := To_String (Name);
   begin
      if Base /= null then
         return Base.Get_Value (Key);
      end if;

      Result := Resolver.Request.Get_Attribute (Key);
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
      Resolver.Request.Set_Attribute (Key, Result);
      return Result;
   end Get_Value;

   --  Set the value associated with a base object and a given property.
   overriding
   procedure Set_Value (Resolver : in Web_ELResolver;
                        Context  : in EL.Contexts.ELContext'Class;
                        Base     : access EL.Beans.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in EL.Objects.Object) is
      pragma Unreferenced (Context);

      Key : constant String := To_String (Name);
   begin
      if Base /= null then
         Base.Set_Value (Name => Key, Value => Value);
      else
         Resolver.Request.Set_Attribute (Name => Key, Value => Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Set the current faces context before processing a view.
   --  ------------------------------
   procedure Set_Context (App : in out Application;
                          Context : in ASF.Contexts.Faces.Faces_Context_Access) is
   begin
      Context.Get_ELContext.Set_Function_Mapper (App.Functions'Unchecked_Access);
      ASF.Contexts.Faces.Set_Current (Context, App'Unchecked_Access);
   end Set_Context;

   --  ------------------------------
   --  Dispatch the request received on a page.
   --  ------------------------------
   procedure Dispatch (App      : in out Application;
                       Page     : in String;
                       Request  : in out ASF.Requests.Request'Class;
                       Response : in out ASF.Responses.Response'Class) is

      use EL.Contexts.Default;
      use EL.Variables;
      use EL.Variables.Default;
      use EL.Contexts;
      use EL.Objects;
      use EL.Beans;
      use ASF.Applications.Views;
      use Ada.Exceptions;

      Writer         : aliased ASF.Contexts.Writer.ResponseWriter;
      Context        : aliased ASF.Contexts.Faces.Faces_Context;
      View           : Components.Root.UIViewRoot;
      ELContext      : aliased EL.Contexts.Default.Default_Context;
      Variables      : aliased Default_Variable_Mapper;
      Root_Resolver  : aliased Web_ELResolver;

      Beans          : aliased Bean_Vectors.Vector;
      --  Get the view handler
      Handler   : constant access View_Handler'Class := App.Get_View_Handler;

      Output         : constant ASF.Streams.Print_Stream := Response.Get_Output_Stream;
   begin
      Log.Info ("Dispatch {0}", Page);

      Root_Resolver.Application := App'Unchecked_Access;
      Root_Resolver.Request := Request'Unchecked_Access;
      Root_Resolver.Beans := Beans'Unchecked_Access;
      ELContext.Set_Resolver (Root_Resolver'Unchecked_Access);
      ELContext.Set_Variable_Mapper (Variables'Unchecked_Access);

      Context.Set_ELContext (ELContext'Unchecked_Access);
      Context.Set_Response_Writer (Writer'Unchecked_Access);
      Writer.Initialize ("text/html", "UTF-8", Output);

      Context.Set_Request (Request'Unchecked_Access);
      Context.Set_Response (Response'Unchecked_Access);
      App.Set_Context (Context'Unchecked_Access);
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
   end Dispatch;

   --  ------------------------------
   --  Dispatch the request received on a page.
   --  ------------------------------
   procedure Postback (App      : in out Application;
                       Page     : in String;
                       Request  : in out ASF.Requests.Request'Class;
                       Response : in out ASF.Responses.Response'Class) is

      use EL.Contexts.Default;
      use EL.Variables;
      use EL.Variables.Default;
      use EL.Contexts;
      use EL.Objects;
      use EL.Beans;
      use ASF.Applications.Views;
      use Ada.Exceptions;

      Writer         : aliased ASF.Contexts.Writer.ResponseWriter;
      Context        : aliased ASF.Contexts.Faces.Faces_Context;
      View           : Components.Root.UIViewRoot;
      ELContext      : aliased EL.Contexts.Default.Default_Context;
      Variables      : aliased Default_Variable_Mapper;
      Root_Resolver  : aliased Web_ELResolver;

      Beans          : aliased Bean_Vectors.Vector;
      --  Get the view handler
      Handler   : constant access View_Handler'Class := App.Get_View_Handler;

      Output         : constant ASF.Streams.Print_Stream := Response.Get_Output_Stream;

      Root : access ASF.Components.Base.UIComponent'Class;
   begin
      Log.Info ("Dispatch {0}", Page);

      Root_Resolver.Application := App'Unchecked_Access;
      Root_Resolver.Request := Request'Unchecked_Access;
      Root_Resolver.Beans := Beans'Unchecked_Access;
      ELContext.Set_Resolver (Root_Resolver'Unchecked_Access);
      ELContext.Set_Variable_Mapper (Variables'Unchecked_Access);

      Context.Set_ELContext (ELContext'Unchecked_Access);
      Context.Set_Response_Writer (Writer'Unchecked_Access);
      Writer.Initialize ("text/html", "UTF-8", Output);

      Context.Set_Request (Request'Unchecked_Access);
      Context.Set_Response (Response'Unchecked_Access);
      App.Set_Context (Context'Unchecked_Access);
      begin
         Handler.Restore_View (Page, Context, View);

      exception
         when E : others =>
            Log.Error ("Error when restoring view {0}: {1}: {2}", Page,
                       Exception_Name (E), Exception_Message (E));
            raise;
      end;

      Root := ASF.Components.Root.Get_Root (View);
      begin
         Root.Process_Decodes (Context);

      exception
         when E: others =>
            Log.Error ("Error when restoring view {0}: {1}: {2}", Page,
                       Exception_Name (E), Exception_Message (E));
            raise;
      end;

      begin
         Root.Process_Updates (Context);

      exception
         when E: others =>
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
   end Postback;

   --  ------------------------------
   --  Find the converter instance that was registered under the given name.
   --  Returns null if no such converter exist.
   --  ------------------------------
   function Find (App  : in Application;
                  Name : in EL.Objects.Object) return access ASF.Converters.Converter'Class is
   begin
      return ASF.Factory.Find (App.Components, Name);
   end Find;

   --  ------------------------------
   --  Register some functions
   --  ------------------------------
   procedure Register_Functions (App : in out Application'Class) is
   begin
      Set_Functions (App.Functions);
   end Register_Functions;

end ASF.Applications.Main;
