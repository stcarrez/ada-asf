-----------------------------------------------------------------------
--  applications -- Ada Web Application
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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

with Util.Beans.Objects;
with Util.Log.Loggers;

with ASF.Streams;
with ASF.Sessions;
with ASF.Contexts.Writer;
with ASF.Contexts.Flash;
with ASF.Components.Core;
with ASF.Components.Core.Factory;
with ASF.Components.Html.Factory;
with ASF.Components.Utils.Factory;
with ASF.Components.Ajax.Factory;
with ASF.Components.Root;
with ASF.Components.Core.Views;
with ASF.Views.Nodes.Core;
with ASF.Views.Nodes.Facelets;
with ASF.Lifecycles.Default;
with ASF.Beans.Params;
with ASF.Beans.Headers;
with ASF.Beans.Flash;
with ASF.Beans.Globals;

with EL.Expressions;
with EL.Contexts.Default;
with EL.Functions.Namespaces;
with EL.Utils;

with Ada.Exceptions;
with Ada.Containers.Indefinite_Vectors;
with Ada.Unchecked_Deallocation;
package body ASF.Applications.Main is

   use Util.Log;
   use Ada.Strings.Unbounded;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Applications.Main");

   --  ------------------------------
   --  Factory for creation of lifecycle, view handler
   --  ------------------------------

   --  ------------------------------
   --  Create the lifecycle handler.  The lifecycle handler is created during
   --  the initialization phase of the application.  The default implementation
   --  creates an <b>ASF.Lifecycles.Default.Default_Lifecycle</b> object.
   --  It can be overriden to change the behavior of the ASF request lifecycle.
   --  ------------------------------
   function Create_Lifecycle_Handler (App : in Application_Factory)
                                      return ASF.Lifecycles.Lifecycle_Access is
      pragma Unreferenced (App);
   begin
      return new ASF.Lifecycles.Default.Lifecycle;
   end Create_Lifecycle_Handler;

   --  ------------------------------
   --  Create the view handler.  The view handler is created during
   --  the initialization phase of the application.  The default implementation
   --  creates an <b>ASF.Applications.Views.View_Handler</b> object.
   --  It can be overriden to change the views associated with the application.
   --  ------------------------------
   function Create_View_Handler (App : in Application_Factory)
                                 return ASF.Applications.Views.View_Handler_Access is
      pragma Unreferenced (App);
   begin
      return new ASF.Applications.Views.View_Handler;
   end Create_View_Handler;

   --  ------------------------------
   --  Create the navigation handler.  The navigation handler is created during
   --  the initialization phase of the application.  The default implementation
   --  creates an <b>ASF.Navigations.Navigation_Handler</b> object.
   --  It can be overriden to change the navigations associated with the application.
   --  ------------------------------
   function Create_Navigation_Handler (App : in Application_Factory)
                                       return ASF.Navigations.Navigation_Handler_Access is
      pragma Unreferenced (App);
   begin
      return new ASF.Navigations.Navigation_Handler;
   end Create_Navigation_Handler;

   --  ------------------------------
   --  Create the permission manager.  The permission manager is created during
   --  the initialization phase of the application.  The default implementation
   --  creates a <b>Security.Permissions.Permission_Manager</b> object.
   --  ------------------------------
   function Create_Permission_Manager (App : in Application_Factory)
                                       return Security.Permissions.Permission_Manager_Access is
      pragma Unreferenced (App);
   begin
      return new Security.Permissions.Permission_Manager;
   end Create_Permission_Manager;

   --  ------------------------------
   --  Create the exception handler.  The exception handler is created during
   --  the initialization phase of the application.  The default implementation
   --  creates a <b>ASF.Contexts.Exceptions.Exception_Handler</b> object.
   --  ------------------------------
   function Create_Exception_Handler (App : in Application_Factory)
                                      return ASF.Contexts.Exceptions.Exception_Handler_Access is
      pragma Unreferenced (App);
   begin
      return new ASF.Contexts.Exceptions.Exception_Handler;
   end Create_Exception_Handler;

   --  ------------------------------
   --  Get the application view handler.
   --  ------------------------------
   function Get_View_Handler (App : access Application)
                              return access Views.View_Handler'Class is
   begin
      return App.View'Unchecked_Access;
   end Get_View_Handler;

   --  ------------------------------
   --  Get the lifecycle handler.
   --  ------------------------------
   function Get_Lifecycle_Handler (App : in Application)
                                   return ASF.Lifecycles.Lifecycle_Access is
   begin
      return App.Lifecycle;
   end Get_Lifecycle_Handler;

   --  ------------------------------
   --  Get the navigation handler.
   --  ------------------------------
   function Get_Navigation_Handler (App : in Application)
                                    return ASF.Navigations.Navigation_Handler_Access is
   begin
      return App.Navigation;
   end Get_Navigation_Handler;

   --  ------------------------------
   --  Get the permission manager associated with this application.
   --  ------------------------------
   function Get_Permission_Manager (App : in Application)
                                    return Security.Permissions.Permission_Manager_Access is
   begin
      return App.Permissions;
   end Get_Permission_Manager;

   --  ------------------------------
   --  Get the action event listener responsible for processing action
   --  events and triggering the navigation to the next view using the
   --  navigation handler.
   --  ------------------------------
   function Get_Action_Listener (App : in Application)
                                 return ASF.Events.Faces.Actions.Action_Listener_Access is
   begin
      return App.Action_Listener;
   end Get_Action_Listener;

   --  ------------------------------
   --  Process the action associated with the action event.  The action returns
   --  and outcome which is then passed to the navigation handler to navigate to
   --  the next view.
   --  ------------------------------
   overriding
   procedure Process_Action (Listener : in Application;
                             Event    : in ASF.Events.Faces.Actions.Action_Event'Class;
                             Context  : in out Contexts.Faces.Faces_Context'Class) is
      Method  : constant EL.Expressions.Method_Expression := Event.Get_Method;
      Action  : constant String := Method.Get_Expression;
      Outcome : Unbounded_String;
   begin
      Log.Info ("Execute bean action {0}", Action);

      begin
         Events.Faces.Actions.Action_Method.Execute (Method  => Method,
                                                     Param   => Outcome,
                                                     Context => Context.Get_ELContext.all);

         Log.Info ("Action outcome is {0}", Outcome);

      exception
         when E : others =>
            Log.Error ("Error when invoking action {0}: {1}: {2}", Action,
                       Ada.Exceptions.Exception_Name (E),
                       Ada.Exceptions.Exception_Message (E));

            Context.Queue_Exception (E);
            Outcome := To_Unbounded_String ("failure");
      end;

      Listener.Navigation.Handle_Navigation (Action  => Action,
                                             Outcome => To_String (Outcome),
                                             Context => Context);
   end Process_Action;

   --  ------------------------------
   --  Initialize the application
   --  ------------------------------
   procedure Initialize (App     : in out Application;
                         Conf    : in Config;
                         Factory : in out Application_Factory'Class) is
      App_Access : constant Application_Access := App'Unchecked_Access;
   begin

      App.Action_Listener := App'Unchecked_Access;

      --  Create the lifecycle handler.
      App.Lifecycle := Factory.Create_Lifecycle_Handler;

      --  Create the navigation handler.
      App.Navigation := Factory.Create_Navigation_Handler;

      --  Create the permission manager.
      App.Permissions := Factory.Create_Permission_Manager;

      Application'Class (App).Initialize_Components;

      App.Conf := Conf;
      Application'Class (App).Initialize_Config (App.Conf);

      --  App.Conf := Conf;
      App.Set_Init_Parameters (Params => App.Conf);

      App.View.Initialize (App.Components'Unchecked_Access, App.Conf);
      ASF.Locales.Initialize (App.Locales, App.Factory, App.Conf);

      --  Initialize the lifecycle handler.
      App.Lifecycle.Initialize (App_Access);

      --  Initialize the navigation handler.
      App.Navigation.Initialize (App_Access);

      Application'Class (App).Initialize_Servlets;
      Application'Class (App).Initialize_Filters;
   end Initialize;

   --  ------------------------------
   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   --  ------------------------------
   procedure Initialize_Servlets (App : in out Application) is
   begin
      null;
   end Initialize_Servlets;

   --  ------------------------------
   --  Initialize the filters provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application filters.
   --  ------------------------------
   procedure Initialize_Filters (App : in out Application) is
   begin
      null;
   end Initialize_Filters;

   --  ------------------------------
   --  Initialize the ASF components provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the component factories used by the application.
   --  ------------------------------
   procedure Initialize_Components (App : in out Application) is
      use ASF.Components;
      use ASF.Views;
   begin
      ASF.Factory.Register (Factory  => App.Components,
                            Bindings => Core.Factory.Definition);
      ASF.Factory.Register (Factory  => App.Components,
                            Bindings => Html.Factory.Definition);
      ASF.Factory.Register (Factory  => App.Components,
                            Bindings => Nodes.Core.Definition);
      ASF.Factory.Register (Factory  => App.Components,
                            Bindings => Nodes.Facelets.Definition);
      ASF.Factory.Register (Factory  => App.Components,
                            Bindings => Utils.Factory.Definition);
      ASF.Factory.Register (Factory  => App.Components,
                            Bindings => Ajax.Factory.Definition);

      ASF.Components.Utils.Factory.Set_Functions (App.Functions);
      ASF.Views.Nodes.Core.Set_Functions (App.Functions);
      Security.Permissions.Set_Functions (App.Functions);
   end Initialize_Components;

   --  ------------------------------
   --  Initialize the application configuration properties.  Properties defined in <b>Conf</b>
   --  are expanded by using the EL expression resolver.
   --  ------------------------------
   procedure Initialize_Config (App  : in out Application;
                                Conf : in out Config) is

      NS_Mapper : aliased EL.Functions.Namespaces.NS_Function_Mapper;
      Context   : aliased EL.Contexts.Default.Default_Context;

   begin
      NS_Mapper.Set_Namespace (Prefix => "fn",
                               URI    => ASF.Views.Nodes.Core.FN_URI);
      NS_Mapper.Set_Function_Mapper (App.Functions'Unchecked_Access);
      Context.Set_Function_Mapper (NS_Mapper'Unchecked_Access);
      EL.Utils.Expand (Source => Conf,
                       Into   => Conf,
                       Context => Context);
   end Initialize_Config;

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
      Value : constant EL.Expressions.Expression := App.Globals.Get_Variable (Name);
   begin
      return Value.Get_Value (Context);
   end Get_Global;

   --  ------------------------------
   --  Get the list of supported locales for this application.
   --  ------------------------------
   function Get_Supported_Locales (App : in Application)
                                   return Util.Locales.Locale_Array is
      R : Util.Locales.Locale_Array (1 .. 1) := (others => App.Default_Locale);
   begin
      return R;
   end Get_Supported_Locales;

   --  ------------------------------
   --  Get the default locale defined by the application.
   --  ------------------------------
   function Get_Default_Locale (App : in Application) return Util.Locales.Locale is
   begin
      return App.Default_Locale;
   end Get_Default_Locale;

   --  ------------------------------
   --  Set the default locale defined by the application.
   --  ------------------------------
   procedure Set_Default_Locale (App    : in out Application;
                                 Locale : in Util.Locales.Locale) is
   begin
      App.Default_Locale := Locale;
   end Set_Default_Locale;

   --  ------------------------------
   --  Create a bean by using the create operation registered for the name
   --  ------------------------------
   procedure Create (App     : in Application;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Context : in EL.Contexts.ELContext'Class;
                     Result  : out Util.Beans.Basic.Readonly_Bean_Access;
                     Scope   : out Scope_Type) is
   begin
      ASF.Beans.Create (App.Factory, Name, Context, Result, Scope);
   end Create;

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
   --  Register the bean identified by <b>Name</b> and associated with the class <b>Class</b>.
   --  The class must have been registered by using the <b>Register</b> class operation.
   --  The scope defines the scope of the bean.
   --  ------------------------------
   procedure Register (App     : in out Application;
                       Name    : in String;
                       Class   : in String;
                       Params  : in Parameter_Bean_Ref.Ref;
                       Scope   : in Scope_Type := REQUEST_SCOPE) is
   begin
      ASF.Beans.Register (App.Factory, Name, Class, Params, Scope);
   end Register;

   --  ------------------------------
   --  Register under the name identified by <b>Name</b> the class instance <b>Class</b>.
   --  ------------------------------
   procedure Register_Class (App     : in out Application;
                             Name    : in String;
                             Class   : in ASF.Beans.Class_Binding_Access) is
   begin
      ASF.Beans.Register_Class (App.Factory, Name, Class);
   end Register_Class;

   --  ------------------------------
   --  Register under the name identified by <b>Name</b> a function to create a bean.
   --  This is a simplified class registration.
   --  ------------------------------
   procedure Register_Class (App     : in out Application;
                             Name    : in String;
                             Handler : in ASF.Beans.Create_Bean_Access) is
   begin
      ASF.Beans.Register_Class (App.Factory, Name, Handler);
   end Register_Class;

   --  ------------------------------
   --  Add a converter in the application.  The converter is referenced by
   --  the specified name in the XHTML files.
   --  ------------------------------
   procedure Add_Converter (App       : in out Application;
                            Name      : in String;
                            Converter : in ASF.Converters.Converter_Access) is
   begin
      ASF.Factory.Register (Factory   => App.Components,
                            Name      => Name,
                            Converter => Converter);
   end Add_Converter;

   --  ------------------------------
   --  Register a binding library in the factory.
   --  ------------------------------
   procedure Add_Components (App      : in out Application;
                             Bindings : in ASF.Factory.Factory_Bindings_Access) is
   begin
      ASF.Factory.Register (Factory  => App.Components,
                            Bindings => Bindings);
   end Add_Components;

   --  ------------------------------
   --  Closes the application
   --  ------------------------------
   procedure Close (App : in out Application) is
   begin
      ASF.Applications.Views.Close (App.View);
   end Close;

   type Bean_Object (Length : Natural) is record
      Bean : Util.Beans.Basic.Readonly_Bean_Access;
      Key  : String (1 .. Length);
   end record;

   package Bean_Vectors is new Ada.Containers.Indefinite_Vectors
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
                       Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                       Name     : Unbounded_String) return EL.Objects.Object;
   overriding
   procedure Set_Value (Resolver : in out Web_ELResolver;
                        Context  : in EL.Contexts.ELContext'Class;
                        Base     : access Util.Beans.Basic.Bean'Class;
                        Name     : in Unbounded_String;
                        Value    : in EL.Objects.Object);

   --  Get the value associated with a base object and a given property.
   overriding
   function Get_Value (Resolver : Web_ELResolver;
                       Context  : EL.Contexts.ELContext'Class;
                       Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                       Name     : Unbounded_String) return EL.Objects.Object is
      use EL.Objects;
      use Util.Beans.Basic;
      use EL.Variables;

      Result : Object;
      Bean   : Util.Beans.Basic.Readonly_Bean_Access;
      Scope  : Scope_Type;
      Key    : constant String := To_String (Name);
   begin
      if Base /= null then
         return Base.Get_Value (Key);
      end if;

      if Key = ASF.Beans.Params.PARAM_ATTRIBUTE_NAME then
         return ASF.Beans.Params.Instance;
      end if;

      if Key = ASF.Beans.Headers.HEADER_ATTRIBUTE_NAME then
         return ASF.Beans.Headers.Instance;
      end if;

      if Key = ASF.Beans.Flash.FLASH_ATTRIBUTE_NAME then
         return ASF.Beans.Flash.Instance;
      end if;

      if Key = ASF.Beans.Globals.INIT_PARAM_ATTRIBUTE_NAME then
         return ASF.Beans.Globals.Instance;
      end if;

      Result := Resolver.Request.Get_Attribute (Key);
      if not EL.Objects.Is_Null (Result) then
         return Result;
      end if;

      --  If there is a session, look if the attribute is defined there.
      declare
         Session : constant ASF.Sessions.Session := Resolver.Request.Get_Session;
      begin
         if Session.Is_Valid then
            Result := Session.Get_Attribute (Key);
            if not Util.Beans.Objects.Is_Null (Result) then
               return Result;
            end if;
         end if;
      end;
      Resolver.Application.Create (Name, Context, Bean, Scope);
      if Bean = null then
         return Resolver.Application.Get_Global (Name, Context);
         --           raise No_Variable
         --             with "Bean not found: '" & To_String (Name) & "'";
      end if;
      Resolver.Beans.Append (Bean_Object '(Key'Length, Bean, Key));
      Result := To_Object (Bean);
      Resolver.Request.Set_Attribute (Key, Result);
      return Result;
   end Get_Value;

   --  Set the value associated with a base object and a given property.
   overriding
   procedure Set_Value (Resolver : in out Web_ELResolver;
                        Context  : in EL.Contexts.ELContext'Class;
                        Base     : access Util.Beans.Basic.Bean'Class;
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
   --  Execute the lifecycle phases on the faces context.
   --  ------------------------------
   procedure Execute_Lifecycle (App     : in Application;
                                Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      App.Lifecycle.Execute (Context => Context);
      if not Context.Get_Response_Completed then
         App.Lifecycle.Render (Context);
      end if;
   end Execute_Lifecycle;

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
      use Util.Beans.Basic;
      use ASF.Applications.Views;
      use Ada.Exceptions;

      Writer         : aliased ASF.Contexts.Writer.Response_Writer;
      Flash          : aliased ASF.Contexts.Flash.Flash_Context;
      Context        : aliased ASF.Contexts.Faces.Faces_Context;
      ELContext      : aliased EL.Contexts.Default.Default_Context;
      Variables      : aliased Default_Variable_Mapper;
      Root_Resolver  : aliased Web_ELResolver;

      Beans          : aliased Bean_Vectors.Vector;

      Output         : constant ASF.Streams.Print_Stream := Response.Get_Output_Stream;

      Prev_Context   : constant Contexts.Faces.Faces_Context_Access := Contexts.Faces.Current;
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

      Context.Set_Ajax_Request (Request.Is_Ajax_Request);
      Context.Set_Request (Request'Unchecked_Access);
      Context.Set_Response (Response'Unchecked_Access);
      Context.Set_Flash (Flash'Unchecked_Access);
      App.Set_Context (Context'Unchecked_Access);

      begin
         Application'Class (App).Execute_Lifecycle (Context);

      exception
         when E : others =>
            Log.Error ("Error when restoring view {0}: {1}: {2}", Page,
                       Exception_Name (E), Exception_Message (E));
            Contexts.Faces.Restore (Prev_Context);
            raise;
      end;
      Contexts.Faces.Restore (Prev_Context);
      Writer.Flush;
--
--        declare
--           C : Bean_Vectors.Cursor := Beans.First;
--        begin
--           while Bean_Vectors.Has_Element (C) loop
--              declare
--                 Bean : constant Bean_Object := Bean_Vectors.Element (C);
--              begin
--                 Request.Remove_Attribute (Name => Bean.Key);
--              end;
--              Bean_Vectors.Next (C);
--           end loop;
--        end;
   end Dispatch;

   --  ------------------------------
   --  Dispatch a bean action request.
   --  1. Find the bean object identified by <b>Name</b>, create it if necessary.
   --  2. Resolve the bean method identified by <b>Operation</b>.
   --  3. If the method is an action method (see ASF.Events.Actions), call that method.
   --  4. Using the outcome action result, decide using the navigation handler what
   --     is the result view.
   --  5. Render the result view resolved by the navigation handler.
   --  ------------------------------
   procedure Dispatch (App       : in out Application;
                       Name      : in String;
                       Operation : in String;
                       Request   : in out ASF.Requests.Request'Class;
                       Response  : in out ASF.Responses.Response'Class;
                       Prepare   : access procedure (Bean : access Util.Beans.Basic.Bean'Class)) is

      use EL.Contexts.Default;
      use EL.Variables;
      use EL.Variables.Default;
      use EL.Contexts;
      use EL.Objects;
      use Util.Beans.Basic;
      use ASF.Applications.Views;
      use Ada.Exceptions;

      Writer         : aliased ASF.Contexts.Writer.Response_Writer;
      Flash          : aliased ASF.Contexts.Flash.Flash_Context;
      Context        : aliased ASF.Contexts.Faces.Faces_Context;
      ELContext      : aliased EL.Contexts.Default.Default_Context;
      Variables      : aliased Default_Variable_Mapper;
      Root_Resolver  : aliased Web_ELResolver;

      Beans          : aliased Bean_Vectors.Vector;

      Output         : constant ASF.Streams.Print_Stream := Response.Get_Output_Stream;

      Prev_Context   : constant Contexts.Faces.Faces_Context_Access := Contexts.Faces.Current;
   begin
      Log.Info ("Dispatch {0} - {1}", Name, Operation);

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
      Context.Set_Flash (Flash'Unchecked_Access);
      App.Set_Context (Context'Unchecked_Access);

      declare
         EL_Expr   : constant String := "#{" & Name & "." & Operation & "}";
         Expr      : EL.Expressions.Method_Expression;
         Method    : EL.Expressions.Method_Info;
         Outcome   : Ada.Strings.Unbounded.Unbounded_String;
      begin
         --  Build a method expression and get a Method_Info to obtain the bean
         --  instance and the method descriptor.
         Expr := EL.Expressions.Create_Expression (Expr    => EL_Expr,
                                                   Context => ELContext);

         Method := Expr.Get_Method_Info (Context => ELContext);

         --  If we have a prepare method and the bean provides a Set_Value method,
         --  call the preparation method to fill the bean with some values.
         if Prepare /= null and (Method.Object.all in Util.Beans.Basic.Bean'Class) then
            Prepare (Bean => Util.Beans.Basic.Bean'Class (Method.Object.all)'Access);
         end if;

         --  Execute the specified method on the bean and get the outcome result string.
         Outcome := To_Unbounded_String ("success");
         ASF.Events.Faces.Actions.Action_Method.Execute (Method => Method,
                                                         Param  => Outcome);

         --  If the response was not produced by the action method, use the navigation handler
         --  to decide what result view must be rendered for the response.
         if not Context.Get_Response_Completed then
            declare
               Root      : constant Components.Core.Views.UIView_Access
                 := new Components.Core.Views.UIView;
               View      : Components.Root.UIViewRoot;
               View_Name : constant String := "/ajax/" & Name & "/" & Operation;
            begin
               ASF.Components.Root.Set_Root (UI   => View,
                                             Root => Root,
                                             Name => View_Name);
               Context.Set_View_Root (View => View);
               App.Navigation.Handle_Navigation (Action  => Operation,
                                                 Outcome => To_String (Outcome),
                                                 Context => Context);

               --  If the navigation indicates a view to render, render it.
               if not Context.Get_Response_Completed then
                  View := Context.Get_View_Root;
                  if ASF.Components.Root.Get_View_Id (View) /= View_Name then
                     App.Lifecycle.Render (Context);
                  else
                     Response.Set_Status (ASF.Responses.SC_OK);
                  end if;
               end if;
            end;
         end if;

      exception
         when E : others =>
            Log.Error ("Error when executing action {0}: {1}: {2}",
                       EL_Expr,
                       Exception_Name (E), Exception_Message (E));
            Contexts.Faces.Restore (Prev_Context);
            raise;
      end;
      Contexts.Faces.Restore (Prev_Context);
      Writer.Flush;

      declare
         C : Bean_Vectors.Cursor := Beans.First;
      begin
         while Bean_Vectors.Has_Element (C) loop
            declare
               Bean : constant Bean_Object := Bean_Vectors.Element (C);
            begin
               Request.Remove_Attribute (Name => Bean.Key);
            end;
            Bean_Vectors.Next (C);
         end loop;
      end;
   end Dispatch;

   --  ------------------------------
   --  Find the converter instance that was registered under the given name.
   --  Returns null if no such converter exist.
   --  ------------------------------
   function Find (App  : in Application;
                  Name : in EL.Objects.Object) return ASF.Converters.Converter_Access is
   begin
      return ASF.Factory.Find (App.Components, Name);
   end Find;

   --  ------------------------------
   --  Find the validator instance that was registered under the given name.
   --  Returns null if no such validator exist.
   --  ------------------------------
   function Find_Validator (App  : in Application;
                            Name : in EL.Objects.Object)
                            return ASF.Validators.Validator_Access is
   begin
      return ASF.Factory.Find (App.Components, Name);
   end Find_Validator;

   --  ------------------------------
   --  Register some functions
   --  ------------------------------
   procedure Register_Functions (App : in out Application'Class) is
   begin
      Set_Functions (App.Functions);
   end Register_Functions;

   --  ------------------------------
   --  Register some bean definitions.
   --  ------------------------------
   procedure Register_Beans (App : in out Application'Class) is
   begin
      Set_Beans (App.Factory);
   end Register_Beans;

   --  ------------------------------
   --  Load the resource bundle identified by the <b>Name</b> and for the given
   --  <b>Locale</b>.
   --  ------------------------------
   procedure Load_Bundle (App    : in out Application;
                          Name   : in String;
                          Locale : in String;
                          Bundle : out ASF.Locales.Bundle) is
   begin
      ASF.Locales.Load_Bundle (App.Locales, Name, Locale, Bundle);
   end Load_Bundle;

   --  ------------------------------
   --  Finalizes the application, freeing the memory.
   --  ------------------------------
   overriding
   procedure Finalize (App : in out Application) is
      procedure Free is new Ada.Unchecked_Deallocation (ASF.Navigations.Navigation_Handler'Class,
                                                        ASF.Navigations.Navigation_Handler_Access);
      procedure Free is new Ada.Unchecked_Deallocation (ASF.Lifecycles.Lifecycle'Class,
                                                        ASF.Lifecycles.Lifecycle_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (Security.Permissions.Permission_Manager'Class,
                                        Security.Permissions.Permission_Manager_Access);
   begin
      Free (App.Navigation);
      Free (App.Lifecycle);
      Free (App.Permissions);
      ASF.Servlets.Servlet_Registry (App).Finalize;
   end Finalize;

end ASF.Applications.Main;
