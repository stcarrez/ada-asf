-----------------------------------------------------------------------
--  applications -- Ada Web Application
--  Copyright (C) 2009, 2010, 2011, 2012, 2017, 2018, 2023 Stephane Carrez
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

with Util.Beans.Basic;
with Util.Locales;

with EL.Objects;
with EL.Contexts;
with EL.Functions;
with EL.Functions.Default;
with EL.Expressions;
with EL.Variables.Default;
with Ada.Strings.Unbounded;

with ASF.Locales;
with ASF.Factory;
with ASF.Converters;
with ASF.Validators;
with ASF.Contexts.Faces;
with ASF.Contexts.Exceptions;
with ASF.Lifecycles;
with ASF.Applications.Views;
with ASF.Navigations;
with ASF.Beans;
with ASF.Requests;
with ASF.Responses;
with ASF.Servlets;
with ASF.Events.Faces.Actions;
with Security.Policies;  use Security;
with Security.OAuth.Servers;
private with Security.Random;

package ASF.Applications.Main is

   use ASF.Beans;

   --  ------------------------------
   --  Factory for creation of lifecycle, view handler
   --  ------------------------------
   type Application_Factory is tagged limited private;

   --  Create the lifecycle handler.  The lifecycle handler is created during
   --  the initialization phase of the application.  The default implementation
   --  creates an <b>ASF.Lifecycles.Default.Default_Lifecycle</b> object.
   --  It can be overridden to change the behavior of the ASF request lifecycle.
   function Create_Lifecycle_Handler (App : in Application_Factory)
                                      return ASF.Lifecycles.Lifecycle_Access;

   --  Create the view handler.  The view handler is created during
   --  the initialization phase of the application.  The default implementation
   --  creates an <b>ASF.Applications.Views.View_Handler</b> object.
   --  It can be overridden to change the views associated with the application.
   function Create_View_Handler (App : in Application_Factory)
                                 return ASF.Applications.Views.View_Handler_Access;

   --  Create the navigation handler.  The navigation handler is created during
   --  the initialization phase of the application.  The default implementation
   --  creates an <b>ASF.Navigations.Navigation_Handler</b> object.
   --  It can be overridden to change the navigations associated with the application.
   function Create_Navigation_Handler (App : in Application_Factory)
                                       return ASF.Navigations.Navigation_Handler_Access;

   --  Create the security policy manager.  The security policy manager is created during
   --  the initialization phase of the application.  The default implementation
   --  creates a <b>Security.Policies.Policy_Manager</b> object.
   function Create_Security_Manager (App : in Application_Factory)
                                       return Security.Policies.Policy_Manager_Access;

   --  Create the OAuth application manager.  The OAuth application manager is created
   --  during the initialization phase of the application.  The default implementation
   --  creates a <b>Security.OAuth.Servers.Auth_Manager</b> object.
   function Create_OAuth_Manager (App : in Application_Factory)
                                  return Security.OAuth.Servers.Auth_Manager_Access;

   --  Create the exception handler.  The exception handler is created during
   --  the initialization phase of the application.  The default implementation
   --  creates a <b>ASF.Contexts.Exceptions.Exception_Handler</b> object.
   function Create_Exception_Handler (App : in Application_Factory)
                                      return ASF.Contexts.Exceptions.Exception_Handler_Access;

   --  ------------------------------
   --  Application
   --  ------------------------------
   type Application is new ASF.Servlets.Servlet_Registry
     and ASF.Events.Faces.Actions.Action_Listener with private;
   type Application_Access is access all Application'Class;

   --  Get the application view handler.
   function Get_View_Handler (App : access Application)
                              return access Views.View_Handler'Class;

   --  Get the lifecycle handler.
   function Get_Lifecycle_Handler (App : in Application)
                                   return ASF.Lifecycles.Lifecycle_Access;

   --  Get the navigation handler.
   function Get_Navigation_Handler (App : in Application)
                                    return ASF.Navigations.Navigation_Handler_Access;

   --  Get the permission manager associated with this application.
   function Get_Security_Manager (App : in Application)
                                    return Security.Policies.Policy_Manager_Access;

   --  Get the OAuth application manager associated with this application.
   function Get_OAuth_Manager (App : in Application)
                               return Security.OAuth.Servers.Auth_Manager_Access;

   --  Get the action event listener responsible for processing action
   --  events and triggering the navigation to the next view using the
   --  navigation handler.
   function Get_Action_Listener (App : in Application)
                                 return ASF.Events.Faces.Actions.Action_Listener_Access;

   --  Get the exception handler configured for this application.
   function Get_Exception_Handler (App : in Application)
                               return ASF.Contexts.Exceptions.Exception_Handler_Access;

   --  Process the action associated with the action event.  The action returns
   --  and outcome which is then passed to the navigation handler to navigate to
   --  the next view.
   overriding
   procedure Process_Action (Listener : in Application;
                             Event    : in ASF.Events.Faces.Actions.Action_Event'Class;
                             Context  : in out Contexts.Faces.Faces_Context'Class);

   --  Execute the action method.  The action returns and outcome which is then passed
   --  to the navigation handler to navigate to the next view.
   procedure Process_Action (Listener : in Application;
                             Method   : in EL.Expressions.Method_Info;
                             Context  : in out Contexts.Faces.Faces_Context'Class);

   --  Initialize the application
   procedure Initialize (App     : in out Application;
                         Conf    : in Config;
                         Factory : in out Application_Factory'Class);

   --  Initialize the ASF components provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the component factories used by the application.
   procedure Initialize_Components (App : in out Application);

   --  Initialize the application configuration properties.  Properties defined in <b>Conf</b>
   --  are expanded by using the EL expression resolver.
   procedure Initialize_Config (App  : in out Application;
                                Conf : in out Config);

   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   procedure Initialize_Servlets (App : in out Application);

   --  Initialize the filters provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application filters.
   procedure Initialize_Filters (App : in out Application);

   --  Finalizes the application, freeing the memory.
   overriding
   procedure Finalize (App : in out Application);

   --  Get the configuration parameter;
   function Get_Config (App   : Application;
                        Param : Config_Param) return String;

   --  Set a global variable in the global EL contexts.
   procedure Set_Global (App     : in out Application;
                         Name    : in String;
                         Value   : in String);

   procedure Set_Global (App     : in out Application;
                         Name    : in String;
                         Value   : in EL.Objects.Object);

   --  Resolve a global variable and return its value.
   --  Raises the <b>EL.Functions.No_Variable</b> exception if the variable does not exist.
   function Get_Global (App : in Application;
                        Name : in Ada.Strings.Unbounded.Unbounded_String;
                        Context : in EL.Contexts.ELContext'Class)
                        return EL.Objects.Object;

   --  Get the list of supported locales for this application.
   function Get_Supported_Locales (App : in Application)
                                   return Util.Locales.Locale_Array;

   --  Add the locale to the list of supported locales.
   procedure Add_Supported_Locale (App    : in out Application;
                                   Locale : in Util.Locales.Locale);

   --  Get the default locale defined by the application.
   function Get_Default_Locale (App : in Application) return Util.Locales.Locale;

   --  Set the default locale defined by the application.
   procedure Set_Default_Locale (App    : in out Application;
                                 Locale : in Util.Locales.Locale);

   --  Compute the locale that must be used according to the <b>Accept-Language</b> request
   --  header and the application supported locales.
   function Calculate_Locale (Handler : in Application;
                              Context : in ASF.Contexts.Faces.Faces_Context'Class)
                              return Util.Locales.Locale;

   --  Register a bundle and bind it to a facelet variable.
   procedure Register (App    : in out Application;
                       Name   : in String;
                       Bundle : in String);

   --  Register the bean identified by <b>Name</b> and associated with the class <b>Class</b>.
   --  The class must have been registered by using the <b>Register</b> class operation.
   --  The scope defines the scope of the bean.
   procedure Register (App     : in out Application;
                       Name    : in String;
                       Class   : in String;
                       Params  : in Parameter_Bean_Ref.Ref;
                       Scope   : in Scope_Type := REQUEST_SCOPE);

   --  Register under the name identified by <b>Name</b> the class instance <b>Class</b>.
   procedure Register_Class (App     : in out Application;
                             Name    : in String;
                             Class   : in ASF.Beans.Class_Binding_Access);

   --  Register under the name identified by <b>Name</b> a function to create a bean.
   --  This is a simplified class registration.
   procedure Register_Class (App     : in out Application;
                             Name    : in String;
                             Handler : in ASF.Beans.Create_Bean_Access);

   --  Create a bean by using the create operation registered for the name
   procedure Create (App     : in Application;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Context : in EL.Contexts.ELContext'Class;
                     Result  : out Util.Beans.Basic.Readonly_Bean_Access;
                     Scope   : out Scope_Type);

   --  Add a converter in the application.  The converter is referenced by
   --  the specified name in the XHTML files.
   procedure Add_Converter (App       : in out Application;
                            Name      : in String;
                            Converter : in ASF.Converters.Converter_Access);

   --  Register a binding library in the factory.
   procedure Add_Components (App      : in out Application;
                             Bindings : in ASF.Factory.Factory_Bindings_Access);

   --  Verify the token validity associated with the `Data`.
   --  Returns True if the token is valid and false if it has expired or is invalid.
   function Verify_Token (App   : in Application;
                          Data  : in String;
                          Token : in String) return Boolean;

   --  Create a token for the data and the expiration time.
   --  The token has an expiration deadline and is signed by the application.
   --  The `Data` remains private and is never part of the returned token.
   function Create_Token (App    : in Application;
                          Data   : in String;
                          Expire : in Duration) return String;

   --  Closes the application
   procedure Close (App : in out Application);

   --  Set the current faces context before processing a view.
   procedure Set_Context (App     : in out Application;
                          Context : in ASF.Contexts.Faces.Faces_Context_Access);

   --  Execute the lifecycle phases on the faces context.
   procedure Execute_Lifecycle (App     : in Application;
                                Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Dispatch the request received on a page.
   procedure Dispatch (App      : in out Application;
                       Page     : in String;
                       Request  : in out ASF.Requests.Request'Class;
                       Response : in out ASF.Responses.Response'Class);

   --  Dispatch a bean action request.
   --  1. Find the bean object identified by <b>Name</b>, create it if necessary.
   --  2. Resolve the bean method identified by <b>Operation</b>.
   --  3. If the method is an action method (see ASF.Events.Actions), call that method.
   --  4. Using the outcome action result, decide using the navigation handler what
   --     is the result view.
   --  5. Render the result view resolved by the navigation handler.
   procedure Dispatch (App       : in out Application;
                       Name      : in String;
                       Operation : in String;
                       Request   : in out ASF.Requests.Request'Class;
                       Response  : in out ASF.Responses.Response'Class;
                       Prepare   : access procedure (Bean : access Util.Beans.Basic.Bean'Class));

   --  Find the converter instance that was registered under the given name.
   --  Returns null if no such converter exist.
   function Find (App  : in Application;
                  Name : in EL.Objects.Object) return ASF.Converters.Converter_Access;

   --  Find the validator instance that was registered under the given name.
   --  Returns null if no such validator exist.
   function Find_Validator (App  : in Application;
                            Name : in EL.Objects.Object)
                            return ASF.Validators.Validator_Access;

   --  Register some functions
   generic
      with procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class);
   procedure Register_Functions (App : in out Application'Class);

   --  Register some bean definitions.
   generic
      with procedure Set_Beans (Factory : in out ASF.Beans.Bean_Factory);
   procedure Register_Beans (App : in out Application'Class);

   --  Load the resource bundle identified by the <b>Name</b> and for the given
   --  <b>Locale</b>.
   procedure Load_Bundle (App    : in out Application;
                          Name   : in String;
                          Locale : in String;
                          Bundle : out ASF.Locales.Bundle);

private

   type Application_Factory is tagged limited null record;

   TOKEN_KEY_LENGTH : constant := 32;

   type Application is new ASF.Servlets.Servlet_Registry
     and ASF.Events.Faces.Actions.Action_Listener with record
      View    : aliased ASF.Applications.Views.View_Handler;
      Lifecycle : ASF.Lifecycles.Lifecycle_Access;
      Factory : aliased ASF.Beans.Bean_Factory;
      Locales : ASF.Locales.Factory;
      Globals : aliased EL.Variables.Default.Default_Variable_Mapper;

      Functions : aliased EL.Functions.Default.Default_Function_Mapper;

      --  The component factory
      Components : aliased ASF.Factory.Component_Factory;

      --  The action listener.
      Action_Listener : ASF.Events.Faces.Actions.Action_Listener_Access;

      --  The navigation handler.
      Navigation      : ASF.Navigations.Navigation_Handler_Access;

      --  The permission manager.
      Permissions     : Security.Policies.Policy_Manager_Access;

      --  The OAuth application manager.
      OAuth           : Security.OAuth.Servers.Auth_Manager_Access;

      --  Exception handler
      Exceptions      : ASF.Contexts.Exceptions.Exception_Handler_Access;

      --  Token CSRF generation support.
      Random          : Security.Random.Generator;
      Token_Key       : String (1 .. TOKEN_KEY_LENGTH);
   end record;

end ASF.Applications.Main;
