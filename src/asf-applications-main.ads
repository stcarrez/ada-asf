-----------------------------------------------------------------------
--  applications -- Ada Web Application
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with EL.Objects;
with EL.Contexts;
with EL.Functions;
with EL.Functions.Default;
with EL.Variables.Default;
with Ada.Strings.Unbounded;

with ASF.Locales;
with ASF.Factory;
with ASF.Converters;
with ASF.Validators;
with ASF.Contexts.Faces;
with ASF.Lifecycles;
with ASF.Applications.Views;
with ASF.Navigations;
with ASF.Beans;
with ASF.Requests;
with ASF.Responses;
with ASF.Servlets;
with ASF.Events.Actions;
package ASF.Applications.Main is

   use ASF.Beans;

   --  ------------------------------
   --  Factory for creation of lifecycle, view handler
   --  ------------------------------
   type Application_Factory is tagged limited private;

   --  Create the lifecycle handler.  The lifecycle handler is created during
   --  the initialization phase of the application.  The default implementation
   --  creates an <b>ASF.Lifecycles.Default.Default_Lifecycle</b> object.
   --  It can be overriden to change the behavior of the ASF request lifecycle.
   function Create_Lifecycle_Handler (App : in Application_Factory)
                                      return ASF.Lifecycles.Lifecycle_Access;

   --  Create the view handler.  The view handler is created during
   --  the initialization phase of the application.  The default implementation
   --  creates an <b>ASF.Applications.Views.View_Handler</b> object.
   --  It can be overriden to change the views associated with the application.
   function Create_View_Handler (App : in Application_Factory)
                                 return ASF.Applications.Views.View_Handler_Access;

   --  Create the navigation handler.  The navigation handler is created during
   --  the initialization phase of the application.  The default implementation
   --  creates an <b>ASF.Navigations.Navigation_Handler</b> object.
   --  It can be overriden to change the navigations associated with the application.
   function Create_Navigation_Handler (App : in Application_Factory)
                                       return ASF.Navigations.Navigation_Handler_Access;

   --  ------------------------------
   --  Application
   --  ------------------------------
   type Application is new ASF.Servlets.Servlet_Registry
     and ASF.Events.Actions.Action_Listener with private;
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

   --  Get the action event listener responsible for processing action
   --  events and triggering the navigation to the next view using the
   --  navigation handler.
   function Get_Action_Listener (App : in Application)
                                 return ASF.Events.Actions.Action_Listener_Access;

   --  Process the action associated with the action event.  The action returns
   --  and outcome which is then passed to the navigation handler to navigate to
   --  the next view.
   overriding
   procedure Process_Action (Listener : in Application;
                             Event    : in ASF.Events.Actions.Action_Event'Class;
                             Context  : in out Contexts.Faces.Faces_Context'Class);

   --  Initialize the application
   procedure Initialize (App     : in out Application;
                         Conf    : in Config;
                         Factory : in Application_Factory'Class);

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

   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
--     procedure Register (App     : in out Application;
--                         Name    : in String;
--                         Handler : in Create_Bean_Access;
--                         Scope   : in Scope_Type := REQUEST_SCOPE);

   --  Register a bundle and bind it to a facelet variable.
   procedure Register (App    : in out Application;
                       Name   : in String;
                       Bundle : in String);

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

   type Application is new ASF.Servlets.Servlet_Registry
     and ASF.Events.Actions.Action_Listener with record
      View    : aliased ASF.Applications.Views.View_Handler;
      Lifecycle : ASF.Lifecycles.Lifecycle_Access;
      Factory : ASF.Beans.Bean_Factory;
      Locales : ASF.Locales.Factory;
      Globals : aliased EL.Variables.Default.Default_Variable_Mapper;
      Conf    : Config;

      Functions : aliased EL.Functions.Default.Default_Function_Mapper;

      --  The component factory
      Components : aliased ASF.Factory.Component_Factory;

      --  The action listener.
      Action_Listener : ASF.Events.Actions.Action_Listener_Access;

      --  The navigation handler.
      Navigation      : ASF.Navigations.Navigation_Handler_Access := null;
   end record;

end ASF.Applications.Main;
