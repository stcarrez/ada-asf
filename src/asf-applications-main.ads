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
with ASF.Contexts.Faces;
with ASF.Lifecycles;
with ASF.Applications.Views;
with ASF.Navigations;
with ASF.Beans;
with ASF.Modules;
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
   procedure Register (App     : in out Application;
                       Name    : in String;
                       Handler : in Create_Bean_Access;
                       Scope   : in Scope_Type := REQUEST_SCOPE);

   --  Register the module in the application
   procedure Register (App     : in out Application;
                       Module  : in ASF.Modules.Module_Access;
                       Name    : in String;
                       URI     : in String := "");

   --  Register a bundle and bind it to a facelet variable.
   procedure Register (App    : in out Application;
                       Name   : in String;
                       Bundle : in String);

   --  Create a bean by using the create operation registered for the name
   procedure Create (App     : in Application;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Result  : out Util.Beans.Basic.Readonly_Bean_Access;
                     Scope   : out Scope_Type);

   --  Add a converter in the application.  The converter is referenced by
   --  the specified name in the XHTML files.
   procedure Add_Converter (App       : in out Application;
                            Name      : in String;
                            Converter : access ASF.Converters.Converter'Class);

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

   --  Find the converter instance that was registered under the given name.
   --  Returns null if no such converter exist.
   function Find (App  : in Application;
                  Name : in EL.Objects.Object) return access ASF.Converters.Converter'Class;

   --  Find the module with the given name
   function Find_Module (App  : in Application;
                         Name : in String) return ASF.Modules.Module_Access;

   --  Register some functions
   generic
      with procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class);
   procedure Register_Functions (App : in out Application'Class);

private

   type Application_Factory is tagged limited null record;

   type Application is new ASF.Servlets.Servlet_Registry
     and ASF.Events.Actions.Action_Listener with record
      View    : aliased ASF.Applications.Views.View_Handler;
      Lifecycle : ASF.Lifecycles.Lifecycle_Access;
      Factory : ASF.Beans.Bean_Factory;
      Locales : ASF.Locales.Factory;
      Modules : aliased ASF.Modules.Module_Registry;
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
