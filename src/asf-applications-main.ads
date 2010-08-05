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

with EL.Beans;
with EL.Objects;
with EL.Contexts;
with EL.Variables.Default;
with Ada.Strings.Unbounded;

with ASF.Locales;
with ASF.Applications.Views;
with ASF.Beans;
with ASF.Modules;
with ASF.Requests;
with ASF.Contexts.Writer;
package ASF.Applications.Main is

   use ASF.Beans;

   type Application is tagged limited private;
   type Application_Access is access all Application'Class;

   --  Get the application view handler.
   function Get_View_Handler (App : access Application)
                              return access Views.View_Handler'Class;

   --  Initialize the application
   procedure Initialize (App  : in out Application;
                         Conf : in Config);

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
                       Free    : in Free_Bean_Access := null;
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
                     Result  : out EL.Beans.Readonly_Bean_Access;
                     Free    : out Free_Bean_Access;
                     Scope   : out Scope_Type);

   --  Closes the application
   procedure Close (App : in out Application);

   --  Dispatch the request received on a page.
   procedure Dispatch (App     : in out Application;
                       Page    : in String;
                       Writer  : in ASF.Contexts.Writer.ResponseWriter_Access;
                       Request : in ASF.Requests.Request_Access);

private

   type Application is tagged limited record
      View    : aliased ASF.Applications.Views.View_Handler;
      Factory : ASF.Beans.Bean_Factory;
      Locales : ASF.Locales.Factory;
      Modules : aliased ASF.Modules.Module_Registry;
      Globals : aliased EL.Variables.Default.Default_Variable_Mapper;
      Conf    : Config;
   end record;

end ASF.Applications.Main;
