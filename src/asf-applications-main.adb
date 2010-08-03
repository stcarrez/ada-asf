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
with EL.Expressions;
package body ASF.Applications.Main is

   --  ------------------------------
   --  Get the application view handler.
   --  ------------------------------
   function Get_View_Handler (App : access Application)
                              return access Views.View_Handler'Class is
   begin
      return App.View'Access;
   end Get_View_Handler;

   --  ------------------------------
   --  Initialize the application
   --  ------------------------------
   procedure Initialize (App  : in out Application;
                         Conf : in Config) is
   begin
      App.View.Initialize (Conf);
      ASF.Modules.Initialize (App.Modules, Conf);
      ASF.Locales.Initialize (App.Locales, App.Factory, Conf);
   end Initialize;

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

   --  Resolve a global variable and return its value.
   --  Raises the <b>EL.Functions.No_Variable</b> exception if the variable does not exist.
   function Get_Global (App : in Application;
                        Name : in Ada.Strings.Unbounded.Unbounded_String;
                        Context : in EL.Contexts.ELContext'Class)
                        return EL.Objects.Object is
      Value : EL.Expressions.ValueExpression := App.Globals.Get_Variable (Name);
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

end ASF.Applications.Main;
