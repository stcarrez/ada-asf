-----------------------------------------------------------------------
--  asf-modules-tests - Unit tests for Modules
--  Copyright (C) 2011 Stephane Carrez
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

with Util.Test_Caller;
with Util.Beans.Basic;
with Util.Beans.Objects;

with Ada.Unchecked_Deallocation;

with EL.Contexts.Default;

with ASF.Modules.Beans;
with ASF.Applications.Tests;
package body ASF.Modules.Tests is


--     use Ada.Strings.Fixed;
   use Util.Tests;

   package Module_Register is new ASF.Modules.Beans (Module, Module_Access);

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ASF.Modules.Register",
                       Test_Create_Module'Access);
      Caller.Add_Test (Suite, "Test ASF.Modules.Find_Module",
                       Test_Find_Module'Access);
      Caller.Add_Test (Suite, "Test ASF.Applications.Main.Register",
                       Test_Create_Application_Module'Access);
      Caller.Add_Test (Suite, "Test ASF.Applications.Main.Create",
                       Test_Create_Application_Module'Access);
      Caller.Add_Test (Suite, "Test ASF.Navigations.Mappers",
                       Test_Module_Navigation'Access);
   end Add_Tests;

   function Create_Form_Bean (Plugin : in Module_Access)
                              return Util.Beans.Basic.Readonly_Bean_Access is
      Result : ASF.Applications.Tests.Form_Bean_Access := new ASF.Applications.Tests.Form_Bean;
   begin
      return Result.all'Access;
   end Create_Form_Bean;

   --  ------------------------------
   --  Initialize the test application
   --  ------------------------------
   procedure Set_Up (T : in out Test) is
      Fact : ASF.Applications.Main.Application_Factory;
      C    : ASF.Applications.Config;
   begin
      Log.Info ("Creating application");
      T.App := new ASF.Applications.Main.Application;
      C.Copy (Util.Tests.Get_Properties);
      T.App.Initialize (C, Fact);
      T.App.Register ("layoutMsg", "layout");
   end Set_Up;

   --  ------------------------------
   --  Deletes the application object
   --  ------------------------------
   overriding
   procedure Tear_Down (T : in out Test) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => ASF.Applications.Main.Application'Class,
                                         Name   => ASF.Applications.Main.Application_Access);
   begin
      Log.Info ("Releasing application");
      Free (T.App);
   end Tear_Down;

   --  ------------------------------
   --  Test creation of module
   --  ------------------------------
   procedure Test_Create_Module (T : in out Test) is
      M : aliased Module;
      R : aliased Module_Registry;
   begin
      R.Config.Copy (Util.Tests.Get_Properties);
      M.App := T.App;
      Register (R'Unchecked_Access, M'Unchecked_Access, "empty", "uri");

      T.Assert_Equals ("empty", M.Get_Name, "Invalid module name");
      T.Assert_Equals ("uri", M.Get_URI, "Invalid module uri");

      T.Assert (Find_By_Name (R, "empty") = M'Unchecked_Access, "Find_By_Name failed");
      T.Assert (Find_By_URI (R, "uri") = M'Unchecked_Access, "Find_By_URI failed");
   end Test_Create_Module;

   --  ------------------------------
   --  Test looking for module
   --  ------------------------------
   procedure Test_Find_Module (T : in out Test) is
      M : aliased Module;
   begin
      M.App := T.App;
      T.App.Register (M'Unchecked_Access, "empty", "uri");

      T.Assert (M.Find_Module ("empty") /= null, "Find_Module should not return a null value");
      T.Assert (M.Find_Module ("toto") = null, "Find_Module should return null");
   end Test_Find_Module;

   --  ------------------------------
   --  Test creation of a module and registration in an application.
   --  ------------------------------
   procedure Test_Create_Application_Module (T : in out Test) is
      use ASF.Beans;
      use type Util.Beans.Basic.Readonly_Bean_Access;

      procedure Check (Name : in String;
                       Kind : in ASF.Beans.Scope_Type);

      procedure Check (Name : in String;
                       Kind : in ASF.Beans.Scope_Type) is
         Value : Util.Beans.Objects.Object;
         Bean  : Util.Beans.Basic.Readonly_Bean_Access;
         Scope : ASF.Beans.Scope_Type;
         Context : EL.Contexts.Default.Default_Context;
      begin
         T.App.Create (Name    => To_Unbounded_String (Name),
                       Context => Context,
                       Result  => Bean,
                       Scope   => Scope);
         T.Assert (Kind = Scope, "Invalid scope for " & Name);
         T.Assert (Bean /= null, "Invalid bean object");
         Value := Util.Beans.Objects.To_Object (Bean);
         T.Assert (not Util.Beans.Objects.Is_Null (Value), "Invalid bean");

         --  Special test for the sessionForm bean which is initialized by configuration properties
         if Name = "sessionForm" then
            T.Assert_Equals ("John.Rambo@gmail.com",
                             Util.Beans.Objects.To_String (Bean.Get_Value ("email")),
                             "Session form not initialized");
         end if;
      end Check;

      M : aliased Module;

   begin
      Module_Register.Register (M, "ASF.Applications.Tests.Form_Bean", Create_Form_Bean'Access);
      T.App.Register (M'Unchecked_Access, "test-module", "uri");

      T.Assert (M.Find_Module ("test-module") /= null,
                "Find_Module should not return a null value");
      T.Assert (M.Find_Module ("toto") = null, "Find_Module should return null");

      --  Check the 'regtests/config/test-module.xml' managed bean configuration.
      Check ("applicationForm", ASF.Beans.APPLICATION_SCOPE);
      Check ("sessionForm", ASF.Beans.SESSION_SCOPE);
      Check ("requestForm", ASF.Beans.REQUEST_SCOPE);
   end Test_Create_Application_Module;

   --  ------------------------------
   --  Test module and navigation rules
   --  ------------------------------
   procedure Test_Module_Navigation (T : in out Test) is
      use ASF.Beans;
      use type Util.Beans.Basic.Readonly_Bean_Access;

      M : aliased Module;
   begin
      Module_Register.Register (M, "ASF.Applications.Tests.Form_Bean", Create_Form_Bean'Access);
      T.App.Register (M'Unchecked_Access, "test-navigations", "uri");

      T.Assert (M.Find_Module ("test-navigations") /= null,
                "Find_Module should not return a null value");

   end Test_Module_Navigation;

end ASF.Modules.Tests;
