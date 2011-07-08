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

with Util.Tests;
with Util.Test_Caller;
with Util.Log.Loggers;
with Util.Measures;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
package body ASF.Modules.Tests is


   use Ada.Strings.Fixed;
   use Util.Tests;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ASF.Modules.Register",
                       Test_Create_Module'Access);
      Caller.Add_Test (Suite, "Test ASF.Modules.Find_Module",
                       Test_Find_Module'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of module
   --  ------------------------------
   procedure Test_Create_Module (T : in out Test) is
      M : aliased Module;
      R : aliased Module_Registry;
   begin
      Register (R'Unchecked_Access, M'Unchecked_Access, "test", "uri");

      T.Assert_Equals ("test", M.Get_Name, "Invalid module name");
      T.Assert_Equals ("uri", M.Get_URI, "Invalid module uri");

      T.Assert (Find_By_Name (R, "test") = M'Unchecked_Access, "Find_By_Name failed");
      T.Assert (Find_By_URI (R, "uri") = M'Unchecked_Access, "Find_By_URI failed");
   end Test_Create_Module;

   --  ------------------------------
   --  Test looking for module
   --  ------------------------------
   procedure Test_Find_Module (T : in out Test) is
      M : aliased Module;
      R : aliased Module_Registry;
   begin
      Register (R'Unchecked_Access, M'Unchecked_Access, "test", "uri");

      T.Assert (M.Find_Module ("test") /= null, "Find_Module should not return a null value");
      T.Assert (M.Find_Module ("toto") = null, "Find_Module should return null");
   end Test_Find_Module;

end ASF.Modules.Tests;
