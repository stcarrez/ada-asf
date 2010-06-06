-----------------------------------------------------------------------
--  Facelet Tests - Unit tests for ASF.Views.Facelet
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

with AUnit.Test_Caller;
with AUnit.Assertions;
with Ada.Text_IO;
with ASF.Testsuite;
with AUnit.Assertions;
package body ASF.Views.Facelets.Tests is

   use AUnit.Assertions;
   use ASF.Testsuite;

   --  Set up performed before each test case
   overriding
   procedure Set_Up (T : in out Test) is
   begin
      null;
   end Set_Up;


   --  Tear down performed after each test case
   overriding
   procedure Tear_Down (T : in out Test) is
   begin
      null;
   end Tear_Down;

   --  Test loading of facelet file
   procedure Test_Load_Facelet (T : in out Test) is
      Factory : ASF.Views.Facelets.Facelet_Factory;
      View    : ASF.Views.Facelets.Facelet;
   begin
      Set_Search_Directory (Factory, "regtests/files;.");
      Find_Facelet (Factory, "test.xhtml", View);
   end Test_Load_Facelet;

   --  Test loading of an unknown file
   procedure Test_Load_Unknown_Facelet (T : in out Test) is
      Factory : ASF.Views.Facelets.Facelet_Factory;
      View    : ASF.Views.Facelets.Facelet;
   begin
      Set_Search_Directory (Factory, "regtests/files;.");
      Find_Facelet (Factory, "not-found-file.xhtml", View);

      Assert (Condition => False,
              Message   => "Exception not raised when loading a missing facelet");

   exception
      when others =>
         null;
   end Test_Load_Unknown_Facelet;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is

   begin
      --  To document what is tested, register the test methods for each
      --  operation that is tested.
      Suite.Add_Test (Caller.Create ("Test ASF.Views.Facelets.Find_Facelet",
        Test_Load_Facelet'Access));
      Suite.Add_Test (Caller.Create ("Test ASF.Views.Facelets.Find_Facelet",
        Test_Load_Unknown_Facelet'Access));
   end Add_Tests;

end ASF.Views.Facelets.Tests;
