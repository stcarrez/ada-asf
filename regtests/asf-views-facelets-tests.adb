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
with ASF.Contexts.Facelets;
with ASF.Applications.Main;
package body ASF.Views.Facelets.Tests is

   use ASF.Contexts.Facelets;

   type Facelet_Context is new ASF.Contexts.Facelets.Facelet_Context with null record;

   --  Get the application associated with this facelet context.
   overriding
   function Get_Application (Context : in Facelet_Context)
                             return access ASF.Applications.Main.Application'Class;


   --  Get the application associated with this facelet context.
   overriding
   function Get_Application (Context : in Facelet_Context)
                             return access ASF.Applications.Main.Application'Class is
      pragma Unreferenced (Context);
   begin
      return null;
   end Get_Application;

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
      Factory    : ASF.Views.Facelets.Facelet_Factory;
      Components : aliased ASF.Factory.Component_Factory;
      View       : ASF.Views.Facelets.Facelet;
      Ctx        : Facelet_Context;
   begin
      Initialize (Factory, Components'Unchecked_Access, "regtests/files/views;.", True, True, True);
      Find_Facelet (Factory, "text.xhtml", Ctx, View);

      T.Assert (Condition => not Is_Null (View),
                Message   => "Loading an existing facelet should return a view");
   end Test_Load_Facelet;

   --  Test loading of an unknown file
   procedure Test_Load_Unknown_Facelet (T : in out Test) is
      Factory    : ASF.Views.Facelets.Facelet_Factory;
      Components : aliased ASF.Factory.Component_Factory;
      View       : ASF.Views.Facelets.Facelet;
      Ctx        : Facelet_Context;
   begin
      Initialize (Factory, Components'Unchecked_Access, "regtests/files;.", True, True, True);
      Find_Facelet (Factory, "not-found-file.xhtml", Ctx, View);

      T.Assert (Condition => Is_Null (View),
                Message   => "Loading a missing facelet should not raise any exception");
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
