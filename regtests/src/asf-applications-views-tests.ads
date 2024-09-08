-----------------------------------------------------------------------
--  Render Tests - Unit tests for ASF.Applications.Views
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

with Ada.Strings.Unbounded;
package ASF.Applications.Views.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   --    type Test is new AUnit.Simple_Test_Cases.Test_Case with record

   type Test is new Util.Tests.Test_Case with record
      Writer : Integer;
      Name    : Ada.Strings.Unbounded.Unbounded_String;
      File    : Ada.Strings.Unbounded.Unbounded_String;
      Expect  : Ada.Strings.Unbounded.Unbounded_String;
      Result  : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Test_Case_Access is access all Test;

   --  Test case name
   overriding
   function Name (T : Test) return Util.Tests.Message_String;

   --  Perform the test.
   overriding
   procedure Run_Test (T : in out Test);

   overriding
   procedure Set_Up (T : in out Test);

   --  Test loading of facelet file
   procedure Test_Load_Facelet (T : in out Test);

end ASF.Applications.Views.Tests;
