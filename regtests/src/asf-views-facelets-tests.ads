-----------------------------------------------------------------------
--  Facelet Tests - Unit tests for ASF.Views.Facelets
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package ASF.Views.Facelets.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with record
      Writer    : Integer;
   end record;

   overriding
   procedure Set_Up (T : in out Test);
   --  Set up performed before each test case

   overriding
   procedure Tear_Down (T : in out Test);
   --  Tear down performed after each test case

   --  Test loading of facelet file
   procedure Test_Load_Facelet (T : in out Test);

   --  Test loading of an unknown file
   procedure Test_Load_Unknown_Facelet (T : in out Test);

end ASF.Views.Facelets.Tests;
