-----------------------------------------------------------------------
--  asf-models-selects-tests - Unit tests for UI select model
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package ASF.Models.Selects.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test creation of Select_Item
   procedure Test_Select_Item (T : in out Test);

   --  Test To_Object and To_Select_Item
   procedure Test_To_Object (T : in out Test);

   --  Test creation of Select_Item_List
   procedure Test_Select_Item_List (T : in out Test);

end ASF.Models.Selects.Tests;
