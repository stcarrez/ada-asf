-----------------------------------------------------------------------
--  asf-models-selects-tests - Unit tests for UI select model
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

package body ASF.Models.Selects.Tests is

   package Caller is new Util.Test_Caller (Test, "Models.Selects");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ASF.Models.Selects.Create_Select_Item",
                       Test_Select_Item'Access);
      Caller.Add_Test (Suite, "Test ASF.Models.Selects.Create_Select_Item_Wide",
                       Test_Select_Item'Access);
      Caller.Add_Test (Suite, "Test ASF.Models.Selects.Get_Label,Get_Value",
                       Test_Select_Item'Access);
      Caller.Add_Test (Suite, "Test ASF.Models.Selects.To_Object",
                       Test_To_Object'Access);
      Caller.Add_Test (Suite, "Test ASF.Models.Selects.To_Select_Item",
                       Test_To_Object'Access);
      Caller.Add_Test (Suite, "Test ASF.Models.Selects.Append,Length,Get_Select_Item",
                       Test_Select_Item_List'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test creation of Select_Item
   --  ------------------------------
   procedure Test_Select_Item (T : in out Test) is
      Val : Select_Item := Create_Select_Item ("red", "1");
   begin
      for I in 1 .. 2 loop
         T.Assert ("red" = Val.Get_Label, "Invalid label");
         T.Assert ("1" = Val.Get_Value, "Invalid value");
         T.Assert (not Val.Is_Disabled, "Not disabled");
         T.Assert (Val.Is_Escaped, "Invalid escape flag");
         Val := Create_Select_Item_Wide ("red", "1");
      end loop;

      Val := Create_Select_Item ("blue", "2", "Blue color");
      for I in 1 .. 2 loop
         T.Assert ("blue" = Val.Get_Label, "Invalid label");
         T.Assert ("2" = Val.Get_Value, "Invalid value");
         T.Assert ("Blue color" = Val.Get_Description, "Invalid description");
         T.Assert (not Val.Is_Disabled, "Not disabled");
         T.Assert (Val.Is_Escaped, "Invalid escape flag");
         Val := Create_Select_Item_Wide ("blue", "2", "Blue color");
      end loop;
   end Test_Select_Item;

   --  ------------------------------
   --  Test To_Object and To_Select_Item
   --  ------------------------------
   procedure Test_To_Object (T : in out Test) is
      Val : Select_Item := Create_Select_Item ("red", "1");
      Res : Select_Item;
      Obj : Util.Beans.Objects.Object := To_Object (Res);
   begin
      T.Assert (Util.Beans.Objects.Is_Null (Obj), "Object should be null");

      Obj := To_Object (Val);
      T.Assert (not Util.Beans.Objects.Is_Empty (Obj), "Object should not be empty");
      T.Assert (not Util.Beans.Objects.Is_Null (Obj), "Object should not be empty");

      Val := Create_Select_Item ("blue", "2");
      T.Assert ("blue" = Val.Get_Label, "Invalid label");

      Res := To_Select_Item (Obj);
      T.Assert ("red" = Res.Get_Label, "Invalid label");
      T.Assert ("1" = Res.Get_Value, "Invalid value");
      T.Assert (not Res.Is_Disabled, "Not disabled");
      T.Assert (Res.Is_Escaped, "Invalid escape flag");
   end Test_To_Object;

   --  ------------------------------
   --  Test creation of Select_Item_List
   --  ------------------------------
   procedure Test_Select_Item_List (T : in out Test) is
      List : Select_Item_List;
      Val  : Select_Item := Create_Select_Item ("red", "1");
   begin
      for I in 1 .. 10 loop
         Val := Create_Select_Item_Wide ("red", Integer'Wide_Wide_Image (I));
         List.Append (Val);
      end loop;

      Util.Tests.Assert_Equals (T, 10, List.Length, "Invalid select item list size");
      for I in 1 .. 10 loop
         Val := List.Get_Select_Item (I);

         T.Assert ("red" = Val.Get_Label, "Invalid label");
         T.Assert (Integer'Wide_Wide_Image (I) = Val.Get_Value, "Invalid value");
      end loop;
   end Test_Select_Item_List;

end ASF.Models.Selects.Tests;
