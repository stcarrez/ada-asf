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
