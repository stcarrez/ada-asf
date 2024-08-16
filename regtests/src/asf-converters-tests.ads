-----------------------------------------------------------------------
--  asf-converters-tests - Unit tests for ASF.Converters
--  Copyright (C) 2014, 2018, 2019 Stephane Carrez
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

with ASF.Contexts.Faces.Tests;
package ASF.Converters.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new ASF.Contexts.Faces.Tests.Test with null record;

   --  Test the date short converter.
   procedure Test_Date_Short_Converter (T : in out Test);

   --  Test the date medium converter.
   procedure Test_Date_Medium_Converter (T : in out Test);

   --  Test the date long converter.
   procedure Test_Date_Long_Converter (T : in out Test);

   --  Test the date full converter.
   procedure Test_Date_Full_Converter (T : in out Test);

   --  Test the time short converter.
   procedure Test_Time_Short_Converter (T : in out Test);

   --  Test the time short converter.
   procedure Test_Time_Medium_Converter (T : in out Test);

   --  Test the time long converter.
   procedure Test_Time_Long_Converter (T : in out Test);

   --  Test converter reporting conversion errors when converting a string back to a date.
   procedure Test_Date_Converter_Error (T : in out Test);

   --  Test number converter.
   procedure Test_Number_Converter (T : in out Test);

end ASF.Converters.Tests;
