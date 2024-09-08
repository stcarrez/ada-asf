-----------------------------------------------------------------------
--  asf-converters-tests - Unit tests for ASF.Converters
--  Copyright (C) 2014, 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
