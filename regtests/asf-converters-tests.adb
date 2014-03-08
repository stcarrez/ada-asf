-----------------------------------------------------------------------
--  Faces Context Tests - Unit tests for ASF.Contexts.Faces
--  Copyright (C) 2010, 2011, 2012, 2013 Stephane Carrez
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
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Util.Beans.Objects.Time;
with Util.Test_Caller;
with ASF.Tests;
with ASF.Components.Html.Text;
with ASF.Converters.Dates;
package body ASF.Converters.Tests is

   use Util.Tests;
   use ASF.Converters.Dates;

   package Caller is new Util.Test_Caller (Test, "Converters");

   procedure Test_Date_Conversion (T          : in out Test;
                                   Date_Style : in Dates.Style_Type;
                                   Time_Style : in Dates.Style_Type;
                                   Expect     : in String);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is

   begin
      --  To document what is tested, register the test methods for each
      --  operation that is tested.
      Caller.Add_Test (Suite, "Test ASF.Converters.Dates.To_String (Date, Short)",
                       Test_Date_Short_Converter'Access);
      Caller.Add_Test (Suite, "Test ASF.Converters.Dates.To_String (Date, Medium)",
                       Test_Date_Medium_Converter'Access);
      Caller.Add_Test (Suite, "Test ASF.Converters.Dates.To_String (Date, Long)",
                       Test_Date_Long_Converter'Access);
      Caller.Add_Test (Suite, "Test ASF.Converters.Dates.To_String (Date, Full)",
                       Test_Date_Full_Converter'Access);

      Caller.Add_Test (Suite, "Test ASF.Converters.Dates.To_String (Time, Short)",
                       Test_Time_Short_Converter'Access);
      Caller.Add_Test (Suite, "Test ASF.Converters.Dates.To_String (Time, Medium)",
                       Test_Time_Medium_Converter'Access);
      Caller.Add_Test (Suite, "Test ASF.Converters.Dates.To_String (Time, Long)",
                       Test_Time_Long_Converter'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test getting an attribute from the faces context.
   --  ------------------------------
   procedure Test_Date_Conversion (T          : in out Test;
                                   Date_Style : in Dates.Style_Type;
                                   Time_Style : in Dates.Style_Type;
                                   Expect     : in String) is
      Ctx   : aliased ASF.Contexts.Faces.Faces_Context;
      UI    : ASF.Components.Html.Text.UIOutput;
      C     : ASF.Converters.Dates.Date_Converter_Access;
      D     : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of (2011, 11, 19,
                                                                             3, 4, 5);
   begin
      T.Setup (Ctx);
      ASF.Contexts.Faces.Set_Current (Ctx'Unchecked_Access, ASF.Tests.Get_Application.all'Access);

      if Date_Style = Dates.DEFAULT then
         C := ASF.Converters.Dates.Create_Date_Converter (Date    => Date_Style,
                                                          Time    => Time_Style,
                                                          Format  => ASF.Converters.Dates.TIME,
                                                          Locale  => "en",
                                                          Pattern => "");
      elsif Time_Style = Dates.DEFAULT then
         C := ASF.Converters.Dates.Create_Date_Converter (Date    => Date_Style,
                                                          Time    => Time_Style,
                                                          Format  => ASF.Converters.Dates.DATE,
                                                          Locale  => "en",
                                                          Pattern => "");
      else
         C := ASF.Converters.Dates.Create_Date_Converter (Date    => Date_Style,
                                                          Time    => Time_Style,
                                                          Format  => ASF.Converters.Dates.BOTH,
                                                          Locale  => "en",
                                                          Pattern => "");
      end if;
      UI.Set_Converter (C.all'Access);
      declare
         R : constant String := C.To_String (Ctx, UI, Util.Beans.Objects.Time.To_Object (D));
      begin
         Util.Tests.Assert_Equals (T, Expect, R, "Invalid date conversion");
      end;
   end Test_Date_Conversion;

   --  ------------------------------
   --  Test the date short converter.
   --  ------------------------------
   procedure Test_Date_Short_Converter (T : in out Test) is
   begin
      Test_Date_Conversion (T, ASF.Converters.Dates.SHORT, ASF.Converters.Dates.DEFAULT,
                            "19/11/2011");
   end Test_Date_Short_Converter;

   --  ------------------------------
   --  Test the date medium converter.
   --  ------------------------------
   procedure Test_Date_Medium_Converter (T : in out Test) is
   begin
      Test_Date_Conversion (T, ASF.Converters.Dates.MEDIUM, ASF.Converters.Dates.DEFAULT,
                            "Nov 19, 2011");
   end Test_Date_Medium_Converter;

   --  ------------------------------
   --  Test the date long converter.
   --  ------------------------------
   procedure Test_Date_Long_Converter (T : in out Test) is
   begin
      Test_Date_Conversion (T, ASF.Converters.Dates.LONG, ASF.Converters.Dates.DEFAULT,
                            "November 19, 2011");
   end Test_Date_Long_Converter;

   --  ------------------------------
   --  Test the date full converter.
   --  ------------------------------
   procedure Test_Date_Full_Converter (T : in out Test) is
   begin
      Test_Date_Conversion (T, ASF.Converters.Dates.FULL, ASF.Converters.Dates.DEFAULT,
                            "Saturday, November 19, 2011");
   end Test_Date_Full_Converter;

   --  ------------------------------
   --  Test the time short converter.
   --  ------------------------------
   procedure Test_Time_Short_Converter (T : in out Test) is
   begin
      Test_Date_Conversion (T, ASF.Converters.Dates.DEFAULT, ASF.Converters.Dates.SHORT,
                            "03:04");
   end Test_Time_Short_Converter;

   --  ------------------------------
   --  Test the time short converter.
   --  ------------------------------
   procedure Test_Time_Medium_Converter (T : in out Test) is
   begin
      Test_Date_Conversion (T, ASF.Converters.Dates.DEFAULT, ASF.Converters.Dates.MEDIUM,
                            "03:04");
   end Test_Time_Medium_Converter;

   --  ------------------------------
   --  Test the time long converter.
   --  ------------------------------
   procedure Test_Time_Long_Converter (T : in out Test) is
   begin
      Test_Date_Conversion (T, ASF.Converters.Dates.DEFAULT, ASF.Converters.Dates.LONG,
                            "03:04:05");
   end Test_Time_Long_Converter;

end ASF.Converters.Tests;
