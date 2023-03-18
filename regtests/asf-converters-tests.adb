-----------------------------------------------------------------------
--  Faces Context Tests - Unit tests for ASF.Contexts.Faces
--  Copyright (C) 2010, 2011, 2012, 2013, 2015, 2018, 2019, 2023 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Util.Beans.Objects.Time;
with Util.Test_Caller;
with Util.Dates;
with ASF.Tests;
with ASF.Components.Html.Text;
with ASF.Converters.Dates;
with ASF.Converters.Numbers;
package body ASF.Converters.Tests is

   --  This file contains Wide_Wide_String constants.
   pragma Wide_Character_Encoding (UTF8);

   use Util.Tests;
   use ASF.Converters.Dates;

   package Caller is new Util.Test_Caller (Test, "Converters");

   procedure Test_Date_Conversion (T          : in out Test;
                                   Date_Style : in Dates.Style_Type;
                                   Time_Style : in Dates.Style_Type;
                                   Expect     : in String);

   procedure Test_Conversion_Error (T          : in out Test;
                                    Value      : in String);

   procedure Test_Number_Conversion (T       : in out Test;
                                     Picture : in String;
                                     Value   : in Float;
                                     Expect  : in Wide_Wide_String);

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
      Caller.Add_Test (Suite, "Test ASF.Converters.Dates.To_Object (Error)",
                       Test_Date_Converter_Error'Access);
      Caller.Add_Test (Suite, "Test ASF.Converters.Numbers.To_String (Float)",
                       Test_Number_Converter'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test getting an attribute from the faces context.
   --  ------------------------------
   procedure Test_Date_Conversion (T          : in out Test;
                                   Date_Style : in Dates.Style_Type;
                                   Time_Style : in Dates.Style_Type;
                                   Expect     : in String) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => ASF.Converters.Dates.Date_Converter'Class,
                                        Name   => ASF.Converters.Dates.Date_Converter_Access);

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
         V : Util.Beans.Objects.Object;
         S : Util.Dates.Date_Record;
      begin
         Util.Tests.Assert_Equals (T, Expect, R, "Invalid date conversion");

         V := C.To_Object (Ctx, UI, R);
         Util.Dates.Split (Into => S, Date => Util.Beans.Objects.Time.To_Time (V));

         if Date_Style /= Dates.DEFAULT then
            T.Assert (Util.Dates.Is_Same_Day (Util.Beans.Objects.Time.To_Time (V), D),
                      "Invalid date");
         else
            Util.Tests.Assert_Equals (T, 3, Natural (S.Hour),
                                      "Invalid date conversion: hour");
            Util.Tests.Assert_Equals (T, 4, Natural (S.Minute),
                                      "Invalid date conversion: minute");
            if Time_Style = Dates.LONG then
               Util.Tests.Assert_Equals (T, 5, Natural (S.Second),
                                         "Invalid date conversion: second");
            end if;
         end if;

      exception
         when others =>
            T.Fail ("Exception when converting date string: " & R);

      end;
      Free (C);
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

   --  ------------------------------
   --  Test getting an attribute from the faces context.
   --  ------------------------------
   procedure Test_Conversion_Error (T          : in out Test;
                                    Value      : in String) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => ASF.Converters.Dates.Date_Converter'Class,
                                        Name   => ASF.Converters.Dates.Date_Converter_Access);

      Ctx   : aliased ASF.Contexts.Faces.Faces_Context;
      UI    : ASF.Components.Html.Text.UIOutput;
      C     : ASF.Converters.Dates.Date_Converter_Access;
   begin
      T.Setup (Ctx);
      ASF.Contexts.Faces.Set_Current (Ctx'Unchecked_Access, ASF.Tests.Get_Application.all'Access);

      C := ASF.Converters.Dates.Create_Date_Converter (Date    => ASF.Converters.Dates.LONG,
                                                       Time    => ASF.Converters.Dates.LONG,
                                                       Format  => ASF.Converters.Dates.BOTH,
                                                       Locale  => "en",
                                                       Pattern => "");
      UI.Set_Converter (C.all'Access);
      declare
         V : Util.Beans.Objects.Object;
         pragma Unreferenced (V);
      begin
         V := C.To_Object (Ctx, UI, Value);
         T.Fail ("No exception raised for " & Value);

      exception
         when Invalid_Conversion =>
            null;

      end;
      Free (C);
   end Test_Conversion_Error;

   --  ------------------------------
   --  Test converter reporting conversion errors when converting a string back to a date.
   --  ------------------------------
   procedure Test_Date_Converter_Error (T : in out Test) is
   begin
      Test_Conversion_Error (T, "some invalid date");
   end Test_Date_Converter_Error;

   --  Test number converter.
   procedure Test_Number_Conversion (T       : in out Test;
                                     Picture : in String;
                                     Value   : in Float;
                                     Expect  : in Wide_Wide_String) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => ASF.Converters.Numbers.Number_Converter'Class,
                                        Name   => ASF.Converters.Numbers.Number_Converter_Access);

      Ctx   : aliased ASF.Contexts.Faces.Faces_Context;
      UI    : ASF.Components.Html.Text.UIOutput;
      C     : ASF.Converters.Numbers.Number_Converter_Access;
      D     : constant Util.Beans.Objects.Object := Util.Beans.Objects.To_Object (Value);
   begin
      T.Setup (Ctx);
      ASF.Contexts.Faces.Set_Current (Ctx'Unchecked_Access, ASF.Tests.Get_Application.all'Access);

      C := new ASF.Converters.Numbers.Number_Converter;
      UI.Set_Converter (C.all'Access);
      C.Set_Picture (Picture);
      declare
         use Ada.Strings.UTF_Encoding;

         R : constant String := C.To_String (Ctx, UI, D);
      begin
         Util.Tests.Assert_Equals (T, Wide_Wide_Strings.Encode (Expect), R,
                                   "Invalid number conversion with picture " & Picture);
      end;
      Free (C);

   end Test_Number_Conversion;

   --  ------------------------------
   --  Test converter reporting conversion errors when converting a string back to a date.
   --  ------------------------------
   procedure Test_Number_Converter (T : in out Test) is
   begin
      Test_Number_Conversion (T, "Z9.99", 12.345323, "12.35");
      Test_Number_Conversion (T, "Z9.99", 2.334323, " 2.33");
      Test_Number_Conversion (T, "<$Z_ZZ9.99>", 2.334323, " €    2.33 ");
      Test_Number_Conversion (T, "Z_ZZ9.99B$", 2.334323, "    2.33 €");
      Test_Number_Conversion (T, "Z_ZZ9.99B$", 2342.334323, "2,342.33 €");
      --  Test_Number_Conversion (T, "Z_ZZ9.99B$", 21342.334323, "2,342.33 €");
   end Test_Number_Converter;

end ASF.Converters.Tests;
