-----------------------------------------------------------------------
--  Writer Tests - Unit tests for ASF.Contexts.Writer
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2018, 2020, 2022 Stephane Carrez
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

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Unchecked_Deallocation;

with Util.Test_Caller;
package body ASF.Contexts.Writer.Tests is

   use Util.Tests;

   procedure Free_Writer is
     new Ada.Unchecked_Deallocation (Object => Test_Writer'Class,
                                     Name   => Test_Writer_Access);

   procedure Initialize (Stream       : in out Test_Writer;
                         Content_Type : in String;
                         Encoding     : in String;
                         Size         : in Natural) is
      Output : ASF.Streams.Print_Stream;
   begin
      Stream.Content.Initialize (Size => Size);
      Output.Initialize (Stream.Content'Unchecked_Access);
      Stream.Initialize (Content_Type, Encoding, Output);
   end Initialize;

   overriding
   procedure Write (Stream : in out Test_Writer;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
   begin
      for I in Buffer'Range loop
         Append (Stream.Response, Character'Val (Buffer (I)));
      end loop;
   end Write;

   overriding
   procedure Flush (Stream : in out Test_Writer) is
   begin
      Response_Writer (Stream).Flush;
      Stream.Content.Flush (Into => Stream.Response);
   end Flush;

   --  Set up performed before each test case
   overriding
   procedure Set_Up (T : in out Test) is
   begin
      T.Writer := new Test_Writer;
      --  use a small buffer to test the flush
      T.Writer.Initialize ("text/xml", "UTF-8", 1024);
   end Set_Up;

   --  Tear down performed after each test case
   overriding
   procedure Tear_Down (T : in out Test) is
   begin
      Free_Writer (T.Writer);
   end Tear_Down;

   --  Test the Start/Write/End_Element methods
   procedure Test_Write_Element (T : in out Test) is
   begin
      T.Writer.Start_Element ("p");
      T.Writer.Start_Element ("b");
      T.Writer.Write_Element ("i", "italic within a bold");
      T.Writer.End_Element ("b");
      T.Writer.End_Element ("p");
      --  T.Writer.Flush;

      Assert_Equals (T, "<p><b><i>italic within a bold</i></b></p>",
                     T.Writer.Response);

      T.Writer.Response := To_Unbounded_String ("");
      T.Writer.Start_Element ("div");
      T.Writer.Write_Attribute ("title", "A ""S&'%^&<>");
      T.Writer.Write_Attribute ("id", "23");
      T.Writer.End_Element ("div");
      --  T.Writer.Flush;

      Assert_Equals (T, "<div title=""A &quot;S&amp;'%^&amp;&lt;&gt;"" id=""23""></div>",
                     T.Writer.Response);
   end Test_Write_Element;

   --  Test the Write_Char/Text methods
   procedure Test_Write_Text (T : in out Test) is
      use Ada.Calendar;
      Start : Ada.Calendar.Time;
      D     : Duration;
   begin
      Start := Ada.Calendar.Clock;
      T.Writer.Start_Element ("p");
      T.Writer.Write_Char ('<');
      T.Writer.Write_Char ('>');
      T.Writer.Write_Char ('~');
      T.Writer.Start_Element ("i");
      T.Writer.Write_Text ("""A' <>&");
      T.Writer.End_Element ("i");
      T.Writer.End_Element ("p");
      --  T.Writer.Flush;
      D := Ada.Calendar.Clock - Start;
      Ada.Text_IO.Put_Line ("Write text: " & Duration'Image (D));

      Assert_Equals (T, "<p>&lt;&gt;~<i>""A' &lt;&gt;&amp;</i></p>",
                     T.Writer.Response);
   end Test_Write_Text;

   package Caller is new Util.Test_Caller (Test, "Contexts.Writer");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is

   begin
      --  To document what is tested, register the test methods for each
      --  operation that is tested.
      Caller.Add_Test (Suite, "Test ASF.Contexts.Writer.Start_Element",
                       Test_Write_Element'Access);
      Caller.Add_Test (Suite, "Test ASF.Contexts.Writer.End_Element",
                       Test_Write_Element'Access);

      Caller.Add_Test (Suite, "Test ASF.Contexts.Writer.Write_Element",
                       Test_Write_Element'Access);

      Caller.Add_Test (Suite, "Test ASF.Contexts.Writer.Write_Attribute",
                       Test_Write_Element'Access);

      Caller.Add_Test (Suite, "Test ASF.Contexts.Writer.Write_Text",
                       Test_Write_Text'Access);

      Caller.Add_Test (Suite, "Test ASF.Contexts.Writer.Write_Char",
                       Test_Write_Text'Access);
   end Add_Tests;

end ASF.Contexts.Writer.Tests;
