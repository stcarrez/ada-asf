-----------------------------------------------------------------------
--  Writer Tests - Unit tests for ASF.Contexts.Writer
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with Util.Streams.Texts;
package ASF.Contexts.Writer.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test_Writer is new Response_Writer with record
      Response : Unbounded_String;
      Content  : aliased Util.Streams.Texts.Print_Stream;
   end record;
   type Test_Writer_Access is access all Test_Writer'Class;

   overriding
   procedure Write (Stream : in out Test_Writer;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   overriding
   procedure Flush (Stream : in out Test_Writer);

   procedure Initialize (Stream       : in out Test_Writer;
                         Content_Type : in String;
                         Encoding     : in String;
                         Size         : in Natural);

   type Test is new Util.Tests.Test with record
      Writer : Test_Writer_Access;
   end record;

   overriding
   procedure Set_Up (T : in out Test);
   --  Set up performed before each test case

   overriding
   procedure Tear_Down (T : in out Test);
   --  Tear down performed after each test case

   procedure Test_Write_Element (T : in out Test);

   --  Test the Write_Char/Text methods
   procedure Test_Write_Text (T : in out Test);

end ASF.Contexts.Writer.Tests;
