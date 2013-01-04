-----------------------------------------------------------------------
--  ASF.Streams -- Print streams for servlets
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

package body ASF.Streams is

   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Util.Streams.Texts.Print_Stream_Access) is
   begin
      Stream.Target := To;
   end Initialize;

   --  ------------------------------
   --  Initialize the stream
   --  ------------------------------
   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Print_Stream'Class) is
   begin
      Stream.Target := To.Target;
   end Initialize;

   --  ------------------------------
   --  Write an integer on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Integer) is
   begin
      Stream.Target.Write (Item);
   end Write;

   --  ------------------------------
   --  Write a string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Stream.Target.Write (Item);
   end Write;

   --  ------------------------------
   --  Write a string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) is
   begin
      Stream.Target.Write (Item);
   end Write;

   --  ------------------------------
   --  Write a raw character on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Char   : in Character) is
   begin
      Stream.Target.Write (Char);
   end Write;

   --  ------------------------------
   --  Write a raw string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in String) is
   begin
      Stream.Target.Write (Item);
   end Write;

   --  ------------------------------
   --  Write a raw string on the stream.
   --  ------------------------------
   procedure Write_Wide (Stream : in out Print_Stream;
                         Item   : in Wide_Wide_String) is
   begin
      for I in Item'Range loop
         Stream.Target.Write (Char => Character'Val (Wide_Wide_Character'Pos (Item (I))));
      end loop;
   end Write_Wide;

   --  ------------------------------
   --  Write the object converted into a string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Item   : in EL.Objects.Object) is
   begin
      Stream.Target.Write (EL.Objects.To_String (Item));
   end Write;

   --  ------------------------------
   --  Write the buffer array to the output stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
   begin
      Stream.Target.Write (Buffer);
   end Write;

   --  ------------------------------
   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   --  ------------------------------
   procedure Flush (Stream : in out Print_Stream) is
   begin
      Stream.Target.Flush;
   end Flush;

   --  ------------------------------
   --  Close the text stream.
   --  ------------------------------
   overriding
   procedure Close (Stream : in out Print_Stream) is
   begin
      Stream.Target.Close;
   end Close;

   --  ------------------------------
   --  Write into the text stream.
   --  ------------------------------
   procedure Write (Stream : in out Print_Stream;
                    Print  : access procedure
                      (Into : in out Util.Streams.Texts.Print_Stream'Class)) is
   begin
      Print (Into => Stream.Target.all);
   end Write;

end ASF.Streams;
