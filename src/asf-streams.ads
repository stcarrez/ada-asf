-----------------------------------------------------------------------
--  ASF.Streams -- Print streams for servlets
--  Copyright (C) 2010 Stephane Carrez
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
with Ada.Streams;
with Ada.Strings.Unbounded;

with Util.Streams;
with Util.Streams.Texts;
with EL.Objects;
package ASF.Streams is

   --  -----------------------
   --  Print stream
   --  -----------------------
   --  The <b>Print_Stream</b> is an output stream which provides helper methods
   --  for writing text streams.
   type Print_Stream is limited new Util.Streams.Output_Stream with private;

   procedure Initialize (Stream : in out Print_Stream;
                         To     : in Util.Streams.Texts.Print_Stream_Access);

   --  Write an integer on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Integer);

   --  Write a string on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String);

   --  Write a raw character on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Char   : in Character);

   --  Write a raw string on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in String);

   --  Write the object converted into a string on the stream.
   procedure Write (Stream : in out Print_Stream;
                    Item   : in EL.Objects.Object);

   --  Write the buffer array to the output stream.
   procedure Write (Stream : in out Print_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Flush the buffer by writing on the output stream.
   --  Raises Data_Error if there is no output stream.
   procedure Flush (Stream : in out Print_Stream);

private

   type Print_Stream is limited new Util.Streams.Output_Stream with record
      Target : Util.Streams.Texts.Print_Stream_Access;
   end record;

end ASF.Streams;
