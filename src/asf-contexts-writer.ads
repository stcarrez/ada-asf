-----------------------------------------------------------------------
--  asf-contexts-writer -- Response stream writer
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2019, 2022 Stephane Carrez
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

--  The <b>ASF.Contexts.Writer</b> defines the response writer to
--  write the rendered result to the response stream.  The <b>IOWriter</b>
--  interface defines the procedure for writing the buffer to the output
--  stream.  The <b>Response_Writer</b> is the main type that provides
--  various methods for writing the content.
--
--  The result stream is encoded according to the encoding type.
--
with Unicode.Encodings;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with EL.Objects;
with Util.Beans.Objects;
with Util.Strings.Builders;
with ASF.Streams;
package ASF.Contexts.Writer is

   use Ada.Strings.Unbounded;
   use Ada.Strings.Wide_Wide_Unbounded;

   --  ------------------------------
   --  IO Writer
   --  ------------------------------
   type IOWriter is limited interface;
   procedure Write (Stream : in out IOWriter;
                    Buffer : in Ada.Streams.Stream_Element_Array) is abstract;

   --  ------------------------------
   --  Response Writer
   --  ------------------------------
   type Response_Writer is new ASF.Streams.Print_Stream with private;
   type Response_Writer_Access is access all Response_Writer'Class;

   --  Backward compatibility
   subtype ResponseWriter is Response_Writer;
   pragma Obsolescent (Entity => ResponseWriter);

   subtype ResponseWriter_Access is Response_Writer_Access;
   pragma Obsolescent (Entity => ResponseWriter_Access);

   --  Initialize the response stream for the given content type and
   --  encoding.  An internal buffer is allocated for writing the stream.
   procedure Initialize (Stream       : in out Response_Writer;
                         Content_Type : in String;
                         Encoding     : in String;
                         Output       : in ASF.Streams.Print_Stream);

   --  Get the content type.
   function Get_Content_Type (Stream : in Response_Writer) return String;

   --  Get the character encoding.
   function Get_Encoding (Stream : in Response_Writer) return String;

   --  Close the current XML entity if an entity was started
   procedure Close_Current (Stream : in out Response_Writer'Class);

   --  Start an XML element with the given name.
   procedure Start_Element (Stream : in out Response_Writer;
                            Name   : in String);

   --  Start an optional XML element with the given name.
   --  The element is written only if it has at least one attribute.
   --  The optional XML element must not contain other XML entities.
   procedure Start_Optional_Element (Stream : in out Response_Writer;
                                     Name   : in String);

   --  Closes an XML element of the given name.
   procedure End_Element (Stream : in out Response_Writer;
                          Name   : in String);

   --  Closes an optional XML element of the given name.
   --  The ending tag is written only if the start tag was written.
   procedure End_Optional_Element (Stream : in out Response_Writer;
                                   Name   : in String);

   --  Write an XML element using the given name and with the content.
   --  This is similar to calling <b>Start_Element</b>, <b>Write_Text</b>
   --  and <b>End_Element</b>.
   procedure Write_Element (Stream   : in out Response_Writer;
                            Name     : in String;
                            Content  : in String);

   procedure Write_Wide_Element (Stream   : in out Response_Writer;
                                 Name     : in String;
                                 Content  : in Wide_Wide_String);

   procedure Write_Wide_Element (Stream   : in out Response_Writer;
                                 Name     : in String;
                                 Content  : in Unbounded_Wide_Wide_String);

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   procedure Write_Attribute (Stream : in out Response_Writer;
                              Name   : in String;
                              Value  : in String);

   procedure Write_Attribute (Stream : in out Response_Writer;
                              Name   : in String;
                              Value  : in Unbounded_String);

   procedure Write_Attribute (Stream : in out Response_Writer;
                              Name   : in String;
                              Value  : in EL.Objects.Object);

   procedure Write_Wide_Attribute (Stream : in out Response_Writer;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String);

   procedure Write_Wide_Attribute (Stream : in out Response_Writer;
                                   Name   : in String;
                                   Value  : in Unbounded_Wide_Wide_String);

   --  Write a text escaping any character as necessary.
   procedure Write_Text (Stream : in out Response_Writer;
                         Text   : in String);
   procedure Write_Text (Stream : in out Response_Writer;
                         Text   : in Unbounded_String);
   procedure Write_Wide_Text (Stream : in out Response_Writer;
                              Text   : in Wide_Wide_String);
   procedure Write_Wide_Text (Stream : in out Response_Writer;
                              Text   : in Unbounded_Wide_Wide_String);
   procedure Write_Text (Stream : in out Response_Writer;
                         Value  : in EL.Objects.Object);

   --  Write a character on the response stream and escape that character
   --  as necessary.
   procedure Write_Char (Stream : in out Response_Writer;
                         Char   : in Character);

   --  Write a character on the response stream and escape that character
   --  as necessary.
   procedure Write_Wide_Char (Stream : in out Response_Writer;
                              Char   : in Wide_Wide_Character);

   --  Write a string on the stream.
   overriding
   procedure Write (Stream : in out Response_Writer;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String);

   --  Write a raw string on the stream.
   procedure Write_Raw (Stream : in out Response_Writer;
                        Item   : in String);

   --  Write a raw wide string on the stream.
   procedure Write_Raw (Stream : in out Response_Writer;
                        Item   : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);

   --  Write a raw wide string on the stream.
   procedure Write_Wide_Raw (Stream : in out Response_Writer;
                             Item   : in Wide_Wide_String);

   --  ------------------------------
   --  Javascript Support
   --  ------------------------------
   --  To optimize the execution of Javascript page code, ASF components can queue some
   --  javascript code and have it merged and executed in a single <script> command.

   --  Write the java scripts that have been queued by <b>Queue_Script</b>
   procedure Write_Scripts (Stream : in out Response_Writer);

   --  Append the <b>Script</b> to the javascript buffer queue.  The <b>Script</b> is not escaped.
   procedure Queue_Script (Stream : in out Response_Writer;
                           Script : in String);

   --  Append the <b>Script</b> to the javascript buffer queue.  The <b>Script</b> is not escaped.
   procedure Queue_Script (Stream : in out Response_Writer;
                           Script : in Ada.Strings.Unbounded.Unbounded_String);

   --  Append the <b>Value</b> to the javascript buffer queue.  The value is escaped according
   --  to Javascript escape rules.
   procedure Queue_Script (Stream : in out Response_Writer;
                           Value  : in Util.Beans.Objects.Object);

   --  Append a <b>script</b> include command to include the Javascript file at the given URL.
   --  The include scripts are flushed by <b>Flush</b> or <b>Write_Scripts</b>.
   procedure Queue_Include_Script (Stream : in out Response_Writer;
                                   URL    : in String;
                                   Async  : in Boolean := False);

   --  Flush the response.
   --  Before flushing the response, the javascript are also flushed
   --  by calling <b>Write_Scripts</b>.
   overriding
   procedure Flush (Stream : in out Response_Writer);

private

   --  Flush the response stream and release the buffer.
   overriding
   procedure Finalize (Object : in out Response_Writer);

   type Buffer_Access is access Ada.Streams.Stream_Element_Array;

   type Response_Writer is new ASF.Streams.Print_Stream with record
      --  Whether an XML element must be closed (that is a '>' is necessary)
      Close_Start : Boolean := False;

      --  The encoding scheme.
      Encoding    : Unicode.Encodings.Unicode_Encoding;

      --  The content type.
      Content_Type : Unbounded_String;

      --  The javascript that has been queued by <b>Queue_Script</b>.
      Script_Queue  : Unbounded_String; -- Util.Strings.Builders.Builder (256);

      --  The javascript files that must be included at the end of the file.
      --  This javascript part is written before the Javascript that was queued.
      Include_Queue : Util.Strings.Builders.Builder (256);

      --  An optional element to write in the stream.
      Optional_Element         : String (1 .. 32);
      Optional_Element_Size    : Natural := 0;
      Optional_Element_Written : Boolean := False;
   end record;

end ASF.Contexts.Writer;
