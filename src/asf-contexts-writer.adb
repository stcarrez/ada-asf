-----------------------------------------------------------------------
--  writer -- Response stream writer
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with Unicode;
with Ada.Unchecked_Deallocation;
package body ASF.Contexts.Writer is

   use Unicode;

   procedure Write_Escape (Stream : in out ResponseWriter'Class;
                           Char   : in Character);

   --  Close the current XML entity if an entity was started
   procedure Close_Current (Stream : in out ResponseWriter'Class);

   procedure Free_Buffer is
     new Ada.Unchecked_Deallocation (Object => Stream_Element_Array,
                                     Name   => Buffer_Access);

   --  ------------------------------
   --  Response Writer
   --  ------------------------------

   --  ------------------------------
   --  Initialize the response stream for the given content type and
   --  encoding.  An internal buffer is allocated for writing the stream.
   --  ------------------------------
   procedure Initialize (Stream       : in out ResponseWriter;
                         Content_Type : in String;
                         Encoding     : in String;
                         Size         : in Positive) is
   begin
      Free_Buffer (Stream.Buffer);
      Stream.Content_Type := To_Unbounded_String (Content_Type);
      Stream.Encoding     := Unicode.Encodings.Get_By_Name (Encoding);
      Stream.Last         := Stream_Element_Offset (Size);
      Stream.Buffer := new Stream_Element_Array (1 .. Stream.Last);
      Stream.Pos    := 1;
   end Initialize;

   --  ------------------------------
   --  Flush the response stream and release the buffer.
   --  ------------------------------
   procedure Finalize (Object : in out ResponseWriter) is
   begin
      if Object.Buffer /= null then
         Object.Flush;
         Free_Buffer (Object.Buffer);
      end if;
   end Finalize;

   --  ------------------------------
   --  Flush the response stream.
   --  ------------------------------
   procedure Flush (Stream : in out ResponseWriter) is
   begin
      if Stream.Pos <= 1 then
         return;
      end if;
      ResponseWriter'Class (Stream).Write (Stream.Buffer (1 .. Stream.Pos - 1));
      Stream.Pos := 1;
   end Flush;

   --  ------------------------------
   --  Get the content type.
   --  ------------------------------
   function Get_Content_Type (Stream : in ResponseWriter) return String is
   begin
      return To_String (Stream.Content_Type);
   end Get_Content_Type;

   --  ------------------------------
   --  Get the character encoding.
   --  ------------------------------
   function Get_Encoding (Stream : in ResponseWriter) return String is
   begin
      return Stream.Encoding.Name.all;
   end Get_Encoding;

   --  ------------------------------
   --  Close the current XML entity if an entity was started
   --  ------------------------------
   procedure Close_Current (Stream : in out ResponseWriter'Class) is
   begin
      if Stream.Close_Start then
         Stream.Write ('>');
         Stream.Close_Start := False;
      end if;
   end Close_Current;

   --  ------------------------------
   --  Start an XML element with the given name.
   --  ------------------------------
   procedure Start_Element (Stream : in out ResponseWriter;
                            Name   : in String) is
   begin
      Close_Current (Stream);
      Stream.Write ('<');
      Stream.Write (Name);
      Stream.Close_Start := True;
   end Start_Element;

   --  ------------------------------
   --  Start an XML element with the given name.
   --  ------------------------------
   procedure Start_Optional_Element (Stream : in out ResponseWriter;
                                     Name   : in String) is
   begin
      Close_Current (Stream);
      Stream.Optional_Element (1 .. Name'Length) := Name;
      Stream.Optional_Element_Size := Name'Length;
   end Start_Optional_Element;

   --  ------------------------------
   --  Closes an XML element of the given name.
   --  ------------------------------
   procedure End_Element (Stream : in out ResponseWriter;
                          Name   : in String) is
   begin
      Close_Current (Stream);
      Stream.Write ("</");
      Stream.Write (Name);
      Stream.Write ('>');
   end End_Element;

   --  ------------------------------
   --  Closes an XML element of the given name.
   --  ------------------------------
   procedure End_Optional_Element (Stream : in out ResponseWriter;
                                   Name   : in String) is
   begin
      Close_Current (Stream);
      if Stream.Optional_Element_Written then
         Stream.Write ("</");
         Stream.Write (Name);
         Stream.Write ('>');
      end if;
      Stream.Optional_Element_Written := False;
      Stream.Optional_Element_Size    := 0;
   end End_Optional_Element;

   --  ------------------------------
   --  Write an XML element using the given name and with the content.
   --  This is similar to calling <b>Start_Element</b>, <b>Write_Text</b>
   --  and <b>End_Element</b>.
   --  ------------------------------
   procedure Write_Element (Stream   : in out ResponseWriter;
                            Name     : in String;
                            Content  : in String) is
   begin
      Stream.Start_Element (Name);
      Stream.Write_Text (Content);
      Stream.End_Element (Name);
   end Write_Element;

   procedure Write_Wide_Element (Stream   : in out ResponseWriter;
                                 Name     : in String;
                                 Content  : in Wide_Wide_String) is
   begin
      Stream.Start_Element (Name);
      Stream.Write_Wide_Text (Content);
      Stream.End_Element (Name);
   end Write_Wide_Element;

   procedure Write_Attribute (Stream : in out ResponseWriter;
                              Name   : in String;
                              Value  : in String) is
   begin
      --  If we have an optional element, start it.
      if Stream.Optional_Element_Size > 0 and not Stream.Optional_Element_Written then
         Stream.Write ('<');
         Stream.Write (Stream.Optional_Element (1 .. Stream.Optional_Element_Size));
         Stream.Close_Start := True;
         Stream.Optional_Element_Written := True;
      end if;
      if Stream.Close_Start then
         Stream.Write (' ');
         Stream.Write (Name);
         Stream.Write ('=');
         Stream.Write ('"');
         for I in Value'Range loop
            declare
               C : constant Character := Value (I);
            begin
               if C = '"' then
                  Stream.Write ("&quot;");
               else
                  Stream.Write_Escape (C);
               end if;
            end;
         end loop;
         Stream.Write ('"');
      end if;
   end Write_Attribute;

   procedure Write_Attribute (Stream : in out ResponseWriter;
                              Name   : in String;
                              Value  : in Unbounded_String) is
   begin
      Stream.Write_Attribute (Name, To_String (Value));
   end Write_Attribute;

   procedure Write_Attribute (Stream : in out ResponseWriter;
                              Name   : in String;
                              Value  : in EL.Objects.Object) is
      S : constant String := EL.Objects.To_String (Value);
   begin
      Stream.Write_Attribute (Name, S);
   end Write_Attribute;

   --  ------------------------------
   --  Write a text escaping any character as necessary.
   --  ------------------------------
   procedure Write_Text (Stream : in out ResponseWriter;
                         Text   : in String) is
   begin
      for I in Text'Range loop
         ResponseWriter'Class (Stream).Write_Char (Text (I));
      end loop;
   end Write_Text;

   procedure Write_Text (Stream : in out ResponseWriter;
                         Text   : in Unbounded_String) is
      Count : constant Natural := Length (Text);
   begin
      if Count > 0 then
         for I in 1 .. Count loop
            ResponseWriter'Class (Stream).Write_Char (Element (Text, I));
         end loop;
      end if;
   end Write_Text;

   procedure Write_Wide_Text (Stream : in out ResponseWriter;
                              Text   : in Wide_Wide_String) is
   begin
      for I in Text'Range loop
         ResponseWriter'Class (Stream).Write_Wide_Char (Text (I));
      end loop;
   end Write_Wide_Text;

   procedure Write_Text (Stream : in out ResponseWriter;
                         Value  : in EL.Objects.Object) is
      use EL.Objects;

      Of_Type : constant EL.Objects.Data_Type := EL.Objects.Get_Type (Value);
   begin
      case Of_Type is
         when TYPE_BOOLEAN =>
            Close_Current (Stream);
            if To_Boolean (Value) then
               ResponseWriter'Class (Stream).Write ("true");
            else
               ResponseWriter'Class (Stream).Write ("false");
            end if;

         when TYPE_INTEGER | TYPE_FLOAT =>
            Close_Current (Stream);
            ResponseWriter'Class (Stream).Write (To_String (Value));

         when TYPE_STRING =>
            ResponseWriter'Class (Stream).Write_Text (To_String (Value));

         when others =>
            ResponseWriter'Class (Stream).Write_Wide_Text (To_Wide_Wide_String (Value));

      end case;
   end Write_Text;

   --  ------------------------------
   --  Write a character on the response stream and escape that character
   --  as necessary.
   --  ------------------------------
   procedure Write_Char (Stream : in out ResponseWriter;
                         Char   : in Character) is
   begin
      Close_Current (Stream);
      Write_Escape (Stream, Char);
   end Write_Char;

   --  ------------------------------
   --  Internal method to write a character on the response stream
   --  and escape that character as necessary.  Unlike 'Write_Char',
   --  this operation does not closes the current XML entity.
   --  ------------------------------
   procedure Write_Escape (Stream : in out ResponseWriter'Class;
                           Char   : in Character) is
      Code : constant Unicode_Char := Character'Pos (Char);
   begin
      --  Tilde or less...
      if Code < 16#A0# then
         --  If "?" or over, no escaping is needed (this covers
         --  most of the Latin alphabet)
         if Code > 16#3F# or Code <= 16#20# then
            Stream.Write (Char);
         elsif Char = '<' then
            Stream.Write ("&lt;");
         elsif Char = '>' then
            Stream.Write ("&gt;");
         elsif Char = '&' then
            Stream.Write ("&amp;");
         else
            Stream.Write (Char);
         end if;
      else
         declare
            S : String (1 .. 5) := "&#00;";
            C : Unicode_Char;
         begin
            C := Code and 16#0F#;
            if C > 10 then
               S (4) := Character'Val (C - 10 + Character'Pos ('A'));
            else
               S (4) := Character'Val (C + Character'Pos ('0'));
            end if;
            C := (Code / 16) and 16#0F#;
            if C > 10 then
               S (3) := Character'Val (C - 10 + Character'Pos ('A'));
            else
               S (3) := Character'Val (C + Character'Pos ('0'));
            end if;
            Stream.Write (S);
         end;
      end if;
   end Write_Escape;

   --  ------------------------------
   --  Write a character on the response stream and escape that character
   --  as necessary.
   --  ------------------------------
   procedure Write_Wide_Char (Stream : in out ResponseWriter;
                              Char   : in Wide_Wide_Character) is
      Code : constant Unicode_Char := Wide_Wide_Character'Pos (Char);
   begin
      Close_Current (Stream);
      --  Tilde or less...
      if Code < 16#100# then
         Stream.Write_Char (Character'Val (Code));
      else
         declare
            S : String (1 .. 5) := "&#00;";
            C : Unicode_Char;
         begin
            C := Code and 16#0F#;
            if C > 10 then
               S (4) := Character'Val (C - 10 + Character'Pos ('A'));
            else
               S (4) := Character'Val (C + Character'Pos ('0'));
            end if;
            C := (Code / 16) and 16#0F#;
            if C > 10 then
               S (3) := Character'Val (C - 10 + Character'Pos ('A'));
            else
               S (3) := Character'Val (C + Character'Pos ('0'));
            end if;
            Stream.Write (S);
         end;
      end if;
   end Write_Wide_Char;

   --  ------------------------------
   --  Write a raw character on the response stream.  The character is not
   --  escaped.
   --  ------------------------------
   procedure Write (Stream : in out ResponseWriter;
                    Char   : in Character) is
   begin
      if Stream.Pos > Stream.Last then
         ResponseWriter'Class (Stream).Write (Stream.Buffer.all);
         Stream.Pos := 1;
      end if;
      Stream.Buffer (Stream.Pos) := Stream_Element (Character'Pos (Char));
      Stream.Pos := Stream.Pos + 1;
   end Write;

   --  ------------------------------
   --  Write a raw string on the response stream.  The string is not
   --  escaped.
   --  ------------------------------
   procedure Write (Stream : in out ResponseWriter;
                    Item   : in Unbounded_String) is
      Count : constant Natural := Length (Item);
   begin
      if Count > 0 then
         for I in 1 .. Count loop
            Stream.Write (Char => Element (Item, I));
         end loop;
      end if;
   end Write;

   --  ------------------------------
   --  Write a raw string on the response stream.  The string is not
   --  escaped
   --  ------------------------------
   procedure Write (Stream : in out ResponseWriter;
                    Item   : in String) is
      Start : Positive := Item'First;
      Pos   : Stream_Element_Offset := Stream.Pos;
      Avail : Natural;
      Size  : Natural;
      Char  : Character;
   begin
      while Start <= Item'Last loop
         Size := Item'Last - Start + 1;
         Avail := Natural (Stream.Last - Pos + 1);
         if Avail = 0 then
            ResponseWriter'Class (Stream).Write (Stream.Buffer.all);
            Pos := 1;
            Avail := Natural (Stream.Last);
         end if;
         if Avail < Size then
            Size := Avail;
         end if;
         while Size > 0 loop
            Char := Item (Start);
            Stream.Buffer (Pos) := Stream_Element (Character'Pos (Char));
            Pos := Pos + 1;
            Start := Start + 1;
            Size := Size - 1;
         end loop;
      end loop;
      Stream.Pos := Pos;
   end Write;

end ASF.Contexts.Writer;
