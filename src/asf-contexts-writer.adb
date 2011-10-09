-----------------------------------------------------------------------
--  writer -- Response stream writer
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with Util.Strings.Transforms;
with Unicode;
package body ASF.Contexts.Writer is

   use Unicode;

   --  Internal method to write a character on the response stream
   --  and escape that character as necessary.  Unlike 'Write_Char',
   --  this operation does not closes the current XML entity.
   procedure Write_Escape (Stream : in out Response_Writer'Class;
                           Char   : in Character);

   procedure Write_Escape (Stream : in out Response_Writer'Class;
                           Char   : in Wide_Wide_Character);

   --  Close the current XML entity if an entity was started
   procedure Close_Current (Stream : in out Response_Writer'Class);

   --  ------------------------------
   --  Response Writer
   --  ------------------------------

   --  ------------------------------
   --  Initialize the response stream for the given content type and
   --  encoding.  An internal buffer is allocated for writing the stream.
   --  ------------------------------
   procedure Initialize (Stream       : in out Response_Writer;
                         Content_Type : in String;
                         Encoding     : in String;
                         Output       : in ASF.Streams.Print_Stream) is
   begin
      Stream.Initialize (Output);
      Stream.Content_Type := To_Unbounded_String (Content_Type);
      Stream.Encoding     := Unicode.Encodings.Get_By_Name (Encoding);
   end Initialize;

   --  ------------------------------
   --  Flush the response stream and release the buffer.
   --  ------------------------------
   procedure Finalize (Object : in out Response_Writer) is
   begin
      Object.Flush;
   end Finalize;

   --  ------------------------------
   --  Get the content type.
   --  ------------------------------
   function Get_Content_Type (Stream : in Response_Writer) return String is
   begin
      return To_String (Stream.Content_Type);
   end Get_Content_Type;

   --  ------------------------------
   --  Get the character encoding.
   --  ------------------------------
   function Get_Encoding (Stream : in Response_Writer) return String is
   begin
      return Stream.Encoding.Name.all;
   end Get_Encoding;

   --  ------------------------------
   --  Close the current XML entity if an entity was started
   --  ------------------------------
   procedure Close_Current (Stream : in out Response_Writer'Class) is
   begin
      if Stream.Close_Start then
         Stream.Write ('>');
         Stream.Close_Start := False;
      end if;
   end Close_Current;

   --  ------------------------------
   --  Start an XML element with the given name.
   --  ------------------------------
   procedure Start_Element (Stream : in out Response_Writer;
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
   procedure Start_Optional_Element (Stream : in out Response_Writer;
                                     Name   : in String) is
   begin
      Close_Current (Stream);
      Stream.Optional_Element (1 .. Name'Length) := Name;
      Stream.Optional_Element_Size := Name'Length;
   end Start_Optional_Element;

   --  ------------------------------
   --  Closes an XML element of the given name.
   --  ------------------------------
   procedure End_Element (Stream : in out Response_Writer;
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
   procedure End_Optional_Element (Stream : in out Response_Writer;
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
   procedure Write_Element (Stream   : in out Response_Writer;
                            Name     : in String;
                            Content  : in String) is
   begin
      Stream.Start_Element (Name);
      Stream.Write_Text (Content);
      Stream.End_Element (Name);
   end Write_Element;

   procedure Write_Wide_Element (Stream   : in out Response_Writer;
                                 Name     : in String;
                                 Content  : in Wide_Wide_String) is
   begin
      Stream.Start_Element (Name);
      Stream.Write_Wide_Text (Content);
      Stream.End_Element (Name);
   end Write_Wide_Element;

   procedure Write_Wide_Element (Stream   : in out Response_Writer;
                                 Name     : in String;
                                 Content  : in Unbounded_Wide_Wide_String) is
   begin
      Stream.Start_Element (Name);
      Stream.Write_Wide_Text (Content);
      Stream.End_Element (Name);
   end Write_Wide_Element;

   procedure Write_Attribute (Stream : in out Response_Writer;
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

   procedure Write_Attribute (Stream : in out Response_Writer;
                              Name   : in String;
                              Value  : in Unbounded_String) is
   begin
      Stream.Write_Attribute (Name, To_String (Value));
   end Write_Attribute;

   procedure Write_Attribute (Stream : in out Response_Writer;
                              Name   : in String;
                              Value  : in EL.Objects.Object) is
      S : constant String := EL.Objects.To_String (Value);
   begin
      Stream.Write_Attribute (Name, S);
   end Write_Attribute;

   procedure Write_Wide_Attribute (Stream : in out Response_Writer;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String) is
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
               C : constant Wide_Wide_Character := Value (I);
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
   end Write_Wide_Attribute;

   procedure Write_Wide_Attribute (Stream : in out Response_Writer;
                                   Name   : in String;
                                   Value  : in Unbounded_Wide_Wide_String) is
   begin
      Stream.Write_Wide_Attribute (Name, To_Wide_Wide_String (Value));
   end Write_Wide_Attribute;

   --  ------------------------------
   --  Write a text escaping any character as necessary.
   --  ------------------------------
   procedure Write_Text (Stream : in out Response_Writer;
                         Text   : in String) is
   begin
      for I in Text'Range loop
         Response_Writer'Class (Stream).Write_Char (Text (I));
      end loop;
   end Write_Text;

   procedure Write_Text (Stream : in out Response_Writer;
                         Text   : in Unbounded_String) is
      Count : constant Natural := Length (Text);
   begin
      if Count > 0 then
         for I in 1 .. Count loop
            Response_Writer'Class (Stream).Write_Char (Element (Text, I));
         end loop;
      end if;
   end Write_Text;

   procedure Write_Wide_Text (Stream : in out Response_Writer;
                              Text   : in Wide_Wide_String) is
   begin
      for I in Text'Range loop
         Response_Writer'Class (Stream).Write_Wide_Char (Text (I));
      end loop;
   end Write_Wide_Text;

   procedure Write_Wide_Text (Stream : in out Response_Writer;
                              Text   : in Unbounded_Wide_Wide_String) is
      Count : constant Natural := Length (Text);
   begin
      if Count > 0 then
         for I in 1 .. Count loop
            Response_Writer'Class (Stream).Write_Wide_Char (Element (Text, I));
         end loop;
      end if;
   end Write_Wide_Text;

   procedure Write_Text (Stream : in out Response_Writer;
                         Value  : in EL.Objects.Object) is
      use EL.Objects;

      Of_Type : constant EL.Objects.Data_Type := EL.Objects.Get_Type (Value);
   begin
      case Of_Type is
         when TYPE_BOOLEAN =>
            Close_Current (Stream);
            if To_Boolean (Value) then
               Response_Writer'Class (Stream).Write ("true");
            else
               Response_Writer'Class (Stream).Write ("false");
            end if;

         when TYPE_INTEGER | TYPE_FLOAT =>
            Close_Current (Stream);
            Response_Writer'Class (Stream).Write (To_String (Value));

         when TYPE_STRING =>
            Response_Writer'Class (Stream).Write_Text (To_String (Value));

         when others =>
            Response_Writer'Class (Stream).Write_Wide_Text (To_Wide_Wide_String (Value));

      end case;
   end Write_Text;

   --  ------------------------------
   --  Write a character on the response stream and escape that character
   --  as necessary.
   --  ------------------------------
   procedure Write_Char (Stream : in out Response_Writer;
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
   procedure Write_Escape (Stream : in out Response_Writer'Class;
                           Char   : in Character) is
      Code : constant Unicode_Char := Character'Pos (Char);
   begin
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
   end Write_Escape;

   --  ------------------------------
   --  Internal method to write a character on the response stream
   --  and escape that character as necessary.  Unlike 'Write_Char',
   --  this operation does not closes the current XML entity.
   --  ------------------------------
   procedure Write_Escape (Stream : in out Response_Writer'Class;
                           Char   : in Wide_Wide_Character) is
      Code : constant Unicode_Char := Wide_Wide_Character'Pos (Char);
   begin
      --  Tilde or less...
      if Code < 16#A0# then
         --  If "?" or over, no escaping is needed (this covers
         --  most of the Latin alphabet)
         if Code > 16#3F# or Code <= 16#20# then
            Stream.Write (Character'Val (Code));
         elsif Char = '<' then
            Stream.Write ("&lt;");
         elsif Char = '>' then
            Stream.Write ("&gt;");
         elsif Char = '&' then
            Stream.Write ("&amp;");
         else
            Stream.Write (Character'Val (Code));
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
   procedure Write_Wide_Char (Stream : in out Response_Writer;
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
   --  Write a string on the stream.
   --  ------------------------------
   procedure Write (Stream : in out Response_Writer;
                    Item   : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Close_Current (Stream);
      ASF.Streams.Print_Stream (Stream).Write (Item);
   end Write;

   --  ------------------------------
   --  Write the java scripts that have been queued by <b>Queue_Script</b>
   --  ------------------------------
   procedure Write_Scripts (Stream : in out Response_Writer) is
   begin
      if Length (Stream.Script_Queue) = 0 then
         return;
      end if;
      Stream.Start_Element ("script");
      Stream.Write_Attribute ("type", "text/javascript");
      Close_Current (Stream);
      Stream.Write (Stream.Script_Queue);
      Stream.End_Element ("script");
      Stream.Script_Queue := Ada.Strings.Unbounded.Null_Unbounded_String;
   end Write_Scripts;

   --  ------------------------------
   --  Append the <b>Script</b> to the javascript buffer queue.  The <b>Script</b> is not escaped.
   --  ------------------------------
   procedure Queue_Script (Stream : in out Response_Writer;
                           Script : in String) is
   begin
      Append (Stream.Script_Queue, Script);
   end Queue_Script;

   --  ------------------------------
   --  Append the <b>Script</b> to the javascript buffer queue.  The <b>Script</b> is not escaped.
   --  ------------------------------
   procedure Queue_Script (Stream : in out Response_Writer;
                           Script : in Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Append (Stream.Script_Queue, Script);
   end Queue_Script;

   --  ------------------------------
   --  Append the <b>Value</b> to the javascript buffer queue.  The value is escaped according
   --  to Javascript escape rules.
   --  ------------------------------
   procedure Queue_Script (Stream : in out Response_Writer;
                           Value  : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;

      Of_Type : constant Util.Beans.Objects.Data_Type := Util.Beans.Objects.Get_Type (Value);
   begin
      case Of_Type is
         when TYPE_BOOLEAN =>
            if To_Boolean (Value) then
               Append (Stream.Script_Queue, "true");
            else
               Append (Stream.Script_Queue, "false");
            end if;

         when TYPE_INTEGER | TYPE_FLOAT =>
            Append (Stream.Script_Queue, To_String (Value));

         when TYPE_STRING =>
            Util.Strings.Transforms.Escape_Javascript (Content => To_String (Value),
                                                       Into    => Stream.Script_Queue);

         when others =>
            Util.Strings.Transforms.Escape_Javascript (Content => To_String (Value),
                                                       Into    => Stream.Script_Queue);

      end case;
   end Queue_Script;

   --  ------------------------------
   --  Flush the response.
   --  Before flusing the response, the javascript are also flushed
   --  by calling <b>Write_Scripts</b>.
   --  ------------------------------
   overriding
   procedure Flush (Stream : in out Response_Writer) is
   begin
      Stream.Write_Scripts;
      ASF.Streams.Print_Stream (Stream).Flush;
   end Flush;

end ASF.Contexts.Writer;
