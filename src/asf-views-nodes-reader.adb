-----------------------------------------------------------------------
--  asf-views-nodes-reader -- XHTML Reader
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2017, 2019, 2022 Stephane Carrez
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

with Ada.Unchecked_Deallocation;
with Unicode;

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Util.Log.Loggers;
with Util.Serialize.IO.XML;
package body ASF.Views.Nodes.Reader is

   use Sax.Readers;
   use Sax.Exceptions;
   use Sax.Locators;
   use Sax.Attributes;
   use Ada.Strings.Fixed;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Views.Nodes.Reader");

   procedure Free is
     new Ada.Unchecked_Deallocation (Element_Context_Array,
                                     Element_Context_Array_Access);

   procedure Push (Handler : in out Xhtml_Reader'Class);
   procedure Pop (Handler  : in out Xhtml_Reader'Class);

   --  Freeze the current Text_Tag node, counting the number of elements it contains.
   procedure Finish_Text_Node (Handler : in out Xhtml_Reader'Class);

   function Is_Self_Closing (Tag : in String) return Boolean;

   --  ------------------------------
   --  Push the current context when entering in an element.
   --  ------------------------------
   procedure Push (Handler : in out Xhtml_Reader'Class) is
   begin
      if Handler.Stack = null then
         Handler.Stack := new Element_Context_Array (1 .. 100);
      elsif Handler.Stack_Pos = Handler.Stack'Last then
         declare
            Old : Element_Context_Array_Access := Handler.Stack;
         begin
            Handler.Stack := new Element_Context_Array (1 .. Old'Last + 100);
            Handler.Stack (1 .. Old'Last) := Old (1 .. Old'Last);
            Free (Old);
         end;
      end if;
      if Handler.Stack_Pos /= Handler.Stack'First then
         Handler.Stack (Handler.Stack_Pos + 1) := Handler.Stack (Handler.Stack_Pos);
      end if;
      Handler.Stack_Pos := Handler.Stack_Pos + 1;
      Handler.Current := Handler.Stack (Handler.Stack_Pos)'Access;
   end Push;

   --  ------------------------------
   --  Pop the context and restore the previous context when leaving an element
   --  ------------------------------
   procedure Pop (Handler  : in out Xhtml_Reader'Class) is
   begin
      Handler.Stack_Pos := Handler.Stack_Pos - 1;
      Handler.Current := Handler.Stack (Handler.Stack_Pos)'Access;
   end Pop;

   --  ------------------------------
   --  Find the function knowing its name.
   --  ------------------------------
   overriding
   function Get_Function (Mapper    : NS_Function_Mapper;
                          Namespace : String;
                          Name      : String) return Function_Access is
      use NS_Mapping;

      Pos : constant NS_Mapping.Cursor := NS_Mapping.Find (Mapper.Mapping, Namespace);
   begin
      if Has_Element (Pos) then
         return Mapper.Mapper.Get_Function (Element (Pos), Name);
      end if;
      raise No_Function with "Function '" & Namespace & ':' & Name & "' not found";
   end Get_Function;

   --  ------------------------------
   --  Bind a name to a function in the given namespace.
   --  ------------------------------
   overriding
   procedure Set_Function (Mapper    : in out NS_Function_Mapper;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_Access) is
   begin
      null;
   end Set_Function;

   --  ------------------------------
   --  Find the create function bound to the name in the given namespace.
   --  Returns null if no such binding exist.
   --  ------------------------------
   function Find (Mapper    : NS_Function_Mapper;
                  Namespace : String;
                  Name      : String) return ASF.Views.Nodes.Binding_Type is
   begin
      return ASF.Factory.Find (Mapper.Factory.all, Namespace, Name);
   end Find;

   procedure Set_Namespace (Mapper : in out NS_Function_Mapper;
                            Prefix : in String;
                            URI    : in String) is
      use NS_Mapping;
   begin
      Log.Debug ("Add namespace {0}:{1}", Prefix, URI);

      Mapper.Mapping.Include (Prefix, URI);
   end Set_Namespace;

   --  ------------------------------
   --  Remove the namespace prefix binding.
   --  ------------------------------
   procedure Remove_Namespace (Mapper : in out NS_Function_Mapper;
                               Prefix : in String) is
      use NS_Mapping;

      Pos : NS_Mapping.Cursor := NS_Mapping.Find (Mapper.Mapping, Prefix);
   begin
      Log.Debug ("Remove namespace {0}", Prefix);

      if Has_Element (Pos) then
         NS_Mapping.Delete (Mapper.Mapping, Pos);
      end if;
   end Remove_Namespace;

   --  ------------------------------
   --  Warning
   --  ------------------------------
   overriding
   procedure Warning (Handler : in out Xhtml_Reader;
                      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
      pragma Warnings (Off, Handler);
   begin
      Log.Warn ("{0}: {1}", Util.Serialize.IO.XML.Get_Location (Except), Get_Message (Except));
   end Warning;

   --  ------------------------------
   --  Error
   --  ------------------------------
   overriding
   procedure Error (Handler : in out Xhtml_Reader;
                    Except  : in Sax.Exceptions.Sax_Parse_Exception'Class) is
      pragma Warnings (Off, Handler);
   begin
      Log.Error ("{0}: {1}", Util.Serialize.IO.XML.Get_Location (Except), Get_Message (Except));
   end Error;

   --  ------------------------------
   --  Fatal_Error
   --  ------------------------------
   overriding
   procedure Fatal_Error (Handler : in out Xhtml_Reader;
                          Except  : in Sax.Exceptions.Sax_Parse_Exception'Class) is
      pragma Unreferenced (Handler);
      Message : constant String := Get_Message (Except);
   begin
      Log.Error ("{0}: {1}", Util.Serialize.IO.XML.Get_Location (Except), Message);
      raise Parsing_Error with Message;
   end Fatal_Error;

   --  ------------------------------
   --  Set_Document_Locator
   --  ------------------------------
   overriding
   procedure Set_Document_Locator (Handler : in out Xhtml_Reader;
                                   Loc     : in out Sax.Locators.Locator) is
   begin
      Handler.Locator := Loc;
   end Set_Document_Locator;

   --  ------------------------------
   --  Start_Document
   --  ------------------------------
   overriding
   procedure Start_Document (Handler : in out Xhtml_Reader) is
   begin
      null;
   end Start_Document;

   --  ------------------------------
   --  End_Document
   --  ------------------------------
   overriding
   procedure End_Document (Handler : in out Xhtml_Reader) is
   begin
      null;
   end End_Document;

   --  ------------------------------
   --  Start_Prefix_Mapping
   --  ------------------------------
   overriding
   procedure Start_Prefix_Mapping (Handler : in out Xhtml_Reader;
                                   Prefix  : in Unicode.CES.Byte_Sequence;
                                   URI     : in Unicode.CES.Byte_Sequence) is
   begin
      if Prefix = "" then
         Handler.Add_NS := To_Unbounded_String (URI);
      else
         Handler.Functions.Set_Namespace (Prefix => Prefix, URI => URI);
      end if;
   end Start_Prefix_Mapping;

   --  ------------------------------
   --  End_Prefix_Mapping
   --  ------------------------------
   overriding
   procedure End_Prefix_Mapping (Handler : in out Xhtml_Reader;
                                 Prefix  : in Unicode.CES.Byte_Sequence) is
   begin
      Handler.Functions.Remove_Namespace (Prefix => Prefix);
   end End_Prefix_Mapping;

   --  ------------------------------
   --  Collect the text for an EL expression.  The EL expression starts
   --  with either '#{' or with '${' and ends with the matching '}'.
   --  If the <b>Value</b> string does not contain the whole EL expression
   --  the <b>Expr_Buffer</b> stored in the reader is used to collect
   --  that expression.
   --  ------------------------------
   procedure Collect_Expression (Handler  : in out Xhtml_Reader) is
      use Ada.Exceptions;

      Expr    : constant String := To_String (Handler.Expr_Buffer);
      Content : constant Tag_Content_Access := Handler.Text.Last;
   begin
      Handler.Expr_Buffer := Null_Unbounded_String;

      Content.Expr := EL.Expressions.Create_Expression (Expr, Handler.ELContext.all);

      Content.Next := new Tag_Content;
      Handler.Text.Last := Content.Next;

   exception
      when E : EL.Functions.No_Function | EL.Expressions.Invalid_Expression =>
         Log.Error ("{0}: Invalid expression: {1}",
                    To_String (Handler.Locator),
                    Exception_Message (E));
         Log.Error ("{0}: {1}",
                    To_String (Handler.Locator),
                    Expr);

      when E : others =>
         Log.Error ("{0}: Internal error: {1}:{2}",
                    To_String (Handler.Locator),
                    Exception_Name (E),
                    Exception_Message (E));
   end Collect_Expression;

   --  ------------------------------
   --  Collect the raw-text in a buffer.  The text must be flushed
   --  when a new element is started or when an exiting element is closed.
   --  ------------------------------
   procedure Collect_Text (Handler : in out Xhtml_Reader;
                           Value   : in Unicode.CES.Byte_Sequence) is
      Pos      : Natural := Value'First;
      C        : Character;
      Content  : Tag_Content_Access;
      Start_Pos : Natural;
      Last_Pos  : Natural;
   begin
      while Pos <= Value'Last loop
         case Handler.State is

            --  Collect the white spaces and newlines in the 'Spaces'
            --  buffer to ignore empty lines but still honor indentation.
            when NO_CONTENT =>
               loop
                  C := Value (Pos);
                  if C = ASCII.CR or else C = ASCII.LF then
                     Handler.Spaces := Null_Unbounded_String;
                  elsif C = ' ' or else C = ASCII.HT then
                     Append (Handler.Spaces, C);
                  else
                     Handler.State := HAS_CONTENT;
                     exit;
                  end if;
                  Pos := Pos + 1;
                  exit when Pos > Value'Last;
               end loop;

               --  Collect an EL expression until the end of that
               --  expression.  Evaluate the expression.
            when PARSE_EXPR =>
               Start_Pos := Pos;
               loop
                  C := Value (Pos);
                  Last_Pos := Pos;
                  Pos := Pos + 1;
                  if C = '}' then
                     Handler.State := HAS_CONTENT;
                     exit;
                  end if;
                  exit when Pos > Value'Last;
               end loop;

               Append (Handler.Expr_Buffer, Value (Start_Pos .. Last_Pos));
               if Handler.State /= PARSE_EXPR then
                  Handler.Collect_Expression;
               end if;

               --  Collect the raw text in the current content buffer
            when HAS_CONTENT =>
               if Handler.Text = null then
                  Handler.Text := new Text_Tag_Node;
                  Initialize (Handler.Text.all'Access, Null_Binding,
                              Handler.Line, Handler.Current.Parent, null);
                  Handler.Text.Last := Handler.Text.Content'Access;

               elsif Length (Handler.Expr_Buffer) > 0 then
                  Handler.Collect_Expression;
                  Pos := Pos + 1;

               end if;
               Content := Handler.Text.Last;

               --  Scan until we find the start of an EL expression
               --  or we have a new line.
               Start_Pos := Pos;
               loop
                  C := Value (Pos);
                  --  Check for the EL start #{ or ${
                  if (C = '#' or else C = '$')
                    and then Pos + 1 <= Value'Last
                    and then Value (Pos + 1) = '{'
                  then
                     Handler.State := PARSE_EXPR;
                     Append (Handler.Expr_Buffer, C);
                     Append (Handler.Expr_Buffer, '{');
                     Last_Pos := Pos - 1;
                     Pos := Pos + 2;
                     exit;

                     --  Handle \#{ and \${ as escape sequence
                  elsif C = '\' and then Pos + 2 <= Value'Last
                    and then Value (Pos + 2) = '{'
                    and then (Value (Pos + 1) = '#' or else Value (Pos + 1) = '$')
                  then
                     --  Since we have to strip the '\', flush the spaces and append the text
                     --  but ignore the '\'.
                     Append (Content.Text, Handler.Spaces);
                     Handler.Spaces := Null_Unbounded_String;
                     if Start_Pos < Pos then
                        Append (Content.Text, Value (Start_Pos .. Pos - 1));
                     end if;
                     Start_Pos := Pos + 1;
                     Pos := Pos + 2;

                  elsif (C = ASCII.CR or else C = ASCII.LF)
                    and then Handler.Ignore_Empty_Lines
                  then
                     Last_Pos := Pos;
                     Handler.State := NO_CONTENT;
                     exit;
                  end if;
                  Last_Pos := Pos;
                  Pos := Pos + 1;
                  exit when Pos > Value'Last;
               end loop;

               --  If we have some pending spaces, add them in the text stream.
               if Length (Handler.Spaces) > 0 then
                  Append (Content.Text, Handler.Spaces);
                  Handler.Spaces := Null_Unbounded_String;
               end if;

               --  If we have some text, append to the current content buffer.
               if Start_Pos <= Last_Pos then
                  Append (Content.Text, Value (Start_Pos .. Last_Pos));
               end if;
         end case;
      end loop;
   end Collect_Text;

   --  ------------------------------
   --  Freeze the current Text_Tag node, counting the number of elements it contains.
   --  ------------------------------
   procedure Finish_Text_Node (Handler : in out Xhtml_Reader'Class) is
   begin
      if Handler.Text /= null then
         Handler.Text.Freeze;
         Handler.Text := null;
      end if;
   end Finish_Text_Node;

   --  ------------------------------
   --  Start_Element
   --  ------------------------------
   overriding
   procedure Start_Element (Handler       : in out Xhtml_Reader;
                            Namespace_URI : in Unicode.CES.Byte_Sequence := "";
                            Local_Name    : in Unicode.CES.Byte_Sequence := "";
                            Qname         : in Unicode.CES.Byte_Sequence := "";
                            Atts          : in Sax.Attributes.Attributes'Class) is

      use Ada.Exceptions;

      Attr_Count : Natural;
      Attributes : Tag_Attribute_Array_Access;
      Node       : Tag_Node_Access;
      Factory    : ASF.Views.Nodes.Binding_Type;
   begin
      Handler.Line.Line := Sax.Locators.Get_Line_Number (Handler.Locator);
      Handler.Line.Column := Sax.Locators.Get_Column_Number (Handler.Locator);
      if not Handler.Current.Has_Content then
         Handler.Current.Has_Content := True;
         Handler.Collect_Text (">");
      end if;

      --  Push the current context to keep track where we are.
      Push (Handler);
      Attr_Count := Get_Length (Atts);
      Factory := Handler.Functions.Find (Namespace => Namespace_URI,
                                         Name      => Local_Name);
      if Factory.Name /= null then
         if Length (Handler.Add_NS) > 0 then
            Attributes := new Tag_Attribute_Array (0 .. Attr_Count);
            Attributes (0).Name  := To_Unbounded_String ("xmlns");
            Attributes (0).Value := Handler.Add_NS;
            Handler.Add_NS := To_Unbounded_String ("");
         else
            Attributes := new Tag_Attribute_Array (1 .. Attr_Count);
         end if;
         for I in 0 .. Attr_Count - 1 loop
            declare
               Attr  : constant Tag_Attribute_Access := Attributes (I + 1)'Access;
               Value : constant String := Get_Value (Atts, I);
               Name  : constant String := Get_Qname (Atts, I);
               Expr  : EL.Expressions.Expression_Access;
            begin
               Attr.Name  := To_Unbounded_String (Name);
               if Index (Value, "#{") > 0 or else Index (Value, "${") > 0 then
                  begin
                     Expr := new EL.Expressions.Expression;
                     Attr.Binding := Expr.all'Access;
                     EL.Expressions.Expression (Expr.all) := EL.Expressions.Create_Expression
                       (Value, Handler.ELContext.all);
                  exception
                     when E : EL.Functions.No_Function =>
                        Log.Error ("{0}: Invalid expression: {1}",
                                   To_String (Handler.Locator),
                                   Exception_Message (E));
                        Attr.Binding := null;
                        Attr.Value := To_Unbounded_String ("");

                     when E : EL.Expressions.Invalid_Expression =>
                        Log.Error ("{0}: Invalid expression: {1}",
                                   To_String (Handler.Locator),
                                   Exception_Message (E));
                        Attr.Binding := null;
                        Attr.Value := To_Unbounded_String ("");
                  end;
               else
                  Attr.Value := To_Unbounded_String (Value);
               end if;
            end;
         end loop;
         Node := Factory.Tag (Binding    => Factory,
                              Line       => Handler.Line,
                              Parent     => Handler.Current.Parent,
                              Attributes => Attributes);
         Handler.Current.Parent := Node;
         Handler.Current.Text   := False;
         Handler.Current.Has_Content := True;

         Finish_Text_Node (Handler);
         Handler.Spaces := Null_Unbounded_String;
         Handler.State  := Handler.Default_State;

      else
         declare
            Is_Unknown : constant Boolean := Namespace_URI /= "" and then Index (Qname, ":") > 0;
         begin
            --  Optimization: we know in which state we are.
            Handler.State := HAS_CONTENT;
            Handler.Current.Text := True;
            Handler.Current.Has_Content := False;
            if Is_Unknown then
               Log.Error ("{0}: Element '{1}' not found",
                          To_String (Handler.Locator), Qname);
            end if;
            if Handler.Escape_Unknown_Tags and then Is_Unknown then
               Handler.Collect_Text ("&lt;");
            else
               Handler.Collect_Text ("<");
            end if;
            Handler.Collect_Text (Qname);
            if Length (Handler.Add_NS) > 0 then
               Handler.Collect_Text (" xmlns=""");
               Handler.Collect_Text (To_String (Handler.Add_NS));
               Handler.Collect_Text ("""");
               Handler.Add_NS := To_Unbounded_String ("");
            end if;
            if Attr_Count /= 0 then
               for I in 0 .. Attr_Count - 1 loop
                  Handler.Collect_Text (" ");
                  Handler.Collect_Text (Get_Qname (Atts, I));
                  Handler.Collect_Text ("=""");

                  declare
                     Value : constant String := Get_Value (Atts, I);
                  begin
                     Handler.Collect_Text (Value);
                  end;
                  Handler.Collect_Text ("""");
               end loop;
            end if;
            if Handler.Escape_Unknown_Tags and then Is_Unknown then
               Handler.Collect_Text ("&gt;");
--            else
--               Handler.Collect_Text (">");
            end if;
         end;
      end if;
   end Start_Element;

   function Is_Self_Closing (Tag : in String) return Boolean is
   begin
      case Tag (Tag'First) is
         when 'b' =>
            return Tag = "br";
         when 'h' =>
            return Tag = "hr";
         when 'm' =>
            return Tag = "meta";
         when 'i' =>
            return Tag = "img";
         when 'l' =>
            return Tag = "link";
         when others =>
            return False;
      end case;
   end Is_Self_Closing;

   --  ------------------------------
   --  End_Element
   --  ------------------------------
   overriding
   procedure End_Element (Handler       : in out Xhtml_Reader;
                          Namespace_URI : in Unicode.CES.Byte_Sequence := "";
                          Local_Name    : in Unicode.CES.Byte_Sequence := "";
                          Qname         : in Unicode.CES.Byte_Sequence := "") is
      pragma Unreferenced (Local_Name);
   begin
      if Handler.Current.Parent = null then
         Finish_Text_Node (Handler);

      elsif not Handler.Current.Text then
         Finish_Text_Node (Handler);
         Handler.Current.Parent.Freeze;

      end if;
      if Handler.Current.Text or else Handler.Text /= null then
         declare
            Is_Unknown : constant Boolean := Namespace_URI /= "" and then Index (Qname, ":") > 0;
         begin
            --  Optimization: we know in which state we are.
            Handler.State := HAS_CONTENT;
            if Handler.Escape_Unknown_Tags and then Is_Unknown then
               Handler.Collect_Text ("&lt;/");
               Handler.Collect_Text (Qname);
               Handler.Collect_Text ("&gt;");
            elsif not Handler.Current.Has_Content and then Is_Self_Closing (Qname) then
               Handler.Collect_Text (" />");
            else
               if not Handler.Current.Has_Content then
                  Handler.Collect_Text ("></");
               else
                  Handler.Collect_Text ("</");
               end if;
               Handler.Collect_Text (Qname);
               Handler.Collect_Text (">");
            end if;
         end;
      else
         Handler.Spaces := Null_Unbounded_String;
      end if;

      --  Pop the current context to restore the last context.
      Pop (Handler);
   end End_Element;

   --  ------------------------------
   --  Characters
   --  ------------------------------
   overriding
   procedure Characters (Handler : in out Xhtml_Reader;
                         Ch      : in Unicode.CES.Byte_Sequence) is
   begin
      if not Handler.Current.Has_Content then
         Handler.Current.Has_Content := True;
         Handler.Collect_Text (">");
      end if;
      Collect_Text (Handler, Ch);
   end Characters;

   --  ------------------------------
   --  Ignorable_Whitespace
   --  ------------------------------
   overriding
   procedure Ignorable_Whitespace (Handler : in out Xhtml_Reader;
                                   Ch      : in Unicode.CES.Byte_Sequence) is
   begin
      if not Handler.Ignore_White_Spaces then
         if not Handler.Current.Has_Content then
            Handler.Current.Has_Content := True;
            Handler.Collect_Text (">");
         end if;
         Collect_Text (Handler, Ch);
      end if;
   end Ignorable_Whitespace;

   --  ------------------------------
   --  Processing_Instruction
   --  ------------------------------
   overriding
   procedure Processing_Instruction (Handler : in out Xhtml_Reader;
                                     Target  : in Unicode.CES.Byte_Sequence;
                                     Data    : in Unicode.CES.Byte_Sequence) is
      pragma Unreferenced (Handler);
   begin
      Log.Error ("Processing instruction: {0}: {1}", Target, Data);
      null;
   end Processing_Instruction;

   --  ------------------------------
   --  Skipped_Entity
   --  ------------------------------
   overriding
   procedure Skipped_Entity (Handler : in out Xhtml_Reader;
                             Name    : in Unicode.CES.Byte_Sequence) is
      pragma Unmodified (Handler);
   begin
      null;
   end Skipped_Entity;

   --  ------------------------------
   --  Start_Cdata
   --  ------------------------------
   overriding
   procedure Start_Cdata (Handler : in out Xhtml_Reader) is
      pragma Unreferenced (Handler);
   begin
      Log.Info ("Start CDATA");
   end Start_Cdata;

   --  ------------------------------
   --  End_Cdata
   --  ------------------------------
   overriding
   procedure End_Cdata (Handler : in out Xhtml_Reader) is
      pragma Unreferenced (Handler);
   begin
      Log.Info ("End CDATA");
   end End_Cdata;

   --  ------------------------------
   --  Resolve_Entity
   --  ------------------------------
   overriding
   function Resolve_Entity (Handler   : Xhtml_Reader;
                            Public_Id : Unicode.CES.Byte_Sequence;
                            System_Id : Unicode.CES.Byte_Sequence)
                            return Input_Sources.Input_Source_Access is
      pragma Unreferenced (Handler);
   begin
      Log.Error ("Cannot resolve entity {0} - {1}", Public_Id, System_Id);
      return null;
   end Resolve_Entity;

   overriding
   procedure Start_DTD (Handler   : in out Xhtml_Reader;
                        Name      : Unicode.CES.Byte_Sequence;
                        Public_Id : Unicode.CES.Byte_Sequence := "";
                        System_Id : Unicode.CES.Byte_Sequence := "") is
   begin
      if Handler.Text = null then
         Handler.Text := new Text_Tag_Node;
         Initialize (Handler.Text.all'Access, Null_Binding,
                     Handler.Line, Handler.Current.Parent, null);
         Handler.Text.Last := Handler.Text.Content'Access;
      end if;
      declare
         Content : constant Tag_Content_Access := Handler.Text.Last;
      begin
         Append (Content.Text, "<!DOCTYPE ");
         Append (Content.Text, Name);
         Append (Content.Text, " ");
         if Public_Id'Length > 0 then
            Append (Content.Text, " PUBLIC """);
            Append (Content.Text, Public_Id);
            Append (Content.Text, """ ");
            if System_Id'Length > 0 then
               Append (Content.Text, '"');
               Append (Content.Text, System_Id);
               Append (Content.Text, '"');
            end if;
         elsif System_Id'Length > 0 then
            Append (Content.Text, " SYSTEM """);
            Append (Content.Text, System_Id);
            Append (Content.Text, """ ");
         end if;
         Append (Content.Text, " >" & ASCII.LF);
      end;
   end Start_DTD;

   --  ------------------------------
   --  Get the root node that was created upon parsing of the XHTML file.
   --  ------------------------------
   function Get_Root (Reader : Xhtml_Reader) return Tag_Node_Access is
   begin
      return Reader.Root;
   end Get_Root;

   --  ------------------------------
   --  Set the XHTML reader to ignore or not the white spaces.
   --  When set to True, the ignorable white spaces will not be kept.
   --  ------------------------------
   procedure Set_Ignore_White_Spaces (Reader : in out Xhtml_Reader;
                                      Value  : in Boolean) is
   begin
      Reader.Ignore_White_Spaces := Value;
   end Set_Ignore_White_Spaces;

   --  ------------------------------
   --  Set the XHTML reader to ignore empty lines.
   --  ------------------------------
   procedure Set_Ignore_Empty_Lines (Reader : in out Xhtml_Reader;
                                     Value  : in Boolean) is
   begin
      Reader.Ignore_Empty_Lines := Value;
   end Set_Ignore_Empty_Lines;

   --  ------------------------------
   --  Set the XHTML reader to escape or not the unknown tags.
   --  When set to True, the tags which are not recognized will be
   --  emitted as a raw text component and they will be escaped using
   --  the XML escape rules.
   --  ------------------------------
   procedure Set_Escape_Unknown_Tags (Reader : in out Xhtml_Reader;
                                      Value  : in Boolean) is
   begin
      Reader.Escape_Unknown_Tags := Value;
   end Set_Escape_Unknown_Tags;

   --  ------------------------------
   --  Parse an XML stream, and calls the appropriate SAX callbacks for each
   --  event.
   --  This is not re-entrant: you can not call Parse with the same Parser
   --  argument in one of the SAX callbacks. This has undefined behavior.
   --  ------------------------------
   procedure Parse (Parser  : in out Xhtml_Reader;
                    Name    : in ASF.Views.File_Info_Access;
                    Input   : in out Input_Sources.Input_Source'Class;
                    Factory : access ASF.Factory.Component_Factory;
                    Context : in EL.Contexts.ELContext_Access) is
   begin
      Parser.Stack_Pos := 1;
      Push (Parser);
      Parser.Line.File := Name;
      Parser.Root   := new Tag_Node;
      Parser.Functions.Factory := Factory;
      Parser.Current.Parent := Parser.Root;
      Parser.Current.Has_Content := True;
      Parser.ELContext := Parser.Context'Unchecked_Access;
      Parser.Context.Set_Function_Mapper (Parser.Functions'Unchecked_Access);
      Parser.Functions.Mapper := Context.Get_Function_Mapper;
      if Parser.Functions.Mapper = null then
         Log.Warn ("There is no function mapper");
      end if;
      Sax.Readers.Reader (Parser).Parse (Input);
      Finish_Text_Node (Parser);
      Parser.Functions.Factory := null;
      Parser.ELContext := null;

      if Parser.Ignore_Empty_Lines then
         Parser.Default_State := NO_CONTENT;
      else
         Parser.Default_State := HAS_CONTENT;
      end if;
      Parser.State := Parser.Default_State;
      Free (Parser.Stack);

   exception
      when others =>
         Free (Parser.Stack);
         raise;
   end Parse;

end ASF.Views.Nodes.Reader;
