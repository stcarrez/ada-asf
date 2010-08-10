-----------------------------------------------------------------------
--  asf -- XHTML Reader
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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Unicode;

with Ada.Exceptions;
with Ada.Strings.Fixed;
with ASF.Views.Nodes.Factory;
with Util.Log.Loggers;
package body ASF.Views.Nodes.Reader is

   use Util.Log;
   use Sax.Readers;
   use Sax.Exceptions;
   use Sax.Locators;
   use Sax.Attributes;
   use Unicode;
   use Unicode.CES;
   use Ada.Strings.Fixed;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Views.Nodes.Reader");

   procedure Free is
     new Ada.Unchecked_Deallocation (Element_Context_Array,
                                     Element_Context_Array_Access);

   procedure Push (Handler : in out Xhtml_Reader'Class);
   procedure Pop (Handler  : in out Xhtml_Reader'Class);

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

      NS  : Unbounded_String := To_Unbounded_String (Namespace);
      Pos : constant NS_Mapping.Cursor := NS_Mapping.Find (Mapper.Mapping, NS);
   begin
      if Has_Element (Pos) then
         NS := Element (Pos);
         return Mapper.Mapper.Get_Function (To_String (NS), Name);
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
                  Name      : String) return ASF.Factory.Binding is
      use NS_Mapping;
   begin
      return ASF.Factory.Find (Mapper.Factory.all, Namespace, Name);
   end Find;

   procedure Set_Namespace (Mapper : in out NS_Function_Mapper;
                            Prefix : in String;
                            URI    : in String) is
      use NS_Mapping;
   begin
      Log.Debug ("Add namespace {0}:{1}", Prefix, URI);

      Mapper.Mapping.Include (To_Unbounded_String (Prefix),
                              To_Unbounded_String (URI));
   end Set_Namespace;

   --  ------------------------------
   --  Remove the namespace prefix binding.
   --  ------------------------------
   procedure Remove_Namespace (Mapper : in out NS_Function_Mapper;
                               Prefix : in String) is
      use NS_Mapping;
      NS  : constant Unbounded_String := To_Unbounded_String (Prefix);
      Pos : NS_Mapping.Cursor := NS_Mapping.Find (Mapper.Mapping, NS);
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
      Log.Warn ("{0}: {1}", To_String (Get_Locator (Except)), Get_Message (Except));
   end Warning;

   --  ------------------------------
   --  Error
   --  ------------------------------
   overriding
   procedure Error (Handler : in out Xhtml_Reader;
                    Except  : in Sax.Exceptions.Sax_Parse_Exception'Class) is
      pragma Warnings (Off, Handler);
   begin
      Log.Error ("{0}: {1}", To_String (Get_Locator (Except)), Get_Message (Except));
   end Error;

   --  ------------------------------
   --  Fatal_Error
   --  ------------------------------
   overriding
   procedure Fatal_Error (Handler : in out Xhtml_Reader;
                          Except  : in Sax.Exceptions.Sax_Parse_Exception'Class) is
      pragma Unreferenced (Handler);
   begin
      Log.Error ("{0}: {1}", To_String (Get_Locator (Except)), Get_Message (Except));
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
      Handler.Functions.Set_Namespace (Prefix => Prefix, URI => URI);
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
   --  Collect the text defined in <b>Value</b> into the content node
   --  pointed to by <b>Content</b>
   --  ------------------------------
   procedure Collect_Text (Handler : in out Xhtml_Reader;
                           Content : in out Tag_Content_Access;
                           Value   : in String) is
      use Ada.Exceptions;

      Pos   : Natural := Value'First;
      Last  : Natural := Value'First;
      N     : Natural;
      Last_Pos : Natural;
   begin
      while Pos <= Value'Last loop
         N := Index (Value, "#{", Pos);
         if N = 0 then
            N := Index (Value, "${", Pos);
         end if;
         if N = 0 then
            Last := Value'Last;
         else
            Last := N - 1;
         end if;
         if N /= Pos then
            Append (Content.Text, Value (Pos .. Last));
         end if;
         if N /= 0 then
            Last_Pos := Index (Value, "}", N + 2);
            if Last_Pos = 0 then
               Last_Pos := Value'Last;
            end if;

            --  Append (Content.Text, Value (Pos .. N - 1));
            begin
               Content.Expr := EL.Expressions.Create_Expression
                 (Value (N .. Last_Pos), Handler.ELContext.all);
            exception
               when E : EL.Functions.No_Function | EL.Expressions.Invalid_Expression =>
                  Log.Error ("{0}: Invalid expression: {1}",
                             To_String (Handler.Locator),
                             Exception_Message (E));
                  Log.Error ("{0}: {1}",
                             To_String (Handler.Locator),
                             Value (N .. Last_Pos));

               when E : others =>
                  Log.Error ("{0}: Internal error: {1}:{2}",
                             To_String (Handler.Locator),
                             Exception_Name (E),
                             Exception_Message (E));
            end;
            Content.Next := new Tag_Content;
            Content := Content.Next;
            Handler.Text.Last := Content;
            Last := Last_Pos;
         end if;
         Pos := Last + 1;
      end loop;
   end Collect_Text;

   --  ------------------------------
   --  Start_Element
   --  ------------------------------
   overriding
   procedure Start_Element (Handler       : in out Xhtml_Reader;
                            Namespace_URI : in Unicode.CES.Byte_Sequence := "";
                            Local_Name    : in Unicode.CES.Byte_Sequence := "";
                            Qname         : in Unicode.CES.Byte_Sequence := "";
                            Atts          : in Sax.Attributes.Attributes'Class) is

      use ASF.Factory;
      use Ada.Exceptions;

      Attr_Count : Natural;
      Attributes : Tag_Attribute_Array_Access;
      Node       : Tag_Node_Access;
      Name       : constant Unbounded_String := To_Unbounded_String (Local_Name);
      Factory    : ASF.Factory.Binding;
   begin
      Handler.Line.Line := Sax.Locators.Get_Line_Number (Handler.Locator);
      Handler.Line.Column := Sax.Locators.Get_Column_Number (Handler.Locator);

      --  Push the current context to keep track where we are.
      Push (Handler);
      Attr_Count := Get_Length (Atts);
      begin
         Factory := Handler.Functions.Find (Namespace => Namespace_URI,
                                            Name      => Local_Name);
         Attributes := new Tag_Attribute_Array (1 .. Attr_Count);
         for I in 0 .. Attr_Count - 1 loop
            declare
               Attr  : constant Tag_Attribute_Access := Attributes (I + 1)'Access;
               Value : constant String := Get_Value (Atts, I);
               Expr  : EL.Expressions.ValueExpression_Access;
            begin
               Attr.Name  := To_Unbounded_String (Get_Qname (Atts, I));
               if Index (Value, "#{") > 0 or Index (Value, "${") > 0 then
                  begin
                     Expr := new EL.Expressions.ValueExpression;
                     Attr.Binding := Expr.all'Access;
                     Expr.all := EL.Expressions.Create_Expression
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
         Node := Factory.Tag (Name       => Name,
                              Line       => Handler.Line,
                              Parent     => Handler.Current.Parent,
                              Attributes => Attributes);
         Node.Factory   := Factory.Component;
         Handler.Current.Parent := Node;
         Handler.Current.Text   := False;
         Handler.Text   := null;

      exception
         when Unknown_Name =>

            if Handler.Text = null then
               Handler.Text := new Text_Tag_Node;
               Initialize (Handler.Text.all'Access, Null_Unbounded_String,
                           Handler.Line, Handler.Current.Parent, null);
               Handler.Text.Last := Handler.Text.Content'Access;
            end if;
            Handler.Current.Text := True;
            declare
               Content : Tag_Content_Access := Handler.Text.Last;
               Is_Unknown : constant Boolean := Namespace_URI /= "" and Index (Qname, ":") > 0;
            begin
               if Is_Unknown then
                  Log.Error ("{0}: Element '{1}' not found",
                             To_String (Handler.Locator), Qname);
               end if;
               if Handler.Escape_Unknown_Tags and Is_Unknown then
                  Append (Content.Text, "&lt;");
               else
                  Append (Content.Text, '<');
               end if;
               Append (Content.Text, Qname);
               if Attr_Count /= 0 then
                  for I in 0 .. Attr_Count - 1 loop
                     Append (Content.Text, ' ');
                     Append (Content.Text, Get_Qname (Atts, I));
                     Append (Content.Text, '=');
                     Append (Content.Text, '"');

                     declare
                        Value : constant String := Get_Value (Atts, I);
                     begin
                        Collect_Text (Handler, Content, Value);
                     end;
                     Append (Content.Text, '"');
                  end loop;
               end if;
               if Handler.Escape_Unknown_Tags and Is_Unknown then
                  Append (Content.Text, "&gt;");
               else
                  Append (Content.Text, '>');
               end if;
            end;
      end;
   end Start_Element;

   --  ------------------------------
   --  End_Element
   --  ------------------------------
   overriding
   procedure End_Element (Handler       : in out Xhtml_Reader;
                          Namespace_URI : in Unicode.CES.Byte_Sequence := "";
                          Local_Name    : in Unicode.CES.Byte_Sequence := "";
                          Qname         : in Unicode.CES.Byte_Sequence := "") is
   begin
      if Handler.Current.Parent = null then
         Handler.Text := null;

      elsif not Handler.Current.Text then
         Handler.Text := null;
         Handler.Current.Parent.Freeze;

      elsif Handler.Current.Text and Handler.Text = null then
         Handler.Text := new Text_Tag_Node;
         Initialize (Handler.Text.all'Access, Null_Unbounded_String,
                     Handler.Line, Handler.Current.Parent, null);
         Handler.Text.Last := Handler.Text.Content'Access;
      end if;
      if Handler.Text /= null then
         declare
            Content    : constant Tag_Content_Access := Handler.Text.Last;
            Is_Unknown : constant Boolean := Namespace_URI /= "" and Index (Qname, ":") > 0;
         begin
            if Handler.Escape_Unknown_Tags and Is_Unknown then
               Append (Content.Text, "&lt;/");
               Append (Content.Text, Qname);
               Append (Content.Text, "&gt;");
            else
               Append (Content.Text, "</");
               Append (Content.Text, Qname);
               Append (Content.Text, '>');
            end if;
         end;
      end if;

      --  Pop the current context to restor the last context.
      Pop (Handler);
   end End_Element;

   --  ------------------------------
   --  Characters
   --  ------------------------------
   overriding
   procedure Characters (Handler : in out Xhtml_Reader;
                         Ch      : in Unicode.CES.Byte_Sequence) is
   begin
      if Handler.Text = null then
         Handler.Text := new Text_Tag_Node;
         Initialize (Handler.Text.all'Access, Null_Unbounded_String,
                     Handler.Line, Handler.Current.Parent, null);
         Handler.Text.Last := Handler.Text.Content'Access;
      end if;
      declare
         Content : Tag_Content_Access := Handler.Text.Last;
      begin
         Collect_Text (Handler, Content, Ch);
      end;
   end Characters;

   --  ------------------------------
   --  Ignorable_Whitespace
   --  ------------------------------
   overriding
   procedure Ignorable_Whitespace (Handler : in out Xhtml_Reader;
                                   Ch      : in Unicode.CES.Byte_Sequence) is
      pragma Unmodified (Handler);
   begin
      if not Handler.Ignore_White_Spaces then
         Characters (Handler, Ch);
      end if;
   end Ignorable_Whitespace;

   --  ------------------------------
   --  Processing_Instruction
   --  ------------------------------
   overriding
   procedure Processing_Instruction (Handler : in out Xhtml_Reader;
                                     Target  : in Unicode.CES.Byte_Sequence;
                                     Data    : in Unicode.CES.Byte_Sequence) is
      pragma Unmodified (Handler);
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
      pragma Unmodified (Handler);
   begin
      Log.Info ("Start CDATA");
   end Start_Cdata;

   --  ------------------------------
   --  End_Cdata
   --  ------------------------------
   overriding
   procedure End_Cdata (Handler : in out Xhtml_Reader) is
      pragma Unmodified (Handler);
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
         Initialize (Handler.Text.all'Access, Null_Unbounded_String,
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
                    Name    : in Util.Strings.Name_Access;
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
      Parser.ELContext := Parser.Context'Unchecked_Access;
      Parser.Context.Set_Function_Mapper (Parser.Functions'Unchecked_Access);
      Parser.Functions.Mapper := Context.Get_Function_Mapper;
      Sax.Readers.Reader (Parser).Parse (Input);
      Parser.Functions.Factory := null;
      Parser.ELContext := null;
      Free (Parser.Stack);
   end Parse;

end ASF.Views.Nodes.Reader;
