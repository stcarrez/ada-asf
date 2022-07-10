-----------------------------------------------------------------------
--  asf -- XHTML Reader
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2017, 2018, 2022 Stephane Carrez
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

with Sax.Exceptions;
with Sax.Locators;
with Sax.Readers;
with Sax.Attributes;
with Unicode.CES;
with Input_Sources;
with EL.Contexts;
with EL.Contexts.Default;
with ASF.Factory;

private with EL.Functions;
private with Util.Strings.Maps;
package ASF.Views.Nodes.Reader is

   Parsing_Error : exception;

   type Xhtml_Reader is new Sax.Readers.Reader with private;

   overriding
   procedure Warning (Handler : in out Xhtml_Reader;
                      Except  : in Sax.Exceptions.Sax_Parse_Exception'Class);

   overriding
   procedure Error (Handler : in out Xhtml_Reader;
                    Except  : in Sax.Exceptions.Sax_Parse_Exception'Class);

   overriding
   procedure Fatal_Error (Handler : in out Xhtml_Reader;
                          Except  : in Sax.Exceptions.Sax_Parse_Exception'Class);

   overriding
   procedure Set_Document_Locator (Handler : in out Xhtml_Reader;
                                   Loc     : in out Sax.Locators.Locator);

   overriding
   procedure Start_Document (Handler : in out Xhtml_Reader);

   overriding
   procedure End_Document (Handler : in out Xhtml_Reader);

   overriding
   procedure Start_Prefix_Mapping (Handler : in out Xhtml_Reader;
                                   Prefix  : in Unicode.CES.Byte_Sequence;
                                   URI     : in Unicode.CES.Byte_Sequence);

   overriding
   procedure End_Prefix_Mapping (Handler : in out Xhtml_Reader;
                                 Prefix  : in Unicode.CES.Byte_Sequence);

   overriding
   procedure Start_Element (Handler       : in out Xhtml_Reader;
                            Namespace_URI : in Unicode.CES.Byte_Sequence := "";
                            Local_Name    : in Unicode.CES.Byte_Sequence := "";
                            Qname         : in Unicode.CES.Byte_Sequence := "";
                            Atts          : in Sax.Attributes.Attributes'Class);

   overriding
   procedure End_Element (Handler       : in out Xhtml_Reader;
                          Namespace_URI : in Unicode.CES.Byte_Sequence := "";
                          Local_Name    : in Unicode.CES.Byte_Sequence := "";
                          Qname         : in Unicode.CES.Byte_Sequence := "");

   overriding
   procedure Characters (Handler : in out Xhtml_Reader;
                         Ch      : in Unicode.CES.Byte_Sequence);

   overriding
   procedure Ignorable_Whitespace (Handler : in out Xhtml_Reader;
                                   Ch      : in Unicode.CES.Byte_Sequence);

   overriding
   procedure Processing_Instruction (Handler : in out Xhtml_Reader;
                                     Target  : in Unicode.CES.Byte_Sequence;
                                     Data    : in Unicode.CES.Byte_Sequence);

   overriding
   procedure Skipped_Entity (Handler : in out Xhtml_Reader;
                             Name    : in Unicode.CES.Byte_Sequence);

   overriding
   procedure Start_Cdata (Handler : in out Xhtml_Reader);

   overriding
   procedure End_Cdata (Handler : in out Xhtml_Reader);

   overriding
   function Resolve_Entity (Handler   : Xhtml_Reader;
                            Public_Id : Unicode.CES.Byte_Sequence;
                            System_Id : Unicode.CES.Byte_Sequence)
                            return Input_Sources.Input_Source_Access;

   overriding
   procedure Start_DTD (Handler   : in out Xhtml_Reader;
                        Name      : Unicode.CES.Byte_Sequence;
                        Public_Id : Unicode.CES.Byte_Sequence := "";
                        System_Id : Unicode.CES.Byte_Sequence := "");

   --  Get the root node that was created upon parsing of the XHTML file.
   function Get_Root (Reader : Xhtml_Reader) return Tag_Node_Access;

   --  Set the XHTML reader to ignore or not the white spaces.
   --  When set to True, the ignorable white spaces will not be kept.
   procedure Set_Ignore_White_Spaces (Reader : in out Xhtml_Reader;
                                      Value  : in Boolean);

   --  Set the XHTML reader to escape or not the unknown tags.
   --  When set to True, the tags which are not recognized will be
   --  emitted as a raw text component and they will be escaped using
   --  the XML escape rules.
   procedure Set_Escape_Unknown_Tags (Reader : in out Xhtml_Reader;
                                      Value  : in Boolean);

   --  Set the XHTML reader to ignore empty lines.
   procedure Set_Ignore_Empty_Lines (Reader : in out Xhtml_Reader;
                                     Value  : in Boolean);

   --  Parse an XML stream, and calls the appropriate SAX callbacks for each
   --  event.
   --  This is not re-entrant: you can not call Parse with the same Parser
   --  argument in one of the SAX callbacks. This has undefined behavior.
   procedure Parse (Parser  : in out Xhtml_Reader;
                    Name    : in ASF.Views.File_Info_Access;
                    Input   : in out Input_Sources.Input_Source'Class;
                    Factory : access ASF.Factory.Component_Factory;
                    Context : in EL.Contexts.ELContext_Access);

private

   --  Collect the text for an EL expression.  The EL expression starts
   --  with either '#{' or with '${' and ends with the matching '}'.
   --  If the <b>Value</b> string does not contain the whole EL expression
   --  the <b>Expr_Buffer</b> stored in the reader is used to collect
   --  that expression.
   procedure Collect_Expression (Handler  : in out Xhtml_Reader);

   --  Collect the raw-text in a buffer.  The text must be flushed
   --  when a new element is started or when an exiting element is closed.
   procedure Collect_Text (Handler : in out Xhtml_Reader;
                           Value   : in Unicode.CES.Byte_Sequence);

   use EL.Functions;
   --  use ASF.Components.Factory;

   package NS_Mapping renames Util.Strings.Maps;

   --  Skip indicates the number of frames to skip in the saved locations
   --  stack
   type NS_Function_Mapper is new Function_Mapper with record
      Mapping : NS_Mapping.Map;
      Mapper  : Function_Mapper_Access;
      Factory : access ASF.Factory.Component_Factory;
   end record;

   --  Find the function knowing its name.
   overriding
   function Get_Function (Mapper    : NS_Function_Mapper;
                          Namespace : String;
                          Name      : String) return Function_Access;

   --  Bind a name to a function in the given namespace.
   overriding
   procedure Set_Function (Mapper    : in out NS_Function_Mapper;
                           Namespace : in String;
                           Name      : in String;
                           Func      : in Function_Access);

   --  Find the create function in bound to the name in the given namespace.
   --  Returns null if no such binding exist.
   function Find (Mapper    : in NS_Function_Mapper;
                  Namespace : in String;
                  Name      : in String) return ASF.Views.Nodes.Binding_Type;

   procedure Set_Namespace (Mapper : in out NS_Function_Mapper;
                            Prefix : in String;
                            URI    : in String);

   --  Remove the namespace prefix binding.
   procedure Remove_Namespace (Mapper : in out NS_Function_Mapper;
                               Prefix : in String);

   type Element_Context is record
      Parent      : Tag_Node_Access;
      Text        : Boolean;
      Has_Content : Boolean;
   end record;
   type Element_Context_Access is access all Element_Context;
   type Element_Context_Array is array (Natural range <>) of aliased Element_Context;
   type Element_Context_Array_Access is access all Element_Context_Array;

   type Text_State is (NO_CONTENT, HAS_CONTENT, PARSE_EXPR);

   type Xhtml_Reader is new Sax.Readers.Reader with record
      Locator    : Sax.Locators.Locator;
      Root       : Tag_Node_Access;
      Text       : Text_Tag_Node_Access;
      Current    : Element_Context_Access;
      ELContext  : EL.Contexts.ELContext_Access;
      Functions  : aliased NS_Function_Mapper;
      Context    : aliased EL.Contexts.Default.Default_Context;
      Stack      : Element_Context_Array_Access;
      Stack_Pos  : Natural := 0;

      --  The line and file information.
      Line                : Line_Info;

      State         : Text_State := NO_CONTENT;
      Default_State : Text_State := NO_CONTENT;

      --  Current expression buffer (See Collect_Expression)
      Expr_Buffer : Unbounded_String;

      --  Some pending white spaces to append to the current text.
      Spaces      : Unbounded_String;

      --  When not empty, the 'xmlns' attribute to insert in the element.  The XML Sax parser
      --  notifies us about 'xmlns' attributes through the Start_Prefix_Mapping operation.
      --  When the default namespace with empty prefix is found, we have to add the corresponding
      --  attribute in Start_Element so that it is written in the facelet tree.
      Add_NS      : Unbounded_String;

      --  Whether the unknown tags are escaped using XML escape rules.
      Escape_Unknown_Tags : Boolean := True;

      --  Whether white spaces can be ignored.
      Ignore_White_Spaces : Boolean := True;

      --  Whether empty lines should be ignored (when white spaces are kept).
      Ignore_Empty_Lines  : Boolean := True;

   end record;

end ASF.Views.Nodes.Reader;
