-----------------------------------------------------------------------
--  nodes-core -- Core nodes
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2018, 2022 Stephane Carrez
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
with Util.Files;

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Fixed;
package body ASF.Views.Nodes.Core is

   --  ------------------------------
   --  Set Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Set Tag
   --  ------------------------------
   function Create_Set_Tag_Node (Binding    : in Binding_Type;
                                 Line       : in Line_Info;
                                 Parent     : in Tag_Node_Access;
                                 Attributes : in Tag_Attribute_Array_Access)
                                 return Tag_Node_Access is
      Node : constant Set_Tag_Node_Access := new Set_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Value      := Find_Attribute (Attributes, "value");
      Node.Var        := Find_Attribute (Attributes, "var");
      if Node.Value = null then
         Node.Error ("Missing 'value' attribute");
      end if;
      if Node.Var = null then
         Node.Error ("Missing 'var' attribute");
      end if;
      return Node.all'Access;
   end Create_Set_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Set_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
      pragma Unreferenced (Parent);
   begin
      if Node.Value /= null then
         declare
            Value  : constant EL.Expressions.Expression := Get_Expression (Node.Value.all);
         begin
            Context.Set_Variable (Node.Var.Value, Value);
         end;
      end if;
   end Build_Components;

   --  ------------------------------
   --  If Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the If Tag
   --  ------------------------------
   function Create_If_Tag_Node (Binding    : in Binding_Type;
                                Line       : in Line_Info;
                                Parent     : in Tag_Node_Access;
                                Attributes : in Tag_Attribute_Array_Access)
                                return Tag_Node_Access is
      Node : constant If_Tag_Node_Access := new If_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Condition  := Find_Attribute (Attributes, "test");
      Node.Var        := Find_Attribute (Attributes, "var");
      if Node.Condition = null then
         Node.Error ("Missing 'test' attribute");
      end if;
      return Node.all'Access;
   end Create_If_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access If_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
   begin
      if Node.Condition /= null then
         declare
            Value : constant EL.Objects.Object := Get_Value (Node.Condition.all, Context);
         begin
            if Node.Var /= null then
               Context.Set_Attribute (Node.Var.Value, Value);
            end if;
            if EL.Objects.To_Boolean (Value) then
               Tag_Node (Node.all).Build_Children (Parent, Context);
            end if;
         end;
      end if;
   end Build_Components;

   --  ------------------------------
   --  Choose Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Choose Tag
   --  ------------------------------
   function Create_Choose_Tag_Node (Binding    : in Binding_Type;
                                    Line       : in Line_Info;
                                    Parent     : in Tag_Node_Access;
                                    Attributes : in Tag_Attribute_Array_Access)
                                    return Tag_Node_Access is
      Node : constant Choose_Tag_Node_Access := new Choose_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      return Node.all'Access;
   end Create_Choose_Tag_Node;

   --  ------------------------------
   --  Freeze the tag node tree and perform any initialization steps
   --  necessary to build the components efficiently.
   --  Prepare the evaluation of choices by identifying the <c:when> and
   --  <c:otherwise> conditions.
   --  ------------------------------
   overriding
   procedure Freeze (Node : access Choose_Tag_Node) is
      Child  : Tag_Node_Access := Node.First_Child;
      Choice : When_Tag_Node_Access := null;
   begin
      while Child /= null loop
         if Child.all in Otherwise_Tag_Node'Class then
            Node.Otherwise := Child;

         elsif Child.all in When_Tag_Node'Class then
            if Choice = null then
               Node.Choices := When_Tag_Node (Child.all)'Access;
            else
               Choice.Next_Choice := When_Tag_Node (Child.all)'Access;
            end if;
            Choice := When_Tag_Node (Child.all)'Access;

         else
            null;
            --  @todo: report a warning in a log
            --  @todo: clean the sub-tree and remove what is not necessary
         end if;
         Child := Child.Next;
      end loop;
   end Freeze;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Choose_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
      Choice : When_Tag_Node_Access := Node.Choices;
   begin
      --  Evaluate the choices and stop at the first which succeeds
      while Choice /= null loop
         if Choice.Is_Selected (Context) then
            Choice.Build_Children (Parent, Context);
            return;
         end if;
         Choice := Choice.Next_Choice;
      end loop;

      --  No choice matched, build the otherwise clause.
      if Node.Otherwise /= null then
         Node.Otherwise.Build_Children (Parent, Context);
      end if;
   end Build_Components;

   --  ------------------------------
   --  Create the When Tag
   --  ------------------------------
   function Create_When_Tag_Node (Binding    : in Binding_Type;
                                  Line       : in Line_Info;
                                  Parent     : in Tag_Node_Access;
                                  Attributes : in Tag_Attribute_Array_Access)
                                  return Tag_Node_Access is
      Node : constant When_Tag_Node_Access := new When_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Condition  := Find_Attribute (Attributes, "test");
      if Node.Condition = null then
         Node.Error ("Missing 'test' attribute");
      end if;
      --        Node.Var        := Find_Attribute (Attributes, "var");
      return Node.all'Access;
   end Create_When_Tag_Node;

   --  ------------------------------
   --  Check whether the node condition is selected.
   --  ------------------------------
   function Is_Selected (Node    : When_Tag_Node;
                         Context : Facelet_Context'Class) return Boolean is
   begin
      if Node.Condition = null then
         return False;
      else
         return EL.Objects.To_Boolean (Get_Value (Node.Condition.all, Context));
      end if;

   exception
      when E : others =>
            Node.Error ("Exception: {0}", Ada.Exceptions.Exception_Message (E));
            return False;
   end Is_Selected;

   --  ------------------------------
   --  Create the Otherwise Tag
   --  ------------------------------
   function Create_Otherwise_Tag_Node (Binding    : in Binding_Type;
                                       Line       : in Line_Info;
                                       Parent     : in Tag_Node_Access;
                                       Attributes : in Tag_Attribute_Array_Access)
                                       return Tag_Node_Access is
      Node : constant Otherwise_Tag_Node_Access := new Otherwise_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      return Node.all'Access;
   end Create_Otherwise_Tag_Node;

   --  Tag names
   CHOOSE_TAG    : aliased constant String := "choose";
   IF_TAG        : aliased constant String := "if";
   OTHERWISE_TAG : aliased constant String := "otherwise";
   SET_TAG       : aliased constant String := "set";
   WHEN_TAG      : aliased constant String := "when";

   --  Name-space URI.  Use the JSTL name-space to make the XHTML views compatible
   --  the JSF.
   URI           : aliased constant String := "http://java.sun.com/jstl/core";

   --  ------------------------------
   --  Tag factory for nodes defined in this package.
   --  ------------------------------
   --  Register the facelets component factory.
   procedure Register (Factory : in out ASF.Factory.Component_Factory) is
   begin
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => CHOOSE_TAG'Access,
                            Tag    => Create_Choose_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => IF_TAG'Access,
                            Tag    => Create_If_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => OTHERWISE_TAG'Access,
                            Tag    => Create_Otherwise_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => SET_TAG'Access,
                            Tag    => Create_Set_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => WHEN_TAG'Access,
                            Tag    => Create_When_Tag_Node'Access,
                            Create => null);
   end Register;

   --  Function names
   CAPITALIZE_FN       : aliased constant String := "capitalize";
   COMPOSE_PATH_FN     : aliased constant String := "composePath";
   CONTAINS_FN         : aliased constant String := "contains";
   ENDS_WITH_FN        : aliased constant String := "endsWith";
   LENGTH_FN           : aliased constant String := "length";
   TO_UPPER_CASE_FN    : aliased constant String := "toUpperCase";
   TO_LOWER_CASE_FN    : aliased constant String := "toLowerCase";
   STARTS_WITH_FN      : aliased constant String := "startsWith";
   SUBSTRING_FN        : aliased constant String := "substring";
   SUBSTRING_AFTER_FN  : aliased constant String := "substringAfter";
   SUBSTRING_BEFORE_FN : aliased constant String := "substringBefore";
   TRIM_FN             : aliased constant String := "trim";
   INDEX_OF_FN         : aliased constant String := "indexOf";
   ESCAPE_XML_FN       : aliased constant String := "escapeXml";
   REPLACE_FN          : aliased constant String := "replace";

   --  JSFL function not implemented
   --     CONTAINS_IGNORE_CASE_FN : aliased constant String := "containsIgnoreCase";
   --     JOIN_FN             : aliased constant String := "join";
   --     SPLIT_FN            : aliased constant String := "split";

   function Length (Value : in EL.Objects.Object) return EL.Objects.Object;
   function Contains (Value : in EL.Objects.Object;
                      Search : in EL.Objects.Object) return EL.Objects.Object;
   function Capitalize (Value : EL.Objects.Object) return EL.Objects.Object;
   function To_Upper_Case (Value : EL.Objects.Object) return EL.Objects.Object;
   function To_Lower_Case (Value : EL.Objects.Object) return EL.Objects.Object;
   function Trim (Value : in EL.Objects.Object) return EL.Objects.Object;
   function Ends_With (Value  : in EL.Objects.Object;
                       Search : in EL.Objects.Object) return EL.Objects.Object;
   function Starts_With (Value  : in EL.Objects.Object;
                         Search : in EL.Objects.Object) return EL.Objects.Object;
   function Index_Of (Value  : in EL.Objects.Object;
                      Search : in EL.Objects.Object) return EL.Objects.Object;

   function Substring_Before (Value : in EL.Objects.Object;
                              Token : in EL.Objects.Object) return EL.Objects.Object;
   function Substring_After (Value : in EL.Objects.Object;
                             Token : in EL.Objects.Object) return EL.Objects.Object;
   function Compose_Path (Paths : in EL.Objects.Object;
                          Dir   : in EL.Objects.Object) return EL.Objects.Object;
   function Index (Value  : in EL.Objects.Object;
                   Search : in EL.Objects.Object) return Natural;
   function Substring (Value  : in EL.Objects.Object;
                       Start  : in EL.Objects.Object;
                       Finish : in EL.Objects.Object) return EL.Objects.Object;

   --  Escapes characters that could be interpreted as XML markup.
   function Escape_Xml (Value : in EL.Objects.Object) return EL.Objects.Object;

   --  Returns a string resulting from replacing in an input string
   --  all occurrences of a "before" string into an "after" substring.
   function Replace (From, Before, After : in EL.Objects.Object) return EL.Objects.Object;

   --  ------------------------------
   --  Escapes characters that could be interpreted as XML markup.
   --  ------------------------------
   function Escape_Xml (Value : in EL.Objects.Object) return EL.Objects.Object is
      Of_Type : constant EL.Objects.Data_Type := EL.Objects.Get_Type (Value);
   begin
      case Of_Type is
         when EL.Objects.TYPE_STRING =>
            declare
               S : constant String := EL.Objects.To_String (Value);
            begin
               return EL.Objects.To_Object (Util.Strings.Transforms.Escape_Xml (S));
            end;

         when others =>
            return Value;

      end case;
   end Escape_Xml;

   --  ------------------------------
   --  Get the length of the object.
   --  ------------------------------
   function Length (Value : in EL.Objects.Object) return EL.Objects.Object is
      S : constant String := EL.Objects.To_String (Value);
   begin
      return EL.Objects.To_Object (Integer (S'Length));
   end Length;

   --  ------------------------------
   --  Find the index of the search string in the value.
   --  ------------------------------
   function Index (Value  : in EL.Objects.Object;
                   Search : in EL.Objects.Object) return Natural is
      Of_Type : constant EL.Objects.Data_Type := EL.Objects.Get_Type (Value);
   begin
      case Of_Type is
         when EL.Objects.TYPE_NULL =>
            return 0;

         when EL.Objects.TYPE_WIDE_STRING =>
            declare
               S : constant Wide_Wide_String := EL.Objects.To_Wide_Wide_String (Value);
               P : constant Wide_Wide_String := EL.Objects.To_Wide_Wide_String (Search);
            begin
               return Ada.Strings.Wide_Wide_Fixed.Index (S, P);
            end;

         when others =>
            declare
               S : constant String := EL.Objects.To_String (Value);
               P : constant String := EL.Objects.To_String (Search);
            begin
               return Ada.Strings.Fixed.Index (S, P);
            end;

      end case;
   end Index;

   --  ------------------------------
   --  Check if the search string is contained in the value.  If the value is a wide string,
   --  the search string is converted to a wide string and the search is made using wide string.
   --  Otherwise the value and search string are converted to a string.
   --  Returns true if the <b>Search</b> is contained in <b>Value</b>
   --  ------------------------------
   function Contains (Value  : in EL.Objects.Object;
                      Search : in EL.Objects.Object) return EL.Objects.Object is
   begin
      return EL.Objects.To_Object (Index (Value, Search) > 0);
   end Contains;

   --  ------------------------------
   --  Find the index of the search string in the value.
   --  Returns the index true if the <b>Search</b> is contained in <b>Value</b>
   --  ------------------------------
   function Index_Of (Value  : in EL.Objects.Object;
                      Search : in EL.Objects.Object) return EL.Objects.Object is
   begin
      return EL.Objects.To_Object (Index (Value, Search));
   end Index_Of;

   --  ------------------------------
   --  Check if the value starts with the given search string.
   --  Returns true if the <b>Value</b> starts with <b>Search</b>
   --  ------------------------------
   function Starts_With (Value  : in EL.Objects.Object;
                         Search : in EL.Objects.Object) return EL.Objects.Object is
      Of_Type : constant EL.Objects.Data_Type := EL.Objects.Get_Type (Value);
   begin
      case Of_Type is
         when EL.Objects.TYPE_NULL =>
            return EL.Objects.To_Object (False);

         when EL.Objects.TYPE_WIDE_STRING =>
            declare
               S : constant Wide_Wide_String := EL.Objects.To_Wide_Wide_String (Value);
               P : constant Wide_Wide_String := EL.Objects.To_Wide_Wide_String (Search);
            begin
               return EL.Objects.To_Object (Ada.Strings.Wide_Wide_Fixed.Index (S, P) = S'First);
            end;

         when others =>
            declare
               S : constant String := EL.Objects.To_String (Value);
               P : constant String := EL.Objects.To_String (Search);
            begin
               return EL.Objects.To_Object (Ada.Strings.Fixed.Index (S, P) = S'First);
            end;

      end case;
   end Starts_With;

   --  ------------------------------
   --  Check if the value ends with the given search string.
   --  Returns true if the <b>Value</b> starts with <b>Search</b>
   --  ------------------------------
   function Ends_With (Value  : in EL.Objects.Object;
                       Search : in EL.Objects.Object) return EL.Objects.Object is
      Of_Type : constant EL.Objects.Data_Type := EL.Objects.Get_Type (Value);
   begin
      case Of_Type is
         when EL.Objects.TYPE_NULL =>
            return EL.Objects.To_Object (False);

         when EL.Objects.TYPE_WIDE_STRING =>
            declare
               S : constant Wide_Wide_String := EL.Objects.To_Wide_Wide_String (Value);
               P : constant Wide_Wide_String := EL.Objects.To_Wide_Wide_String (Search);
               I : constant Natural := Ada.Strings.Wide_Wide_Fixed.Index (S, P);
            begin
               return EL.Objects.To_Object (I > 0 and then I + P'Length - 1 = S'Last);
            end;

         when others =>
            declare
               S : constant String := EL.Objects.To_String (Value);
               P : constant String := EL.Objects.To_String (Search);
               I : constant Natural := Ada.Strings.Fixed.Index (S, P);
            begin
               return EL.Objects.To_Object (I > 0 and then I + P'Length - 1 = S'Last);
            end;

      end case;
   end Ends_With;

   --  ------------------------------
   --  Returns the substring starting from the <b>Start</b> index up to the <b>Finish</b>
   --  index inclusive.
   --  ------------------------------
   function Substring (Value  : in EL.Objects.Object;
                       Start  : in EL.Objects.Object;
                       Finish : in EL.Objects.Object) return EL.Objects.Object is
      Of_Type : constant EL.Objects.Data_Type := EL.Objects.Get_Type (Value);
   begin
      case Of_Type is
         when EL.Objects.TYPE_NULL =>
            return Value;

         when EL.Objects.TYPE_WIDE_STRING =>
            declare
               S     : constant Wide_Wide_String := EL.Objects.To_Wide_Wide_String (Value);
               First : Natural := EL.Objects.To_Integer (Start);
               Last  : Natural := EL.Objects.To_Integer (Finish);
            begin
               if First <= S'First then
                  First := S'First;
               end if;
               if Last >= S'Last then
                  Last := S'Last;
               end if;
               return EL.Objects.To_Object (S (First .. Last));
            end;

         when others =>
            declare
               S : constant String := EL.Objects.To_String (Value);
               First : Natural := EL.Objects.To_Integer (Start);
               Last  : Natural := EL.Objects.To_Integer (Finish);
            begin
               if First <= S'First then
                  First := S'First;
               end if;
               if Last >= S'Last then
                  Last := S'Last;
               end if;
               return EL.Objects.To_Object (S (First .. Last));
            end;

      end case;

   exception
      when others =>
         return Value;
   end Substring;

   function Capitalize (Value : EL.Objects.Object) return EL.Objects.Object is
      S : constant String := EL.Objects.To_String (Value);
   begin
      return EL.Objects.To_Object (Util.Strings.Transforms.Capitalize (S));
   end Capitalize;

   function To_Upper_Case (Value : EL.Objects.Object) return EL.Objects.Object is
      S : constant String := EL.Objects.To_String (Value);
   begin
      return EL.Objects.To_Object (Util.Strings.Transforms.To_Upper_Case (S));
   end To_Upper_Case;

   function To_Lower_Case (Value : EL.Objects.Object) return EL.Objects.Object is
      S : constant String := EL.Objects.To_String (Value);
   begin
      return EL.Objects.To_Object (Util.Strings.Transforms.To_Lower_Case (S));
   end To_Lower_Case;

   --  ------------------------------
   --  Trim the white spaces at beginning and end of the string.
   --  ------------------------------
   function Trim (Value : in EL.Objects.Object) return EL.Objects.Object is
      Of_Type : constant EL.Objects.Data_Type := EL.Objects.Get_Type (Value);
   begin
      case Of_Type is
         when EL.Objects.TYPE_STRING =>
            declare
               S : constant String := EL.Objects.To_String (Value);
            begin
               return EL.Objects.To_Object (Ada.Strings.Fixed.Trim (S, Ada.Strings.Both));
            end;

         when EL.Objects.TYPE_WIDE_STRING =>
            declare
               S : constant Wide_Wide_String := EL.Objects.To_Wide_Wide_String (Value);
            begin
               return EL.Objects.To_Object
                 (Ada.Strings.Wide_Wide_Fixed.Trim (S, Ada.Strings.Both));
            end;

         when others =>
            return Value;
      end case;
   end Trim;

   --  ------------------------------
   --  Returns a string resulting from replacing in an input string
   --  all occurrences of a "before" string into an "after" substring.
   --  ------------------------------
   function Replace (From, Before, After : in EL.Objects.Object) return EL.Objects.Object is
      Of_Type : constant EL.Objects.Data_Type := EL.Objects.Get_Type (From);
   begin
      case Of_Type is
         when EL.Objects.TYPE_NULL =>
            return From;

         when others =>
            declare
               S : Unbounded_String := EL.Objects.To_Unbounded_String (From);
               B : constant String := EL.Objects.To_String (Before);
               A : constant String := EL.Objects.To_String (After);
               I : Natural := 1;
            begin
               loop
                  I := Ada.Strings.Unbounded.Index (S, B, I);
                  exit when I = 0;
                  Ada.Strings.Unbounded.Replace_Slice (S, I, I + B'Length - 1, A);
                  I := I + B'Length;
               end loop;
               return EL.Objects.To_Object (S);
            end;

      end case;
   end Replace;

   --  ------------------------------
   --  Return the substring before the token string
   --  ------------------------------
   function Substring_Before (Value : in EL.Objects.Object;
                              Token : in EL.Objects.Object) return EL.Objects.Object is
      S   : constant String := EL.Objects.To_String (Value);
      T   : constant String := EL.Objects.To_String (Token);
      Pos : constant Natural := Ada.Strings.Fixed.Index (S, T);
   begin
      if Pos = 0 then
         return EL.Objects.Null_Object;
      else
         return EL.Objects.To_Object (S (S'First .. Pos - 1));
      end if;
   end Substring_Before;

   --  ------------------------------
   --  Return the substring after the token string
   --  ------------------------------
   function Substring_After (Value : in EL.Objects.Object;
                              Token : in EL.Objects.Object) return EL.Objects.Object is
      S   : constant String := EL.Objects.To_String (Value);
      T   : constant String := EL.Objects.To_String (Token);
      Pos : constant Natural := Ada.Strings.Fixed.Index (S, T);
   begin
      if Pos = 0 then
         return EL.Objects.Null_Object;
      else
         return EL.Objects.To_Object (S (Pos + T'Length .. S'Last));
      end if;
   end Substring_After;

   --  ------------------------------
   --  Expand the search paths <b>Paths</b> into a new search path which adds the
   --  directory component <b>Dir</b>.  Example:
   --    Paths= /usr;/usr/local;/opt
   --    Dir  = bin
   --    Result= /usr/bin;/usr/local/bin;/opt/bin
   --  ------------------------------
   function Compose_Path (Paths : in EL.Objects.Object;
                          Dir   : in EL.Objects.Object) return EL.Objects.Object is
      P   : constant String := EL.Objects.To_String (Paths);
      D   : constant String := EL.Objects.To_String (Dir);
      R   : constant String := Util.Files.Compose_Path (P, D);
   begin
      return EL.Objects.To_Object (R);
   end Compose_Path;

   --  ------------------------------
   --  Register a set of functions in the namespace
   --  xmlns:fn="http://java.sun.com/jsp/jstl/functions"
   --  Functions:
   --    capitalize, toUpperCase, toLowerCase
   --  ------------------------------
   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class) is
   begin
      Mapper.Set_Function (Name      => STARTS_WITH_FN,
                           Namespace => FN_URI,
                           Func      => Starts_With'Access);
      Mapper.Set_Function (Name      => ENDS_WITH_FN,
                           Namespace => FN_URI,
                           Func      => Ends_With'Access);
      Mapper.Set_Function (Name      => INDEX_OF_FN,
                           Namespace => FN_URI,
                           Func      => Index_Of'Access);
      Mapper.Set_Function (Name      => SUBSTRING_FN,
                           Namespace => FN_URI,
                           Func      => Substring'Access);

      Mapper.Set_Function (Name      => CAPITALIZE_FN,
                           Namespace => FN_URI,
                           Func      => Capitalize'Access);
      Mapper.Set_Function (Name      => CONTAINS_FN,
                           Namespace => FN_URI,
                           Func      => Contains'Access);
      Mapper.Set_Function (Name      => LENGTH_FN,
                           Namespace => FN_URI,
                           Func      => Length'Access);
      Mapper.Set_Function (Name      => TO_LOWER_CASE_FN,
                           Namespace => FN_URI,
                           Func      => To_Lower_Case'Access);
      Mapper.Set_Function (Name      => TO_UPPER_CASE_FN,
                           Namespace => FN_URI,
                           Func      => To_Upper_Case'Access);
      Mapper.Set_Function (Name      => SUBSTRING_BEFORE_FN,
                           Namespace => FN_URI,
                           Func      => Substring_Before'Access);
      Mapper.Set_Function (Name      => TRIM_FN,
                           Namespace => FN_URI,
                           Func      => Trim'Access);
      Mapper.Set_Function (Name      => SUBSTRING_AFTER_FN,
                           Namespace => FN_URI,
                           Func      => Substring_After'Access);
      Mapper.Set_Function (Name      => COMPOSE_PATH_FN,
                           Namespace => FN_URI,
                           Func      => Compose_Path'Access);

      Mapper.Set_Function (Name      => ESCAPE_XML_FN,
                           Namespace => FN_URI,
                           Func      => Escape_Xml'Access);

      Mapper.Set_Function (Name      => REPLACE_FN,
                           Namespace => FN_URI,
                           Func      => Replace'Access);
   end Set_Functions;

end ASF.Views.Nodes.Core;
