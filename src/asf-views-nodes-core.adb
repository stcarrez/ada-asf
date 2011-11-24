-----------------------------------------------------------------------
--  nodes-core -- Core nodes
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
   function Create_Set_Tag_Node (Name       : Unbounded_String;
                                 Line       : Line_Info;
                                 Parent     : Tag_Node_Access;
                                 Attributes : Tag_Attribute_Array_Access)
                                 return Tag_Node_Access is
      Node : constant Set_Tag_Node_Access := new Set_Tag_Node;
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
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

      Value  : constant EL.Expressions.Expression
        := Get_Expression (Node.Value.all);
   begin
      Context.Set_Variable (Node.Var.Value, Value);
   end Build_Components;

   --  ------------------------------
   --  If Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the If Tag
   --  ------------------------------
   function Create_If_Tag_Node (Name       : Unbounded_String;
                                Line       : Line_Info;
                                Parent     : Tag_Node_Access;
                                Attributes : Tag_Attribute_Array_Access)
                                return Tag_Node_Access is
      Node : constant If_Tag_Node_Access := new If_Tag_Node;
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
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
      Value : constant EL.Objects.Object := Get_Value (Node.Condition.all, Context);
   begin
      if Node.Var /= null then
         Context.Set_Attribute (Node.Var.Value, Value);
      end if;
      if EL.Objects.To_Boolean (Value) then
         Tag_Node (Node.all).Build_Children (Parent, Context);
      end if;
   end Build_Components;

   --  ------------------------------
   --  Choose Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Choose Tag
   --  ------------------------------
   function Create_Choose_Tag_Node (Name       : Unbounded_String;
                                    Line       : Line_Info;
                                    Parent     : Tag_Node_Access;
                                    Attributes : Tag_Attribute_Array_Access)
                                    return Tag_Node_Access is
      Node : constant Choose_Tag_Node_Access := new Choose_Tag_Node;
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
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
   function Create_When_Tag_Node (Name       : Unbounded_String;
                                  Line       : Line_Info;
                                  Parent     : Tag_Node_Access;
                                  Attributes : Tag_Attribute_Array_Access)
                                  return Tag_Node_Access is
      Node : constant When_Tag_Node_Access := new When_Tag_Node;
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
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
      return EL.Objects.To_Boolean (Get_Value (Node.Condition.all, Context));

   exception
      when E : others =>
            Node.Error ("Exception: {0}", Ada.Exceptions.Exception_Message (E));
            return False;
   end Is_Selected;

   --  ------------------------------
   --  Create the Otherwise Tag
   --  ------------------------------
   function Create_Otherwise_Tag_Node (Name       : Unbounded_String;
                                       Line       : Line_Info;
                                       Parent     : Tag_Node_Access;
                                       Attributes : Tag_Attribute_Array_Access)
                                       return Tag_Node_Access is
      Node : constant Otherwise_Tag_Node_Access := new Otherwise_Tag_Node;
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
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

   --  Tag library definition.  Names must be sorted.
   Tag_Bindings  : aliased constant ASF.Factory.Binding_Array
     := ((Name => CHOOSE_TAG'Access,
          Component => null,
          Tag => Create_Choose_Tag_Node'Access),
       (Name => IF_TAG'Access,
        Component => null,
        Tag => Create_If_Tag_Node'Access),
       (Name => OTHERWISE_TAG'Access,
        Component => null,
        Tag => Create_Otherwise_Tag_Node'Access),
       (Name => SET_TAG'Access,
        Component => null,
        Tag => Create_Set_Tag_Node'Access),
       (Name => WHEN_TAG'Access,
        Component => null,
        Tag => Create_When_Tag_Node'Access));

   Tag_Factory   : aliased constant ASF.Factory.Factory_Bindings
     := (URI => URI'Access, Bindings => Tag_Bindings'Access);

   --  ------------------------------
   --  Tag factory for nodes defined in this package.
   --  ------------------------------
   function Definition return ASF.Factory.Factory_Bindings_Access is
   begin
      return Tag_Factory'Access;
   end Definition;

   --  Function names
   CAPITALIZE_FN       : aliased constant String := "capitalize";
   COMPOSE_PATH_FN     : aliased constant String := "composePath";
   CONTAINS_FN         : aliased constant String := "contains";
   LENGTH_FN           : aliased constant String := "length";
   TO_UPPER_CASE_FN    : aliased constant String := "toUpperCase";
   TO_LOWER_CASE_FN    : aliased constant String := "toLowerCase";
   SUBSTRING_AFTER_FN  : aliased constant String := "substringAfter";
   SUBSTRING_BEFORE_FN : aliased constant String := "substringBefore";
   TRIM_FN             : aliased constant String := "trim";

   function Length (Value : in EL.Objects.Object) return EL.Objects.Object;
   function Contains (Value : in EL.Objects.Object;
                      Search : in EL.Objects.Object) return EL.Objects.Object;
   function Capitalize (Value : EL.Objects.Object) return EL.Objects.Object;
   function To_Upper_Case (Value : EL.Objects.Object) return EL.Objects.Object;
   function To_Lower_Case (Value : EL.Objects.Object) return EL.Objects.Object;
   function Trim (Value : in EL.Objects.Object) return EL.Objects.Object;

   function Substring_Before (Value : in EL.Objects.Object;
                              Token : in EL.Objects.Object) return EL.Objects.Object;
   function Substring_After (Value : in EL.Objects.Object;
                             Token : in EL.Objects.Object) return EL.Objects.Object;
   function Compose_Path (Paths : in EL.Objects.Object;
                          Dir   : in EL.Objects.Object) return EL.Objects.Object;

   --  ------------------------------
   --  Get the length of the object.
   --  ------------------------------
   function Length (Value : in EL.Objects.Object) return EL.Objects.Object is
      S : constant String := EL.Objects.To_String (Value);
   begin
      return EL.Objects.To_Object (Integer (S'Length));
   end Length;

   --  ------------------------------
   --  Check if the search string is contained in the value.  If the value is a wide string,
   --  the search string is converted to a wide string and the search is made using wide string.
   --  Otherwise the value and search string are converted to a string.
   --  Returns true if the <b>Search</b> is contained in <b>Value</b>
   --  ------------------------------
   function Contains (Value  : in EL.Objects.Object;
                      Search : in EL.Objects.Object) return EL.Objects.Object is
      use Ada.Strings;

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
               return EL.Objects.To_Object (Ada.Strings.Wide_Wide_Fixed.Index (S, P) > 0);
            end;

         when others =>
            declare
               S : constant String := EL.Objects.To_String (Value);
               P : constant String := EL.Objects.To_String (Search);
            begin
               return EL.Objects.To_Object (Ada.Strings.Fixed.Index (S, P) > 0);
            end;

      end case;
   end Contains;

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
      use Ada.Strings;

      Of_Type : constant EL.Objects.Data_Type := EL.Objects.Get_Type (Value);
   begin
      case Of_Type is
         when EL.Objects.TYPE_STRING =>
            declare
               S : constant String := EL.Objects.To_String (Value);
            begin
               return EL.Objects.To_Object (Ada.Strings.Fixed.Trim (S, Both));
            end;

         when EL.Objects.TYPE_WIDE_STRING =>
            declare
               S : constant Wide_Wide_String := EL.Objects.To_Wide_Wide_String (Value);
            begin
               return EL.Objects.To_Object (Ada.Strings.Wide_Wide_Fixed.Trim (S, Both));
            end;

         when others =>
            return Value;
      end case;
   end Trim;

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
   end Set_Functions;

end ASF.Views.Nodes.Core;
