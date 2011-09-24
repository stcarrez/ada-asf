-----------------------------------------------------------------------
--  nodes-core -- Core nodes
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
with Util.Strings.Transforms;
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
   CAPITALIZE_FN    : aliased constant String := "capitalize";
   TO_UPPER_CASE_FN : aliased constant String := "toUpperCase";
   TO_LOWER_CASE_FN : aliased constant String := "toLowerCase";

   function Capitalize (Value : EL.Objects.Object) return EL.Objects.Object;
   function To_Upper_Case (Value : EL.Objects.Object) return EL.Objects.Object;
   function To_Lower_Case (Value : EL.Objects.Object) return EL.Objects.Object;

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
   --  Register a set of functions in the namespace
   --  xmlns:fn="http://java.sun.com/jsp/jstl/functions"
   --  Functions:
   --    capitalize, toUpperCase, toLowerCase
   --  ------------------------------
   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class) is
      URI : constant String := "http://java.sun.com/jsp/jstl/functions";
   begin
      Mapper.Set_Function (Name      => CAPITALIZE_FN,
                           Namespace => URI,
                           Func      => Capitalize'Access);
      Mapper.Set_Function (Name      => TO_LOWER_CASE_FN,
                           Namespace => URI,
                           Func      => To_Lower_Case'Access);
      Mapper.Set_Function (Name      => TO_UPPER_CASE_FN,
                           Namespace => URI,
                           Func      => To_Upper_Case'Access);
   end Set_Functions;

end ASF.Views.Nodes.Core;
