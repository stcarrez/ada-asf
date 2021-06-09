-----------------------------------------------------------------------
--  asf-views-nodes-core -- Core nodes
--  Copyright (C) 2009 - 2021 Stephane Carrez
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

--  = JSTL Components =
--  The JSTL components are defined in the following namespace:
--  ```
--  xmlns:c="http://java.sun.com/jstl/core"
--  ```
--
--  The facelet components are implemented by the `ASF.Views.Nodes.Facelets`
--  package which defines the pre-defined tags for composing a view.  Nodes of
--  this package are instantiated when the facelet XML tag is found when reading
--  the XHTML view description.
--
--  @include-doc docs/comp-jstl/*.txt
--
--  @notes
--  The <b>ASF.Views.Nodes.Core</b> package defines some pre-defined
--  core tag nodes which are mapped in the following namespaces:
--
--    xmlns:c="http://java.sun.com/jstl/core"
--    xmlns:ui="http://java.sun.com/jsf/facelets"
--    xmlns:fn="http://java.sun.com/jsp/jstl/functions"
with ASF.Factory;
with EL.Functions;
package ASF.Views.Nodes.Core is

   FN_URI : constant String := "http://java.sun.com/jsp/jstl/functions";

   --  Register the facelets component factory.
   procedure Register (Factory : in out ASF.Factory.Component_Factory);

   --  Register a set of functions in the namespace
   --  xmlns:fn="http://java.sun.com/jsp/jstl/functions"
   --  Functions:
   --    capitalize, toUpperCase, toLowerCase
   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class);

   --  ------------------------------
   --  Set Tag
   --  ------------------------------
   --  The <c:set var="name" value="#{expr}"/> variable creation.
   --  The variable is created in the faces context.
   type Set_Tag_Node is new Tag_Node with private;
   type Set_Tag_Node_Access is access all Set_Tag_Node'Class;

   --  Create the Set Tag
   function Create_Set_Tag_Node (Binding    : in Binding_Type;
                                 Line       : in Line_Info;
                                 Parent     : in Tag_Node_Access;
                                 Attributes : in Tag_Attribute_Array_Access)
                                 return Tag_Node_Access;

   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   overriding
   procedure Build_Components (Node    : access Set_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class);

   --  ------------------------------
   --  If Tag
   --  ------------------------------
   --  The <c:if test="#{expr}"> ...</c:if> condition.
   --  If the condition evaluates to true when the component tree is built,
   --  the children of this node are evaluated.  Otherwise the entire
   --  sub-tree is not present in the component tree.
   type If_Tag_Node is new Tag_Node with private;
   type If_Tag_Node_Access is access all If_Tag_Node'Class;

   --  Create the If Tag
   function Create_If_Tag_Node (Binding    : in Binding_Type;
                                Line       : in Line_Info;
                                Parent     : in Tag_Node_Access;
                                Attributes : in Tag_Attribute_Array_Access)
                                return Tag_Node_Access;

   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   overriding
   procedure Build_Components (Node    : access If_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class);

   --  ------------------------------
   --  Choose Tag
   --  ------------------------------
   --  The <c:choose> ...</c:choose> choice.
   --  Evaluate a set of choices (<c:when>) until one of them is found.
   --  When no choice is found, evaluate the <c:otherwise> node.
   type Choose_Tag_Node is new Tag_Node with private;
   type Choose_Tag_Node_Access is access all Choose_Tag_Node'Class;

   --  Freeze the tag node tree and perform any initialization steps
   --  necessary to build the components efficiently.
   --  Prepare the evaluation of choices by identifying the <c:when> and
   --  <c:otherwise> conditions.
   overriding
   procedure Freeze (Node : access Choose_Tag_Node);

   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   overriding
   procedure Build_Components (Node    : access Choose_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class);

   --  Create the <c:choose> tag node
   function Create_Choose_Tag_Node (Binding    : in Binding_Type;
                                    Line       : in Line_Info;
                                    Parent     : in Tag_Node_Access;
                                    Attributes : in Tag_Attribute_Array_Access)
                                   return Tag_Node_Access;

   --  ------------------------------
   --  When Tag
   --  ------------------------------
   --  The <c:when test="#{expr}"> ...</c:when> choice.
   --  If the condition evaluates to true when the component tree is built,
   --  the children of this node are evaluated.  Otherwise the entire
   --  sub-tree is not present in the component tree.
   type When_Tag_Node is new If_Tag_Node with private;
   type When_Tag_Node_Access is access all When_Tag_Node'Class;

   --  Check whether the node condition is selected.
   function Is_Selected (Node    : When_Tag_Node;
                         Context : Facelet_Context'Class) return Boolean;

   --  Create the When Tag
   function Create_When_Tag_Node (Binding    : in Binding_Type;
                                  Line       : in Line_Info;
                                  Parent     : in Tag_Node_Access;
                                  Attributes : in Tag_Attribute_Array_Access)
                                 return Tag_Node_Access;

   --  ------------------------------
   --  Otherwise Tag
   --  ------------------------------
   --  The <c:otherwise> ...</c:otherwise> choice.
   --  When all the choice conditions were false, the component tree is built,
   --  the children of this node are evaluated.
   type Otherwise_Tag_Node is new Tag_Node with null record;
   type Otherwise_Tag_Node_Access is access all Otherwise_Tag_Node'Class;

   --  Create the Otherwise Tag
   function Create_Otherwise_Tag_Node (Binding    : in Binding_Type;
                                       Line       : in Line_Info;
                                       Parent     : in Tag_Node_Access;
                                       Attributes : in Tag_Attribute_Array_Access)
                                       return Tag_Node_Access;

   --  Java Facelet provides a <c:repeat> tag.  It must not be implemented
   --  because it was proven this was not a good method for iterating over a list.
private

   type Set_Tag_Node is new Tag_Node with record
      Var   : Tag_Attribute_Access;
      Value : Tag_Attribute_Access;
   end record;

   type If_Tag_Node is new Tag_Node with record
      Condition : Tag_Attribute_Access;
      Var       : Tag_Attribute_Access;
   end record;

   type When_Tag_Node is new If_Tag_Node with record
      Next_Choice : When_Tag_Node_Access;
   end record;

   type Choose_Tag_Node is new Tag_Node with record
      Choices   : When_Tag_Node_Access;
      Otherwise : Tag_Node_Access;
   end record;

end ASF.Views.Nodes.Core;
