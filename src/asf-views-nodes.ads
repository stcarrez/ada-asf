-----------------------------------------------------------------------
--  asf-views-nodes -- Facelet node tree representation
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

--  The <b>ASF.Views.Nodes</b> package defines the nodes and attributes
--  the represent the facelet node tree used to create the component tree.
--
--  The facelet node tree is composed of nodes represented by <b>Tag_Node</b>
--  and attributes represented by <b>Tag_Attribute</b>.  In a sense, this
--  is very close to an XML DOM tree.
with Ada.Finalization;
with Ada.Strings.Unbounded;
with EL.Expressions;
with EL.Objects;
with ASF.Components;
with ASF.Contexts.Faces;
with ASF.Contexts.Facelets;
package ASF.Views.Nodes is

   use Ada.Strings.Unbounded;
   use ASF.Components;
   use ASF.Contexts.Faces;
   use ASF.Contexts.Facelets;

   --  ------------------------------
   --  Attribute of a node.
   --  ------------------------------
   --  The attribute has a name and a value.  When the value is not
   --  a literal, an EL expression is created to allow its evaluation.
   type Tag_Attribute is private;
   type Tag_Attribute_Access is access all Tag_Attribute;

   type Tag_Attribute_Array is array (Natural range <>) of aliased Tag_Attribute;

   type Tag_Attribute_Array_Access is access Tag_Attribute_Array;

   --  Get the attribute name.
   function Get_Name (Attribute : Tag_Attribute) return Unbounded_String;

   --  Get the attribute literal value.
   function Get_Value (Attribute : Tag_Attribute) return Unbounded_String;

   --  Get the attribute value.  If the attribute is an EL expression
   --  evaluate that expression in the context of the given UI component.
   function Get_Value (Attribute : Tag_Attribute;
                       UI        : UIComponent'Class) return EL.Objects.Object;

   function Get_Value (Attribute : Tag_Attribute;
                       Context   : Faces_Context'Class) return EL.Objects.Object;

   function Get_Value (Attribute : Tag_Attribute;
                       Context   : Facelet_Context'Class) return EL.Objects.Object;

   --  Find the tag attribute having the given name.
   --  Returns an access to the attribute cell within the array or null
   --  if the no attribute matches the name.
   function Find_Attribute (Attributes : Tag_Attribute_Array_Access;
                            Name       : String) return Tag_Attribute_Access;

   --  ------------------------------
   --  XHTML node
   --  ------------------------------
   --  The <b>Tag_Node</b> represents a UI component node in a view.
   type Tag_Node is tagged limited private;
   type Tag_Node_Access is access all Tag_Node'Class;

   --  Get the node name.
   function Get_Name (Node : Tag_Node) return Unbounded_String;

   --  Get the node attribute with the given name.
   --  Returns null if the node does not have such attribute.
   function Get_Attribute (Node : Tag_Node;
                           Name : String) return Tag_Attribute_Access;

   --  Append a child tag node.
   procedure Append_Tag (Node  : in Tag_Node_Access;
                         Child : in Tag_Node_Access);

   --  Freeze the tag node tree and perform any initialization steps
   --  necessary to build the components efficiently.  After this call
   --  the tag node tree should not be modified and it represents a read-only
   --  tree.
   procedure Freeze (Node : access Tag_Node);

   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   procedure Build_Components (Node    : access Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class);

   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   procedure Build_Children (Node    : access Tag_Node;
                             Parent  : in UIComponent_Access;
                             Context : in out Facelet_Context'Class);

   --  Delete the node and its children freeing the memory as necessary
   procedure Delete (Node : access Tag_Node);

   --  ------------------------------
   --  Text nodes mixed with EL expressions.
   --  ------------------------------
   --  The text node is used when the XHTML reader does not recognize an entity.
   --  The reader appends the content to a text node until an entity is recognized.
   --  The text node can contain attributes associated with the unrecognize entities.
   --  Attributes and raw text may contain EL expressions that will be evaluated
   --  when the component is rendered.  The <b>Text_Tag_Node</b> contains a list
   --  of raw text and EL expression to evaluate.
   type Text_Tag_Node is new Tag_Node with private;
   type Text_Tag_Node_Access is access all Text_Tag_Node;

   --  Encode the content represented by this text node.
   --  The expressions are evaluated if necessary.
   procedure Encode_All (Node    : in Text_Tag_Node;
                         Context : in Faces_Context'Class);

   overriding
   procedure Build_Components (Node    : access Text_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class);


   type Cursor is private;
   function First (Node : in Tag_Node_Access) return Cursor;

   function Has_Element (C : Cursor) return Boolean;
   function Element (Position : Cursor) return Tag_Node_Access;
   procedure Next (Position : in out Cursor);

   --  Create function to build a UIComponent
   type Create_Access is access function return ASF.Components.UIComponent_Access;

   --  Create function to build a tag node
   type Tag_Node_Create_Access is access
     function (Name       : Unbounded_String;
               Parent     : Tag_Node_Access;
               Attributes : Tag_Attribute_Array_Access) return Tag_Node_Access;

   --  Create the When Tag
   function Create_Component_Node (Name       : Unbounded_String;
                                   Parent     : Tag_Node_Access;
                                   Attributes : Tag_Attribute_Array_Access)
                                   return Tag_Node_Access;

private

   type Cursor is record
      Node : Tag_Node_Access;
   end record;

   type Tag_Attribute is record
      Name    : Unbounded_String;
      Value   : Unbounded_String;
      Binding : EL.Expressions.Expression_Access;
   end record;

   type Tag_Node is tagged limited record
      --  The parent node.
      Parent     : Tag_Node_Access;

      --  The tag name.
      Name       : Unbounded_String;

      --  Attributes associated with this node.
      Attributes : Tag_Attribute_Array_Access;

      --  The UIComponent factory that must be used to create the component.
      Factory    : Create_Access;

      Next        : Tag_Node_Access;
      First_Child : Tag_Node_Access;
      Last_Child  : Tag_Node_Access;
   end record;

   type Tag_Content;
   type Tag_Content_Access is access all Tag_Content;

   type Tag_Content is record
      Next : Tag_Content_Access;
      Text : Unbounded_String;
      Expr : EL.Expressions.Expression;
   end record;

   type Text_Tag_Node is new Tag_Node with record
      Content : aliased Tag_Content;
      Last    : Tag_Content_Access;
   end record;

end ASF.Views.Nodes;
