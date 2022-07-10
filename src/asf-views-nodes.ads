-----------------------------------------------------------------------
--  asf-views-nodes -- Facelet node tree representation
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014, 2018, 2022 Stephane Carrez
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
with Ada.Strings.Unbounded;
with EL.Expressions;
with EL.Objects;
with Util.Strings;
with ASF.Components.Base;
with ASF.Contexts.Faces;
with ASF.Contexts.Facelets;
package ASF.Views.Nodes is

   use Ada.Strings.Unbounded;
   use ASF.Components.Base;
   use ASF.Contexts.Faces;
   use ASF.Contexts.Facelets;

   type Expression_Access_Array is array (Natural range <>) of EL.Expressions.Expression_Access;
   type Expression_Access_Array_Access is access Expression_Access_Array;

   --  ------------------------------
   --  Attribute of a node.
   --  ------------------------------
   --  The attribute has a name and a value.  When the value is not
   --  a literal, an EL expression is created to allow its evaluation.
   type Tag_Attribute is limited private;
   type Tag_Attribute_Access is access all Tag_Attribute;

   type Tag_Attribute_Array is array (Natural range <>) of aliased Tag_Attribute;

   type Tag_Attribute_Array_Access is access Tag_Attribute_Array;

   function "=" (Left : in Tag_Attribute; Right : in String) return Boolean;

   function "=" (Left, Right : in Tag_Attribute) return Boolean;

   --  Get the attribute name.
   function Get_Name (Attribute : Tag_Attribute) return String;

   --  Returns True if the attribute is static (not an EL expression).
   function Is_Static (Attribute : Tag_Attribute) return Boolean;

   --  Get the attribute value.  If the attribute is an EL expression
   --  evaluate that expression in the context of the given UI component.
   function Get_Value (Attribute : Tag_Attribute;
                       UI        : UIComponent'Class) return EL.Objects.Object;

   function Get_Value (Attribute : Tag_Attribute;
                       Context   : Faces_Context'Class) return EL.Objects.Object;

   function Get_Value (Attribute : Tag_Attribute;
                       Context   : Facelet_Context'Class) return EL.Objects.Object;

   --  Get the value from the attribute.  If the attribute is null or evaluates to
   --  a NULL object, returns the default value.  Convert the value into a string.
   function Get_Value (Attribute : in Tag_Attribute_Access;
                       Context   : in Facelet_Context'Class;
                       Default   : in String) return String;

   --  Get the EL expression associated with the given tag attribute.
   function Get_Expression (Attribute : in Tag_Attribute)
                                  return EL.Expressions.Expression;

   function Get_Value_Expression (Attribute : Tag_Attribute)
                                  return EL.Expressions.Value_Expression;

   function Get_Method_Expression (Attribute : Tag_Attribute)
                                  return EL.Expressions.Method_Expression;

   --  Reduce the expression by eliminating known variables and computing
   --  constant expressions.  The result expression is either another
   --  expression or a computed constant value.
   function Reduce_Expression (Attribute : Tag_Attribute;
                               Context   : Facelet_Context'Class)
                               return EL.Expressions.Expression;

   --  Find the tag attribute having the given name.
   --  Returns an access to the attribute cell within the array or null
   --  if the no attribute matches the name.
   function Find_Attribute (Attributes : Tag_Attribute_Array_Access;
                            Name       : String) return Tag_Attribute_Access;

   --  Report an error message for the attribute.
   procedure Error (Attribute : in Tag_Attribute;
                    Message   : in String;
                    Param1    : in String;
                    Param2    : in String := "");

   --  ------------------------------
   --  XHTML node
   --  ------------------------------
   --  The <b>Tag_Node</b> represents a UI component node in a view.
   type Tag_Node is tagged limited private;
   type Tag_Node_Access is access all Tag_Node'Class;

   --  Get the node attribute with the given name.
   --  Returns null if the node does not have such attribute.
   function Get_Attribute (Node : Tag_Node;
                           Name : String) return Tag_Attribute_Access;

   --  Get the line information where the tag node is defined.
   function Get_Line_Info (Node : Tag_Node) return Line_Info;

   --  Get the line information as a string.
   function Get_Line_Info (Node : Tag_Node) return String;

   --  Get the relative path name of the XHTML file in which this tag is defined.
   function Get_File_Name (Node : in Tag_Node) return String;

   --  Append a child tag node.
   procedure Append_Tag (Node  : in Tag_Node_Access;
                         Child : in Tag_Node_Access);

   --  Freeze the tag node tree and perform any initialization steps
   --  necessary to build the components efficiently.  After this call
   --  the tag node tree should not be modified and it represents a read-only
   --  tree.
   procedure Freeze (Node : access Tag_Node);

   --  Build the component attributes from the facelet tag node and the facelet context.
   procedure Build_Attributes (UI      : in out UIComponent'Class;
                               Node    : in Tag_Node'Class;
                               Context : in out Facelet_Context'Class);

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

   procedure Destroy (Node : in out Tag_Node_Access);

   --  Report an error message
   procedure Error (Node    : in Tag_Node'Class;
                    Message : in String;
                    Param1  : in String := "";
                    Param2  : in String := "");

   --  Iterate over the attributes defined on the node and
   --  execute the <b>Process</b> procedure.
   generic
      with procedure Process (Attr : in Tag_Attribute_Access);
   procedure Iterate_Attributes (Node : in Tag_Node'Class);

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
                         Expr    : in Expression_Access_Array_Access;
                         Context : in Faces_Context'Class);

   overriding
   procedure Build_Components (Node    : access Text_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class);

   --  Freeze the tag node tree.
   --  Count the number of Tag_Content represented by this node.
   overriding
   procedure Freeze (Node : access Text_Tag_Node);

   --  Delete the node and its children freeing the memory as necessary
   overriding
   procedure Delete (Node : access Text_Tag_Node);

   type Cursor is private;
   function First (Node : in Tag_Node_Access) return Cursor;

   function Has_Element (C : Cursor) return Boolean;
   function Element (Position : Cursor) return Tag_Node_Access;
   procedure Next (Position : in out Cursor);

   type Binding_Type;
   type Binding_Access is access constant Binding_Type;

   --  Create function to build a UIComponent
   type Create_Access is access function return ASF.Components.Base.UIComponent_Access;

   --  Create function to build a tag node
   type Tag_Node_Create_Access is access
     function (Binding    : in Binding_Type;
               Line       : in Line_Info;
               Parent     : in Tag_Node_Access;
               Attributes : in Tag_Attribute_Array_Access) return Tag_Node_Access;

   --  Create the When Tag
   function Create_Component_Node (Binding    : in Binding_Type;
                                   Line       : in Line_Info;
                                   Parent     : in Tag_Node_Access;
                                   Attributes : in Tag_Attribute_Array_Access)
                                   return Tag_Node_Access;

   --  Binding name
   type Name_Access is new Util.Strings.Name_Access;

   --  ------------------------------
   --  Binding definition.
   --  ------------------------------
   --  The binding links an XHTML entity name to a tag node implementation
   --  and a component creation handler.  When the XHTML entity is found,
   --  the associated binding is searched and when found the node is created
   --  by using the <b>Tag</b> create function.
   type Binding_Type is record
      Name      : Name_Access;
      Component : ASF.Views.Nodes.Create_Access;
      Tag       : ASF.Views.Nodes.Tag_Node_Create_Access;
   end record;

   Null_Binding : constant Binding_Type := Binding_Type '(null, null, null);

private

   type Cursor is record
      Node : Tag_Node_Access;
   end record;

   type Tag_Attribute is record
      Tag     : Tag_Node_Access;
      Binding : EL.Expressions.Expression_Access;
      Name    : Unbounded_String;
      Value   : Unbounded_String;
   end record;

   type Tag_Node is tagged limited record
      --  The parent node.
      Parent      : Tag_Node_Access;

      --  Attributes associated with this node.
      Attributes  : Tag_Attribute_Array_Access;

      --  The UIComponent factory that must be used to create the component.
      Factory     : Create_Access;

      Next        : Tag_Node_Access;
      First_Child : Tag_Node_Access;
      Last_Child  : Tag_Node_Access;

      --  Source line information where the tag node is defined (for error messages)
      Line        : Line_Info;
   end record;

   --  Initialize the node
   procedure Initialize (Node       : in Tag_Node_Access;
                         Binding    : in Binding_Type;
                         Line       : in Line_Info;
                         Parent     : in Tag_Node_Access;
                         Attributes : in Tag_Attribute_Array_Access);

   type Tag_Content;
   type Tag_Content_Access is access all Tag_Content;

   type Tag_Content is record
      Next : Tag_Content_Access := null;
      Text : Unbounded_String;
      Expr : EL.Expressions.Expression;
   end record;

   type Text_Tag_Node is new Tag_Node with record
      Count   : Natural := 0;
      Last    : Tag_Content_Access := null;
      Content : aliased Tag_Content;
   end record;

end ASF.Views.Nodes;
