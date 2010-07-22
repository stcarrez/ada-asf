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

with ASF.Components.Core;
with ASF.Contexts.Writer;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Util.Log.Loggers;
package body ASF.Views.Nodes is

   use EL.Expressions;

   procedure Free is
     new Ada.Unchecked_Deallocation (Tag_Node'Class,
                                     Tag_Node_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Tag_Attribute_Array,
                                     Tag_Attribute_Array_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (EL.Expressions.Expression'Class,
                                     EL.Expressions.Expression_Access);

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Views.Nodes");

   --  ------------------------------
   --  Attribute of a node.
   --  ------------------------------

   --  ------------------------------
   --  Get the attribute name.
   --  ------------------------------
   function Get_Name (Attribute : Tag_Attribute) return Unbounded_String is
   begin
      return Attribute.Name;
   end Get_Name;

   --  ------------------------------
   --  Get the attribute literal value.
   --  ------------------------------
   function Get_Value (Attribute : Tag_Attribute) return Unbounded_String is
   begin
      return Attribute.Value;
   end Get_Value;

   --  ------------------------------
   --  Get the attribute value.  If the attribute is an EL expression
   --  evaluate that expression in the context of the given UI component.
   --  ------------------------------
   function Get_Value (Attribute : Tag_Attribute;
                       UI        : UIComponent'Class) return EL.Objects.Object is
   begin
      if Attribute.Binding /= null then
         return Attribute.Binding.Get_Value (UI.Get_Context.Get_ELContext.all);
      else
         return EL.Objects.To_Object (Attribute.Value);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the attribute value.  If the attribute is an EL expression
   --  evaluate that expression in the context of the given UI component.
   --  ------------------------------
   function Get_Value (Attribute : Tag_Attribute;
                       Context   : Faces_Context'Class) return EL.Objects.Object is
   begin
      if Attribute.Binding /= null then
         return Attribute.Binding.Get_Value (Context.Get_ELContext.all);
      else
         return EL.Objects.To_Object (Attribute.Value);
      end if;
   end Get_Value;

   function Get_Value (Attribute : Tag_Attribute;
                       Context   : Facelet_Context'Class) return EL.Objects.Object is
   begin
      if Attribute.Binding /= null then
         return Attribute.Binding.Get_Value (Context.Get_ELContext.all);
      else
         return EL.Objects.To_Object (Attribute.Value);
      end if;

   exception
      when E : others =>
         Log.Error ("Expression error: {0}", Ada.Exceptions.Exception_Message (E));
         return EL.Objects.Null_Object;
   end Get_Value;

   function Get_ValueExpression (Attribute : Tag_Attribute;
                                 Context   : Facelet_Context'Class)
                                 return EL.Expressions.ValueExpression is
      V : EL.Objects.Object := EL.Objects.To_Object (Attribute.Value);
   begin
      if Attribute.Binding /= null then
         return EL.Expressions.Create_ValueExpression (V);
      else
         return EL.Expressions.Create_ValueExpression (V);
      end if;
   end Get_ValueExpression;

   --  ------------------------------
   --  Find the tag attribute having the given name.
   --  Returns an access to the attribute cell within the array or null
   --  if the no attribute matches the name.
   --  ------------------------------
   function Find_Attribute (Attributes : Tag_Attribute_Array_Access;
                            Name       : String) return Tag_Attribute_Access is
   begin
      for I in Attributes'Range loop
         declare
            Attr : constant Tag_Attribute_Access := Attributes (I)'Access;
         begin
            if Attr.Name = Name then
               return Attr;
            end if;
         end;
      end loop;
      return null;
   end Find_Attribute;

   --  ------------------------------
   --  XHTML node
   --  ------------------------------

   --  ------------------------------
   --  Get the node name.
   --  ------------------------------
   function Get_Name (Node : Tag_Node) return Unbounded_String is
   begin
      return Node.Name;
   end Get_Name;

   --  ------------------------------
   --  Get the node attribute with the given name.
   --  Returns null if the node does not have such attribute.
   --  ------------------------------
   function Get_Attribute (Node : Tag_Node;
                           Name : String) return Tag_Attribute_Access is
   begin
      if Node.Attributes = null then
         return null;
      end if;
      return Find_Attribute (Node.Attributes, Name);
   end Get_Attribute;

   --  ------------------------------
   --  Append a child tag node.
   --  ------------------------------
   procedure Append_Tag (Node  : in Tag_Node_Access;
                         Child : in Tag_Node_Access) is
   begin
      if Node.First_Child = null then
         Node.First_Child := Child;
      else
         Node.Last_Child.Next := Child;
      end if;
      Node.Last_Child := Child;
      Child.Parent    := Node;
      Child.Next      := null;
   end Append_Tag;

   --  ------------------------------
   --  Freeze the tag node tree and perform any initialization steps
   --  necessary to build the components efficiently.  After this call
   --  the tag node tree should not be modified and it represents a read-only
   --  tree.
   --  ------------------------------
   procedure Freeze (Node : access Tag_Node) is
   begin
      null;
   end Freeze;

   --  ------------------------------
   --  Delete the node and its children freeing the memory as necessary
   --  ------------------------------
   procedure Delete (Node : access Tag_Node) is
      Child : Tag_Node_Access := Node.First_Child;
      Next  : Tag_Node_Access;
   begin
      while Child /= null loop
         Next := Child.Next;
         Child.Delete;
         Free (Child);
         Child := Next;
      end loop;
      if Node.Attributes /= null then
         for I in Node.Attributes'Range loop
            declare
               Expr : EL.Expressions.Expression_Access := Node.Attributes (I).Binding;
            begin
               Free (Expr);
            end;
         end loop;
         Free (Node.Attributes);
      end if;
   end Delete;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  ------------------------------
   procedure Build_Components (Node    : access Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
      UI : constant UIComponent_Access := Node.Factory.all;
   begin
      Append (Parent, UI, Node);
      Node.Build_Children (UI, Context);
   end Build_Components;

   procedure Build_Children (Node    : access Tag_Node;
                             Parent  : in UIComponent_Access;
                             Context : in out Facelet_Context'Class) is
      Child : Tag_Node_Access;
   begin
      Child := Node.First_Child;
      while Child /= null loop
         Child.Build_Components (Parent, Context);
         Child := Child.Next;
      end loop;
   end Build_Children;

   overriding
   procedure Build_Components (Node    : access Text_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
      UI : constant UIComponent_Access
        := ASF.Components.Core.Create_UIText (Node.all'Access);
   begin
      Append (Parent, UI, Node);
   end Build_Components;

   --  ------------------------------
   --  Encode the content represented by this text node.
   --  The expressions are evaluated if necessary.
   --  ------------------------------
   procedure Encode_All (Node    : Text_Tag_Node;
                         Context : in Faces_Context'Class) is
      Writer  : constant ASF.Contexts.Writer.ResponseWriter_Access
        := Context.Get_Response_Writer;
      Content : access constant Tag_Content := Node.Content'Access;
   begin
      loop
         Writer.Write (Content.Text);
         declare
            Value : constant EL.Objects.Object
              := Content.Expr.Get_Value (Context.Get_ELContext.all);
         begin
            if not EL.Objects.Is_Null (Value) then
               Writer.Write_Text (Value);
            end if;
         end;
         Content := Content.Next;
         exit when Content = null;
      end loop;
   end Encode_All;

   function First (Node : in Tag_Node_Access) return Cursor is
      Result : Cursor;
   begin
      Result.Node := Node.First_Child;
      return Result;
   end First;

   function Has_Element (C : Cursor) return Boolean is
   begin
      return C.Node /= null;
   end Has_Element;

   function Element (Position : Cursor) return Tag_Node_Access is
   begin
      return Position.Node;
   end Element;

   procedure Next (Position : in out Cursor) is
   begin
      Position.Node := Position.Node.Next;
   end Next;

   --  Create the text Tag
   function Create_Component_Node (Name       : Unbounded_String;
                                   Parent     : Tag_Node_Access;
                                   Attributes : Tag_Attribute_Array_Access)
                                   return Tag_Node_Access is
      Node : constant Tag_Node_Access := new Tag_Node;
   begin
      Node.Name       := Name;
      Node.Parent     := Parent;
      Node.Attributes := Attributes;
      return Node.all'Access;
   end Create_Component_Node;

end ASF.Views.Nodes;
