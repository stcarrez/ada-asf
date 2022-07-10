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

with EL.Contexts.Default;
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
     new Ada.Unchecked_Deallocation (Tag_Content,
                                     Tag_Content_Access);

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
   --  Report an error message for the attribute.
   --  ------------------------------
   procedure Error (Attribute : in Tag_Attribute;
                    Message   : in String;
                    Param1    : in String;
                    Param2    : in String := "") is
   begin
      if Attribute.Tag /= null then
         Attribute.Tag.Error (Message, Param1, Param2);
      else
         Log.Error (Message, Param1, Param2);
      end if;
   end Error;

   --  ------------------------------
   --  Compare the attribute name.
   --  ------------------------------
   function "=" (Left : in Tag_Attribute; Right : in String) return Boolean is
   begin
      return Left.Name = Right;
   end "=";

   overriding
   function "=" (Left, Right : in Tag_Attribute) return Boolean is
   begin
      return Left.Name = Right.Name;
   end "=";

   --  ------------------------------
   --  Get the attribute name.
   --  ------------------------------
   function Get_Name (Attribute : Tag_Attribute) return String is
   begin
      return To_String (Attribute.Name);
   end Get_Name;

   --  ------------------------------
   --  Returns True if the attribute is static (not an EL expression).
   --  ------------------------------
   function Is_Static (Attribute : Tag_Attribute) return Boolean is
   begin
      return Attribute.Binding = null;
   end Is_Static;

   --  ------------------------------
   --  Get the attribute value.  If the attribute is an EL expression
   --  evaluate that expression in the context of the given UI component.
   --  ------------------------------
   function Get_Value (Attribute : Tag_Attribute;
                       UI        : UIComponent'Class) return EL.Objects.Object is

      procedure Handle_Exception (E : in Ada.Exceptions.Exception_Occurrence);

      procedure Handle_Exception (E : in Ada.Exceptions.Exception_Occurrence) is
      begin
         Error (Attribute, "Evaluation error: {0}", Ada.Exceptions.Exception_Message (E));
      end Handle_Exception;

   begin
      if Attribute.Binding /= null then
         declare
            Ctx     : constant EL.Contexts.ELContext_Access := UI.Get_Context.Get_ELContext;
            Context : EL.Contexts.Default.Guarded_Context (Handle_Exception'Access, Ctx);
         begin
            return Attribute.Binding.Get_Value (Context);
         end;
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

      procedure Handle_Exception (E : in Ada.Exceptions.Exception_Occurrence);

      procedure Handle_Exception (E : in Ada.Exceptions.Exception_Occurrence) is
      begin
         Error (Attribute, "Evaluation error: {0}", Ada.Exceptions.Exception_Message (E));
      end Handle_Exception;

   begin
      if Attribute.Binding /= null then
         declare
            Ctx     : constant EL.Contexts.ELContext_Access := Context.Get_ELContext;
            Context : EL.Contexts.Default.Guarded_Context (Handle_Exception'Access, Ctx);
         begin
            return Attribute.Binding.Get_Value (Context);
         end;
      else
         return EL.Objects.To_Object (Attribute.Value);
      end if;
   end Get_Value;

   function Get_Value (Attribute : Tag_Attribute;
                       Context   : Facelet_Context'Class) return EL.Objects.Object is

      procedure Handle_Exception (E : in Ada.Exceptions.Exception_Occurrence);

      procedure Handle_Exception (E : in Ada.Exceptions.Exception_Occurrence) is
      begin
         Error (Attribute, "Evaluation error: {0}", Ada.Exceptions.Exception_Message (E));
      end Handle_Exception;
   begin
      if Attribute.Binding /= null then
         declare
            Ctx     : constant EL.Contexts.ELContext_Access := Context.Get_ELContext;
            Context : EL.Contexts.Default.Guarded_Context (Handle_Exception'Access, Ctx);
         begin
            return Attribute.Binding.Get_Value (Context);
         end;
      else
         return EL.Objects.To_Object (Attribute.Value);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the value from the attribute.  If the attribute is null or evaluates to
   --  a NULL object, returns the default value.  Convert the value into a string.
   --  ------------------------------
   function Get_Value (Attribute : in Tag_Attribute_Access;
                       Context   : in Facelet_Context'Class;
                       Default   : in String) return String is
   begin
      if Attribute = null then
         return Default;
      else
         declare
            Value : constant EL.Objects.Object := Get_Value (Attribute.all, Context);
         begin
            if EL.Objects.Is_Null (Value) then
               return Default;
            else
               return EL.Objects.To_String (Value);
            end if;
         end;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the EL expression associated with the given tag attribute.
   --  ------------------------------
   function Get_Expression (Attribute : in Tag_Attribute)
                            return EL.Expressions.Expression is
   begin
      if Attribute.Binding /= null then
         return EL.Expressions.Expression (Attribute.Binding.all);
      else
         return EL.Expressions.Create_Expression (EL.Objects.To_Object (Attribute.Value));
      end if;
   end Get_Expression;

   function Get_Value_Expression (Attribute : Tag_Attribute)
                                  return EL.Expressions.Value_Expression is
   begin
      if Attribute.Binding /= null then
         return EL.Expressions.Create_Expression (Attribute.Binding.all);
      else
         return EL.Expressions.Create_ValueExpression (EL.Objects.To_Object (Attribute.Value));
      end if;
   end Get_Value_Expression;

   function Get_Method_Expression (Attribute : Tag_Attribute)
                                   return EL.Expressions.Method_Expression is
   begin
      if Attribute.Binding /= null then
         return EL.Expressions.Create_Expression (Attribute.Binding.all);
      else
         Error (Attribute, "Invalid method expression", "");
         raise Constraint_Error with "Invalid method expression";
      end if;
   end Get_Method_Expression;

   --  ------------------------------
   --  Reduce the expression by eliminating known variables and computing
   --  constant expressions.  The result expression is either another
   --  expression or a computed constant value.
   --  ------------------------------
   function Reduce_Expression (Attribute : Tag_Attribute;
                               Context   : Facelet_Context'Class)
                               return EL.Expressions.Expression is
      E : constant EL.Expressions.Expression
        := EL.Expressions.Expression (Attribute.Binding.all);
   begin
      return E.Reduce_Expression (Context.Get_ELContext.all);
   end Reduce_Expression;

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
   --  Get the line information where the tag node is defined.
   --  ------------------------------
   function Get_Line_Info (Node : Tag_Node) return Line_Info is
   begin
      return Node.Line;
   end Get_Line_Info;

   --  ------------------------------
   --  Get the line information as a string.
   --  ------------------------------
   function Get_Line_Info (Node : Tag_Node) return String is
      L : constant String := Natural'Image (Node.Line.Line);
      C : constant String := Natural'Image (Node.Line.Column);
   begin
      if Node.Line.File = null then
         return "?:"
           & L (L'First + 1 .. L'Last)
           & ':' & C (C'First + 1 .. C'Last);
      else
         return Node.Line.File.Path
           & ':' & L (L'First + 1 .. L'Last)
           & ':' & C (C'First + 1 .. C'Last);
      end if;
   end Get_Line_Info;

   --  ------------------------------
   --  Get the relative path name of the XHTML file in which this tag is defined.
   --  ------------------------------
   function Get_File_Name (Node : in Tag_Node) return String is
      File : constant File_Info_Access := Node.Line.File;
   begin
      return File.Path (File.Relative_Pos .. File.Path'Last);
   end Get_File_Name;

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
   --  Initialize the node
   --  ------------------------------
   procedure Initialize (Node       : in Tag_Node_Access;
                         Binding    : in Binding_Type;
                         Line       : in Line_Info;
                         Parent     : in Tag_Node_Access;
                         Attributes : in Tag_Attribute_Array_Access) is
   begin
      Node.Factory := Binding.Component;
      Node.Line    := Line;
      Node.Parent  := Parent;
      Node.Attributes := Attributes;
      if Node.Attributes /= null then
         for I in Attributes.all'Range loop
            Attributes (I).Tag := Node;
         end loop;
      end if;
      Append_Tag (Parent, Node);
   end Initialize;

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

   procedure Destroy (Node : in out Tag_Node_Access) is
   begin
      Node.Delete;
      Free (Node);
   end Destroy;

   --  ------------------------------
   --  Report an error message
   --  ------------------------------
   procedure Error (Node    : in Tag_Node'Class;
                    Message : in String;
                    Param1  : in String := "";
                    Param2  : in String := "") is
      L : constant String := Node.Get_Line_Info;
   begin
      Log.Error (L & ":" & Message, Param1, Param2);
   end Error;

   --  ------------------------------
   --  Build the component attributes from the facelet tag node and the facelet context.
   --  ------------------------------
   procedure Build_Attributes (UI      : in out UIComponent'Class;
                               Node    : in Tag_Node'Class;
                               Context : in out Facelet_Context'Class) is

      procedure Process_Attribute (Attr : in Tag_Attribute_Access);

      procedure Process_Attribute (Attr : in Tag_Attribute_Access) is
      begin
         if Attr.Binding /= null then
            --  Reduce the expression by eliminating variables which are defined in
            --  the Facelet context.  We can obtain another expression or a constant value.
            declare
               Ctx  : constant EL.Contexts.ELContext_Access := Context.Get_ELContext;
               Expr : constant EL.Expressions.Expression
                 := EL.Expressions.Expression (Attr.Binding.all).Reduce_Expression (Ctx.all);
            begin
               if Expr.Is_Constant then
                  UI.Set_Attribute (Def   => Attr,
                                    Value => Expr.Get_Value (Ctx.all));
               else
                  UI.Set_Attribute (Def   => Attr,
                                    Value => Expr);
               end if;
            end;
         end if;
      end Process_Attribute;

      --  Iterate over the attributes to resolve some value expressions.
      procedure Iterate_Attributes is
        new ASF.Views.Nodes.Iterate_Attributes (Process_Attribute);
   begin
      Iterate_Attributes (Node);
   end Build_Attributes;

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
      Build_Attributes (UI.all, Node.all, Context);
      UI.Initialize (UI.Get_Context.all);
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

   --  ------------------------------
   --  Iterate over the attributes defined on the node and
   --  execute the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate_Attributes (Node : in Tag_Node'Class) is
   begin
      for I in Node.Attributes'Range loop
         declare
            Attr : constant Tag_Attribute_Access := Node.Attributes (I)'Access;
         begin
            Process (Attr);
         end;
      end loop;
   end Iterate_Attributes;

   --  ------------------------------
   --  Freeze the tag node tree.
   --  Count the number of Tag_Content represented by this node.
   --  ------------------------------
   overriding
   procedure Freeze (Node : access Text_Tag_Node) is
      Content : access constant Tag_Content := Node.Content'Access;
      Count   : Natural := 0;
   begin
      loop
         Content := Content.Next;
         Count   := Count + 1;
         exit when Content = null;
      end loop;
      Node.Count := Count;
   end Freeze;

   overriding
   procedure Build_Components (Node    : access Text_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is

      UI : constant ASF.Components.Core.UIText_Access
        := ASF.Components.Core.Create_UIText (Node.all'Access);
      Expr_Table : Expression_Access_Array_Access := null;
      Ctx        : constant EL.Contexts.ELContext_Access := Context.Get_ELContext;
      Content    : access constant Tag_Content := Node.Content'Access;
      Pos        : Natural := 1;
   begin
      Append (Parent, UI.all'Access, Node);
      loop
         if not Content.Expr.Is_Null then
            --  Reduce the expression by eliminating variables which are defined in
            --  the Facelet context.  We can obtain another expression or a constant value.
            declare
               Expr : constant EL.Expressions.Expression
                 := Content.Expr.Reduce_Expression (Ctx.all);
            begin
               if Expr /= Content.Expr then
                  if Expr_Table = null then
                     Expr_Table := new Expression_Access_Array (1 .. Node.Count);
                     UI.Set_Expression_Table (Expr_Table);
                  end if;
                  Expr_Table (Pos) := new EL.Expressions.Expression '(Expr);
               end if;
            end;
         end if;

         Content := Content.Next;
         Pos     := Pos + 1;
         exit when Content = null;
      end loop;
   end Build_Components;

   --  ------------------------------
   --  Delete the node and its children freeing the memory as necessary
   --  ------------------------------
   overriding
   procedure Delete (Node : access Text_Tag_Node) is
      Content : Tag_Content_Access := Node.Content.Next;
   begin
      while Content /= null loop
         declare
            Next : constant Tag_Content_Access := Content.Next;
         begin
            Free (Content);
            Content := Next;
         end;
      end loop;
      Node.Content.Next := null;
      Node.Last := null;
   end Delete;

   --  ------------------------------
   --  Encode the content represented by this text node.
   --  The expressions are evaluated if necessary.
   --  ------------------------------
   procedure Encode_All (Node    : in Text_Tag_Node;
                         Expr    : in Expression_Access_Array_Access;
                         Context : in Faces_Context'Class) is
      Writer  : constant ASF.Contexts.Writer.Response_Writer_Access
        := Context.Get_Response_Writer;
      Content : access constant Tag_Content := Node.Content'Access;
      Pos     : Natural := 1;
   begin
      loop
         Writer.Write (Content.Text);
         begin
            if Expr /= null and then Expr (Pos) /= null then
               declare
                  Value : constant EL.Objects.Object
                    := Expr (Pos).Get_Value (Context.Get_ELContext.all);
               begin
                  if not EL.Objects.Is_Null (Value) then
                     Writer.Write_Text (Value);
                  end if;
               end;
            else
               declare
                  Value : constant EL.Objects.Object
                    := Content.Expr.Get_Value (Context.Get_ELContext.all);
               begin
                  if not EL.Objects.Is_Null (Value) then
                     Writer.Write_Text (Value);
                  end if;
               end;
            end if;

         exception
            when E : others =>
               Node.Error ("Evaluation error: {0}", Ada.Exceptions.Exception_Message (E));

         end;
         Content := Content.Next;
         Pos := Pos + 1;
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

   --  Create a tag node
   --  Create the text Tag
   function Create_Component_Node (Binding    : in Binding_Type;
                                   Line       : in Line_Info;
                                   Parent     : in Tag_Node_Access;
                                   Attributes : in Tag_Attribute_Array_Access)
                                   return Tag_Node_Access is
      Node : constant Tag_Node_Access := new Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      return Node.all'Access;
   end Create_Component_Node;

end ASF.Views.Nodes;
