-----------------------------------------------------------------------
--  nodes-facelets -- Facelets composition nodes
--  Copyright (C) 2009, 2010, 2011, 2015, 2018, 2022 Stephane Carrez
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

--  The <b>ASF.Views.Nodes.Facelets</b> package defines some pre-defined
--  tags for composing a view.
--
--    xmlns:ui="http://java.sun.com/jsf/facelets"
--
--  The following Facelets core elements are defined:
--    <ui:include src="..."/>
--    <ui:decorate view="..."/>
--    <ui:define name="..."/>
--    <ui:insert name="..."/>
--    <ui:param name="..." value="..."/>
--    <ui:debug/>
--    <ui:comment condition="...">...</ui:comment>
--    <ui:composition .../>
--
with Ada.Exceptions;
with Util.Log.Loggers;
with EL.Contexts;
with EL.Variables.Default;
package body ASF.Views.Nodes.Facelets is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Views.Nodes.Facelets");

   COMPOSITION_TAG  : aliased constant String := "composition";
   DECORATE_TAG     : aliased constant String := "decorate";
   DEBUG_TAG        : aliased constant String := "debug";
   DEFINE_TAG       : aliased constant String := "define";
   INCLUDE_TAG      : aliased constant String := "include";
   INSERT_TAG       : aliased constant String := "insert";
   PARAM_TAG        : aliased constant String := "param";
   COMMENT_TAG      : aliased constant String := "comment";

   URI           : aliased constant String := "http://java.sun.com/jsf/facelets";

   --  ------------------------------
   --  Register the facelets component factory.
   --  ------------------------------
   procedure Register (Factory : in out ASF.Factory.Component_Factory) is
   begin
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => COMMENT_TAG'Access,
                            Tag    => Create_Comment_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => COMPOSITION_TAG'Access,
                            Tag    => Create_Composition_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => DEBUG_TAG'Access,
                            Tag    => Create_Debug_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => DECORATE_TAG'Access,
                            Tag    => Create_Decorate_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => DEFINE_TAG'Access,
                            Tag    => Create_Define_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => INCLUDE_TAG'Access,
                            Tag    => Create_Include_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => INSERT_TAG'Access,
                            Tag    => Create_Insert_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => PARAM_TAG'Access,
                            Tag    => Create_Param_Tag_Node'Access,
                            Create => null);
   end Register;

   --  ------------------------------
   --  Include Tag
   --  ------------------------------
   --  The <ui:include src="..."/>

   --  ------------------------------
   --  Create the Include Tag
   --  ------------------------------
   function Create_Include_Tag_Node (Binding    : in Binding_Type;
                                     Line       : in Line_Info;
                                     Parent     : in Tag_Node_Access;
                                     Attributes : in Tag_Attribute_Array_Access)
                                     return Tag_Node_Access is
      Node : constant Include_Tag_Node_Access := new Include_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Source     := Find_Attribute (Attributes, "src");
      if Node.Source = null then
         Node.Error ("Missing 'src' attribute");
      end if;
      return Node.all'Access;
   end Create_Include_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Include_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
      use EL.Objects;
      use EL.Variables;

      Ctx    : constant EL.Contexts.ELContext_Access := Context.Get_ELContext;
      Old    : constant access Variable_Mapper'Class := Ctx.Get_Variable_Mapper;
   begin
      if Node.Source = null then
         return;
      end if;
      declare
         Source : constant String := To_String (Get_Value (Node.Source.all, Context));
         Mapper : aliased Default.Default_Variable_Mapper;
      begin
         --  Chain the variable mapper with the previous one.
         if Old /= null then
            Mapper.Set_Next_Variable_Mapper (Old.all'Unchecked_Access);
         end if;

         --  Set a variable mapper for the include context.
         --  The <ui:param> variables will be declared in that mapper.
         Ctx.all.Set_Variable_Mapper (Mapper'Unchecked_Access);

         --  Build the children to take into account the <ui:param> nodes.
         Node.Build_Children (Parent, Context);
         Context.Include_Facelet (Source => Source, Parent => Parent);

      exception
         when E : others =>
            Node.Error ("Exception {0} while including {1}",
                        Ada.Exceptions.Exception_Message (E), Source);
            Ctx.all.Set_Variable_Mapper (Old);
            raise;
      end;
      Ctx.all.Set_Variable_Mapper (Old);
   end Build_Components;

   --  ------------------------------
   --  Composition Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Composition Tag
   --  ------------------------------
   function Create_Composition_Tag_Node (Binding    : in Binding_Type;
                                         Line       : in Line_Info;
                                         Parent     : in Tag_Node_Access;
                                         Attributes : in Tag_Attribute_Array_Access)
                                         return Tag_Node_Access is
      Node : constant Composition_Tag_Node_Access := new Composition_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Template   := Find_Attribute (Attributes, "template");
      return Node.all'Access;
   end Create_Composition_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Composition_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
      use EL.Objects;
      use EL.Variables;

      Ctx    : constant EL.Contexts.ELContext_Access := Context.Get_ELContext;
      Old    : constant access Variable_Mapper'Class := Ctx.Get_Variable_Mapper;
      Mapper : aliased Default.Default_Variable_Mapper;
   begin
      --  Chain the variable mapper with the previous one.
      if Old /= null then
         Mapper.Set_Next_Variable_Mapper (Old.all'Unchecked_Access);
      end if;
      Context.Push_Defines (Node);

      --  Set a variable mapper for the include context.
      --  The <ui:param> variables will be declared in that mapper.
      Ctx.all.Set_Variable_Mapper (Mapper'Unchecked_Access);
      begin
         Node.Build_Children (Parent, Context);
         if Node.Template /= null then
            declare
               Source : constant String := To_String (Get_Value (Node.Template.all, Context));
            begin
               Log.Info ("Include facelet {0}", Source);
               Context.Include_Facelet (Source => Source, Parent => Parent);
            end;
         end if;
      exception
         when E : others =>
            Node.Error ("Exception {0} while expanding composition",
                        Ada.Exceptions.Exception_Message (E));
            Ctx.all.Set_Variable_Mapper (Old);
            Context.Pop_Defines;
            raise;
      end;
      Ctx.all.Set_Variable_Mapper (Old);
      Context.Pop_Defines;
   end Build_Components;

   --  ------------------------------
   --  Freeze the tag node tree and perform any initialization steps
   --  necessary to build the components efficiently.  After this call
   --  the tag node tree should not be modified and it represents a read-only
   --  tree.
   --  ------------------------------
   overriding
   procedure Freeze (Node : access Composition_Tag_Node) is
      Child  : Tag_Node_Access := Node.First_Child;
      Define : Define_Tag_Node_Access := null;
   begin
      while Child /= null loop
         if Child.all in Define_Tag_Node'Class then
            Define := Define_Tag_Node (Child.all)'Access;
            Node.Defines.Insert (To_String (Define.Define_Name), Define);
         end if;
         Child := Child.Next;
      end loop;
   end Freeze;

   --  ------------------------------
   --  Include in the component tree the definition identified by the name.
   --  Upon completion, return in <b>Found</b> whether the definition was found
   --  within this composition context.
   --  ------------------------------
   procedure Include_Definition (Node    : access Composition_Tag_Node;
                                 Parent  : in UIComponent_Access;
                                 Context : in out Facelet_Context'Class;
                                 Name    : in Unbounded_String;
                                 Found   : out Boolean) is
      Pos : constant Define_Maps.Cursor := Node.Defines.Find (To_String (Name));
      Old : Unbounded_String;
   begin
      if not Define_Maps.Has_Element (Pos) then
         Found := False;
      else
         Found := True;
         declare
            File : constant String := Node.Get_File_Name;
         begin
            Context.Set_Relative_Path (Path     => File,
                                       Previous => Old);
            Define_Maps.Element (Pos).Build_Children (Parent, Context);

         exception
            when E : others =>
               Node.Error ("Error when inserting definition: {0}",
                           Ada.Exceptions.Exception_Message (E));
         end;
         Context.Set_Relative_Path (Path => Old);
      end if;
   end Include_Definition;

   --  ------------------------------
   --  Create the Debug Tag
   --  ------------------------------
   function Create_Debug_Tag_Node (Binding    : in Binding_Type;
                                   Line       : in Line_Info;
                                   Parent     : in Tag_Node_Access;
                                   Attributes : in Tag_Attribute_Array_Access)
                                   return Tag_Node_Access is
      Node : constant Debug_Tag_Node_Access := new Debug_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      return Node.all'Access;
   end Create_Debug_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Debug_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
   begin
      null;
   end Build_Components;

   --  ------------------------------
   --  Decorate Tag
   --  ------------------------------

   --  Create the Decorate Tag
   function Create_Decorate_Tag_Node (Binding    : in Binding_Type;
                                      Line       : in Line_Info;
                                      Parent     : in Tag_Node_Access;
                                      Attributes : in Tag_Attribute_Array_Access)
                                      return Tag_Node_Access is
      Node : constant Decorate_Tag_Node_Access := new Decorate_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Template   := Find_Attribute (Attributes, "template");
      if Node.Template = null then
         Node.Error ("Missing attribute 'template' on the decorator");
      end if;
      return Node.all'Access;
   end Create_Decorate_Tag_Node;

   --  ------------------------------
   --  Create the Define Tag
   --  ------------------------------
   function Create_Define_Tag_Node (Binding    : in Binding_Type;
                                    Line       : in Line_Info;
                                    Parent     : in Tag_Node_Access;
                                    Attributes : in Tag_Attribute_Array_Access)
                                    return Tag_Node_Access is
      Node : Define_Tag_Node_Access;
      Attr : constant Tag_Attribute_Access := Find_Attribute (Attributes, "name");
   begin
      if Attr = null then
         Node := new Define_Tag_Node; --  (Len => 0);
         Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
         Node.Error ("Missing attribute 'name' on node");
      else
         Node := new Define_Tag_Node; --  (Len => Attr.Value_Length);
         Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
         Node.Define_Name := Attr.Value;
      end if;
      return Node.all'Access;
   end Create_Define_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Define_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
   begin
      null;
   end Build_Components;

   --  ------------------------------
   --  Create the Insert Tag
   --  ------------------------------
   function Create_Insert_Tag_Node (Binding    : in Binding_Type;
                                    Line       : in Line_Info;
                                    Parent     : in Tag_Node_Access;
                                    Attributes : in Tag_Attribute_Array_Access)
                                    return Tag_Node_Access is
      Node : constant Insert_Tag_Node_Access := new Insert_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Insert_Name := Find_Attribute (Attributes, "name");
      return Node.all'Access;
   end Create_Insert_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Insert_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
      Name  : EL.Objects.Object;
      Found : Boolean;
   begin
      if Node.Insert_Name = null then
         Found := False;
      else
         Name := Get_Value (Node.Insert_Name.all, Context);
         Context.Include_Definition (EL.Objects.To_Unbounded_String (Name), Parent, Found);
      end if;

      --  If the definition was not found, insert the content of the <ui:insert> node.
      if not Found then
         Node.Build_Children (Parent, Context);
      end if;
   end Build_Components;

   --  ------------------------------
   --  Param Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Param Tag
   --  ------------------------------
   function Create_Param_Tag_Node (Binding    : in Binding_Type;
                                   Line       : in Line_Info;
                                   Parent     : in Tag_Node_Access;
                                   Attributes : in Tag_Attribute_Array_Access)
                                   return Tag_Node_Access is
      Node : constant Param_Tag_Node_Access := new Param_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Value      := Find_Attribute (Attributes, "value");
      if Node.Value = null then
         Node.Error ("Missing attribute 'value'");
      end if;
      Node.Var        := Find_Attribute (Attributes, "name");
      if Node.Var = null then
         Node.Error ("Missing attribute 'name'");
      end if;
      return Node.all'Access;
   end Create_Param_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Param_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
      pragma Unreferenced (Parent);
   begin
      if Node.Value /= null and then Node.Var /= null then
         declare
            Value  : constant EL.Expressions.Expression
              := Get_Expression (Node.Value.all);
         begin
            Context.Set_Variable (Node.Var.Value, Value);
         end;
      end if;
   end Build_Components;

   --  ------------------------------
   --  Comment Tag
   --  ------------------------------

   --  Create the Comment Tag
   function Create_Comment_Tag_Node (Binding    : in Binding_Type;
                                     Line       : in Line_Info;
                                     Parent     : in Tag_Node_Access;
                                     Attributes : in Tag_Attribute_Array_Access)
                                     return Tag_Node_Access is
      Node : constant Comment_Tag_Node_Access := new Comment_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Condition := Find_Attribute (Attributes, "condition");
      return Node.all'Access;
   end Create_Comment_Tag_Node;

   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   overriding
   procedure Build_Components (Node    : access Comment_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Facelet_Context'Class) is
   begin
      null;
   end Build_Components;

end ASF.Views.Nodes.Facelets;
