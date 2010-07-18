-----------------------------------------------------------------------
--  nodes-facelets -- Facelets composition nodes
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
--    <ui:composition .../>
--
with Util.Log.Loggers;
with EL.Contexts;
with EL.Variables.Default;
package body ASF.Views.Nodes.Facelets is

   use ASF.Factory;

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Views.Nodes.Facelets");

   COMPOSITION_TAG  : aliased constant String := "composition";
   DECORATE_TAG    : aliased constant String := "decorate";
   DEFINE_TAG       : aliased constant String := "define";
   INCLUDE_TAG      : aliased constant String := "include";
   INSERT_TAG       : aliased constant String := "insert";

   URI           : aliased constant String := "http://java.sun.com/jsf/facelets";

   Tag_Bindings  : aliased constant ASF.Factory.Binding_Array
     := (
       (Name      => COMPOSITION_TAG'Access,
        Component => null,
        Tag       => Create_Composition_Tag_Node'Access),

       (Name      => DECORATE_TAG'Access,
        Component => null,
        Tag       => Create_Decorate_Tag_Node'Access),

       (Name      => DEFINE_TAG'Access,
        Component => null,
        Tag       => Create_Define_Tag_Node'Access),

       (Name      => INCLUDE_TAG'Access,
        Component => null,
        Tag       => Create_Include_Tag_Node'Access),

       (Name      => INSERT_TAG'Access,
        Component => null,
        Tag       => Create_Insert_Tag_Node'Access)
      );

   Tag_Factory   : aliased constant ASF.Factory.Factory_Bindings
     := (URI => URI'Access, Bindings => Tag_Bindings'Access);

   --  ------------------------------
   --  Tag factory for nodes defined in this package.
   --  ------------------------------
   function Definition return ASF.Factory.Factory_Bindings_Access is
   begin
      return Tag_Factory'Access;
   end Definition;

   --  ------------------------------
   --  Include Tag
   --  ------------------------------
   --  The <ui:include src="..."/>

   --  ------------------------------
   --  Create the Include Tag
   --  ------------------------------
   function Create_Include_Tag_Node (Name       : Unbounded_String;
                                     Parent     : Tag_Node_Access;
                                     Attributes : Tag_Attribute_Array_Access)
                                     return Tag_Node_Access is
      Node : constant Include_Tag_Node_Access := new Include_Tag_Node;
   begin
      Node.Name       := Name;
      Node.Parent     := Parent;
      Node.Attributes := Attributes;
      Node.Source     := Find_Attribute (Attributes, "src");
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
      Source : constant String := To_String (Get_Value (Node.Source.all, Context));
      Old    : constant access VariableMapper'Class := Ctx.Get_Variable_Mapper;
      Mapper : aliased Default.Default_Variable_Mapper;
   begin
      --  Set a variable mapper for the include context.
      --  The <ui:param> variables will be declared in that mapper.
      Ctx.all.Set_Variable_Mapper (Mapper'Unchecked_Access);
      Context.Include_Facelet (Source => Source, Parent => Parent);
      Ctx.all.Set_Variable_Mapper (Old);
   end Build_Components;

   --  ------------------------------
   --  Composition Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Composition Tag
   --  ------------------------------
   function Create_Composition_Tag_Node (Name       : Unbounded_String;
                                         Parent     : Tag_Node_Access;
                                         Attributes : Tag_Attribute_Array_Access)
                                         return Tag_Node_Access is
      Node : constant Composition_Tag_Node_Access := new Composition_Tag_Node;
   begin
      Node.Name       := Name;
      Node.Parent     := Parent;
      Node.Attributes := Attributes;
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
      Old    : constant access VariableMapper'Class := Ctx.Get_Variable_Mapper;
      Mapper : aliased Default.Default_Variable_Mapper;
   begin
      Context.Push_Defines (Node);
      --  Set a variable mapper for the include context.
      --  The <ui:param> variables will be declared in that mapper.
      Ctx.all.Set_Variable_Mapper (Mapper'Unchecked_Access);
      if Node.Template /= null then
         declare
            Source : constant String := To_String (Get_Value (Node.Template.all, Context));
         begin
            Log.Info ("Include facelet {0}", Source);
            Context.Include_Facelet (Source => Source, Parent => Parent);
         end;
      else
         Node.Build_Children (Parent, Context);
      end if;
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
            Node.Defines.Insert (Define.Define_Name, Define);
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
      Pos : constant Define_Maps.Cursor := Node.Defines.Find (Name);
   begin
      if not Define_Maps.Has_Element (Pos) then
         Found := False;
      else
         Found := True;
         Define_Maps.Element (Pos).Build_Children (Parent, Context);
      end if;
   end Include_Definition;

   --  ------------------------------
   --  Decorate Tag
   --  ------------------------------

   --  Create the Decorate Tag
   function Create_Decorate_Tag_Node (Name       : Unbounded_String;
                                      Parent     : Tag_Node_Access;
                                      Attributes : Tag_Attribute_Array_Access)
                                      return Tag_Node_Access is
      Node : constant Decorate_Tag_Node_Access := new Decorate_Tag_Node;
   begin
      Node.Name       := Name;
      Node.Parent     := Parent;
      Node.Attributes := Attributes;
      Node.Template   := Find_Attribute (Attributes, "template");
      if Node.Template = null then
         Log.Error ("Missing attribute 'template' on the decorator");
      end if;
      return Node.all'Access;
   end Create_Decorate_Tag_Node;

   --  ------------------------------
   --  Create the Define Tag
   --  ------------------------------
   function Create_Define_Tag_Node (Name       : Unbounded_String;
                                    Parent     : Tag_Node_Access;
                                    Attributes : Tag_Attribute_Array_Access)
                                    return Tag_Node_Access is
      Node : constant Define_Tag_Node_Access := new Define_Tag_Node;
      Attr : constant Tag_Attribute_Access := Find_Attribute (Attributes, "name");
   begin
      Node.Name       := Name;
      Node.Parent     := Parent;
      Node.Attributes := Attributes;
      if Attr = null then
         Log.Error ("Missing attribute 'name' on node");
      else
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
   function Create_Insert_Tag_Node (Name       : Unbounded_String;
                                    Parent     : Tag_Node_Access;
                                    Attributes : Tag_Attribute_Array_Access)
                                    return Tag_Node_Access is
      Node : constant Insert_Tag_Node_Access := new Insert_Tag_Node;
   begin
      Node.Name       := Name;
      Node.Parent     := Parent;
      Node.Attributes := Attributes;
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
      Name : constant EL.Objects.Object := Get_Value (Node.Insert_Name.all, Context);
      Found : Boolean;
   begin
      Context.Include_Definition (EL.Objects.To_Unbounded_String (Name), Parent, Found);

      --  If the definition was not found, insert the content of the <ui:insert> node.
      if not Found then
         Node.Build_Children (Parent, Context);
      end if;
   end Build_Components;

end ASF.Views.Nodes.Facelets;
