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
with EL.Contexts;
with EL.Variables.Default;
package body ASF.Views.Nodes.Facelets is

   use ASF.Factory;

   INCLUDE_TAG      : aliased constant String := "include";
   COMPOSITION_TAG  : aliased constant String := "composition";

   URI           : aliased constant String := "http://java.sun.com/jsf/facelets";

   Tag_Bindings  : aliased constant ASF.Factory.Binding_Array
     := (
       (Name => COMPOSITION_TAG'Access,
        Component => null,
        Tag => Create_Composition_Tag_Node'Access),

       (Name => INCLUDE_TAG'Access,
        Component => null,
        Tag => Create_Include_Tag_Node'Access)
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
      Node.Source     := Find_Attribute (Attributes, "template");
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

end ASF.Views.Nodes.Facelets;
