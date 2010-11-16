-----------------------------------------------------------------------
--  views.nodes.jsf -- JSF Core Tag Library
--  Copyright (C) 2010 Stephane Carrez
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
with ASF.Views.Nodes;
with ASF.Contexts.Facelets;

--  The <b>ASF.Views.Nodes.Jsf</b> package implements various JSF Core Tag
--  components which alter the component tree but don't need to create
--  new UI components.
package ASF.Views.Nodes.Jsf is

   --  ------------------------------
   --  Converter Tag
   --  ------------------------------
   --  The <b>Converter_Tag_Node</b> is created in the facelet tree when
   --  the <f:converter> element is found.  When building the component tree,
   --  we have to find the <b>Converter</b> object and attach it to the
   --  parent component.  The parent component must implement the <b>Value_Holder</b>
   --  interface.
   type Converter_Tag_Node is new Views.Nodes.Tag_Node with private;
   type Converter_Tag_Node_Access is access all Converter_Tag_Node'Class;

   --  Create the Converter Tag
   function Create_Converter_Tag_Node (Name       : Unbounded_String;
                                       Line       : Views.Nodes.Line_Info;
                                       Parent     : Views.Nodes.Tag_Node_Access;
                                       Attributes : Views.Nodes.Tag_Attribute_Array_Access)
                                       return Views.Nodes.Tag_Node_Access;

   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.  Get the specified converter and
   --  add it to the parent component.  This operation does not create any
   --  new UIComponent.
   overriding
   procedure Build_Components (Node    : access Converter_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class);

private

   type Converter_Tag_Node is new Views.Nodes.Tag_Node with record
      Converter : EL.Objects.Object;
   end record;

end ASF.Views.Nodes.Jsf;
