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

with ASF.Converters;
package body ASF.Views.Nodes.Jsf is

   use ASF;
   use EL.Objects;

   --  ------------------------------
   --  Converter Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Converter Tag
   --  ------------------------------
   function Create_Converter_Tag_Node (Name       : Unbounded_String;
                                       Line       : Views.Nodes.Line_Info;
                                       Parent     : Views.Nodes.Tag_Node_Access;
                                       Attributes : Views.Nodes.Tag_Attribute_Array_Access)
                                       return Views.Nodes.Tag_Node_Access is

      use ASF.Views.Nodes;

      Node : constant Converter_Tag_Node_Access := new Converter_Tag_Node;
      Conv : constant Tag_Attribute_Access := Find_Attribute (Attributes,
                                                              "converterId");
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
      if Conv = null then
         Node.Error ("Missing 'converterId' attribute");
      else
         Node.Converter := EL.Objects.To_Object (Conv.Value);
      end if;
      return Node.all'Access;
   end Create_Converter_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.  Get the specified converter and
   --  add it to the parent component.  This operation does not create any
   --  new UIComponent.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Converter_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class) is
      Cvt : constant access Converters.Converter'Class := Context.Get_Converter (Node.Converter);
   begin
      if not (Parent.all in Value_Holder'Class) then
         Node.Error ("Parent component is not an instance of Value_Holder");
         return;
      end if;

      if Cvt = null then
         Node.Error ("Converter was not found");
         return;
      end if;

      declare
         VH : constant access Value_Holder'Class := Value_Holder'Class (Parent.all)'Access;
      begin
         VH.Set_Converter (Converter => Cvt);
      end;
   end Build_Components;

end ASF.Views.Nodes.Jsf;
