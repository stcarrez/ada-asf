-----------------------------------------------------------------------
--  components -- Component tree
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

--  The <bASF.Components</b> describes the components that form the
--  tree view.  Each component has attributes and children.  Children
--  represent sub-components and attributes control the rendering and
--  behavior of the component.
--
--  The component tree is created from the <b>ASF.Views</b> tag nodes
--  for each request.  Unlike tag nodes, the component tree is not shared.
with Ada.Strings.Unbounded;
with EL.Objects;
with EL.Expressions;
limited with ASF.Views.Nodes;
package ASF.Components is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Attribute of a component
   --  ------------------------------
   type UIAttribute is private;

private

   type UIAttribute_Access is access all UIAttribute;

   type UIAttribute is record
      Definition : access ASF.Views.Nodes.Tag_Attribute;
      Name       : Unbounded_String;
      Value      : EL.Objects.Object;
      Expr       : EL.Expressions.Expression;
      Next_Attr  : UIAttribute_Access;
   end record;

end ASF.Components;
