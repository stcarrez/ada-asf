-----------------------------------------------------------------------
--  components-util -- ASF Util Components
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with ASF.Views;
with ASF.Views.Nodes;
package body ASF.Components.Utils is

   function Get_Line_Info (UI : in Base.UIComponent'Class) return Views.Line_Info;

   --  ------------------------------
   --  Get the line information where the component is defined.
   --  ------------------------------
   function Get_Line_Info (UI : in Base.UIComponent'Class) return Views.Line_Info is
      Tag : constant access ASF.Views.Nodes.Tag_Node'Class := UI.Get_Tag;
   begin
      if Tag /= null then
         return Tag.Get_Line_Info;
      else
         return Tag.Get_Line_Info;
      end if;
   end Get_Line_Info;
   pragma Unreferenced (Get_Line_Info);

   --  ------------------------------
   --  Get the line information where the component is defined.
   --  ------------------------------
   function Get_Line_Info (UI : in Base.UIComponent'Class) return String is
      Tag : constant access ASF.Views.Nodes.Tag_Node'Class := UI.Get_Tag;
   begin
      if Tag /= null then
         return Tag.Get_Line_Info;
      else
         return Tag.Get_Line_Info;
      end if;
   end Get_Line_Info;

end ASF.Components.Utils;
