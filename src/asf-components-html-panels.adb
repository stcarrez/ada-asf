-----------------------------------------------------------------------
--  html.panels -- Layout panels
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
with EL.Objects;
package body ASF.Components.Html.Panels is

   function Get_Layout (UI : UIPanelGroup;
                        Context : in Faces_Context'Class) return String is
      Value  : constant EL.Objects.Object := UI.Get_Attribute (Context, "layout");
      Layout : constant String := EL.Objects.To_String (Value);
   begin
      if Layout = "div" or Layout = "block" then
         return "div";
      elsif Layout = "none" then
         return "";
      else
         return "span";
      end if;
   end Get_Layout;

   procedure Encode_Begin (UI      : in UIPanelGroup;
                           Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
         Tag    : constant String := UI.Get_Layout (Context);
      begin
         if Tag'Length > 0 then
            Writer.Start_Optional_Element (Tag);
            UI.Render_Attributes (Context, Writer);
         end if;
      end;
   end Encode_Begin;

   procedure Encode_End (UI      : in UIPanelGroup;
                         Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
         Tag    : constant String := UI.Get_Layout (Context);
      begin
         if Tag'Length > 0 then
            Writer.End_Optional_Element (Tag);
         end if;
      end;
   end Encode_End;

end ASF.Components.Html.Panels;
