-----------------------------------------------------------------------
--  html -- ASF HTML Components
--  Copyright (C) 2009, 2010, 2012 Stephane Carrez
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
with Util.Strings;
with ASF.Utils;
package body ASF.Components.Html.Links is

   use EL.Objects;

   LINK_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   --  ------------------------------
   --  Get the value to write on the output.
   --  ------------------------------
   function Get_Value (UI    : in UIOutputLink) return EL.Objects.Object is
   begin
      return UI.Get_Attribute (UI.Get_Context.all, "value");
      --        return UI.Value;
   end Get_Value;

   --  ------------------------------
   --  Set the value to write on the output.
   --  ------------------------------
   procedure Set_Value (UI    : in out UIOutputLink;
                        Value : in EL.Objects.Object) is
   begin
      UI.Value := Value;
   end Set_Value;

   --  ------------------------------
   --  Get the link to be rendered in the <b>href</b> attribute.
   --  ------------------------------
   function Get_Link (UI      : in UIOutputLink;
                      Context : in Faces_Context'Class) return String is
      Value : constant String := UI.Get_Attribute ("value", Context,  "");
   begin
      return Value;
   end Get_Link;

   --  ------------------------------
   --  Encode the begining of the link.
   --  ------------------------------
   procedure Encode_Begin (UI      : in UIOutputLink;
                           Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
         Disabled : constant Boolean := UI.Get_Attribute ("disabled", Context, False);
      begin
         if Disabled then
            Writer.Start_Optional_Element ("span");
         else
            Writer.Start_Element ("a");
            Writer.Write_Attribute ("href", UIOutputLink'Class (UI).Get_Link (Context));
         end if;
         UI.Render_Attributes (Context, Writer);
      end;
   end Encode_Begin;

   --  ------------------------------
   --  Encode the end of the link.
   --  ------------------------------
   procedure Encode_End (UI      : in UIOutputLink;
                         Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Disabled : constant Boolean := UI.Get_Attribute ("disabled", Context, False);
         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
      begin
         if Disabled then
            Writer.End_Optional_Element ("span");
         else
            Writer.End_Element ("a");
         end if;
      end;
   end Encode_End;

begin
   Utils.Set_Text_Attributes (LINK_ATTRIBUTE_NAMES);
   Utils.Set_Interactive_Attributes (LINK_ATTRIBUTE_NAMES);
   Utils.Set_Link_Attributes (LINK_ATTRIBUTE_NAMES);
end ASF.Components.Html.Links;
