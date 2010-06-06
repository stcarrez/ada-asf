-----------------------------------------------------------------------
--  html -- ASF HTML Components
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
package body ASF.Components.Html.Text is

   use EL.Objects;

   --  ------------------------------
   --  Get the value to write on the output.
   --  ------------------------------
   function Get_Value (UI    : in UIOutput) return EL.Objects.Object is
   begin
      return UI.Get_Attribute (UI.Get_Context.all, "value");
--        return UI.Value;
   end Get_Value;

   --  ------------------------------
   --  Set the value to write on the output.
   --  ------------------------------
   procedure Set_Value (UI    : in out UIOutput;
                        Value : in EL.Objects.Object) is
   begin
      UI.Value := Value;
   end Set_Value;

   procedure Encode_Begin (UI      : in UIOutput;
                           Context : in out Faces_Context'Class) is
      Writer : constant ResponseWriter_Access := Context.Get_Response_Writer;
      Escape : constant Object := UI.Get_Attribute (Context, "escape");
   begin
      Writer.Start_Optional_Element ("span");
      UI.Render_Attributes (Context, Writer);
      if Is_Null (Escape) or To_Boolean (Escape) then
         Writer.Write_Text (UI.Get_Value);
      else
         Writer.Write_Text (UI.Get_Value);
      end if;
      Writer.End_Optional_Element ("span");
   end Encode_Begin;

   procedure Encode_Begin (UI      : in UILabel;
                           Context : in out Faces_Context'Class) is
   begin
      null;
   end Encode_Begin;

end ASF.Components.Html.Text;
