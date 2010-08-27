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
with ASF.Components.Core;
with Util.Texts.Formats;
package body ASF.Components.Html.Text is

   use EL.Objects;

   TEXT_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   LABEL_ATTRIBUTE_NAMES : Util.Strings.String_Set.Set;

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

   procedure Write_Output (UI      : in UIOutput;
                           Context : in out Faces_Context'Class;
                           Value   : in EL.Objects.Object) is
      Writer : constant ResponseWriter_Access := Context.Get_Response_Writer;
      Escape : constant Object := UI.Get_Attribute (Context, "escape");
   begin
      Writer.Start_Optional_Element ("span");
      UI.Render_Attributes (Context, TEXT_ATTRIBUTE_NAMES, Writer);
      if Is_Null (Escape) or To_Boolean (Escape) then
         Writer.Write_Text (Value);
      else
         Writer.Write_Text (Value);
      end if;
      Writer.End_Optional_Element ("span");
   end Write_Output;

   procedure Encode_Begin (UI      : in UIOutput;
                           Context : in out Faces_Context'Class) is
   begin
      if UI.Is_Rendered (Context) then
         UI.Write_Output (Context => Context,
                          Value   => UI.Get_Value);
      end if;
   end Encode_Begin;

   procedure Encode_Begin (UI      : in UILabel;
                           Context : in out Faces_Context'Class) is
      Writer : ResponseWriter_Access;
   begin
      if UI.Is_Rendered (Context) then
         Writer := Context.Get_Response_Writer;
         Writer.Start_Element ("label");
         UI.Render_Attributes (Context, TEXT_ATTRIBUTE_NAMES, Writer);
      end if;
   end Encode_Begin;

   procedure Encode_End (UI      : in UILabel;
                         Context : in out Faces_Context'Class) is
      Writer : ResponseWriter_Access;
   begin
      if UI.Is_Rendered (Context) then
         Writer := Context.Get_Response_Writer;
         Writer.End_Element ("label");
      end if;
   end Encode_End;

   type Object_Array is array (Positive range <>) of EL.Objects.Object;

   package Formats is
     new Util.Texts.Formats (Stream => Ada.Strings.Unbounded.Unbounded_String,
                               Char   => Character,
                               Input  => String,
                               Value  => EL.Objects.Object,
                               Value_List => Object_Array,
                               Put        => Ada.Strings.Unbounded.Append,
                               To_Input   => EL.Objects.To_String);

   procedure Encode_Begin (UI      : in UIOutputFormat;
                           Context : in out Faces_Context'Class) is
      use ASF.Components.Core;
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      declare
         Params : constant UIParameter_Access_Array := Get_Parameters (UI);
         Values : Object_Array (Params'Range);
         Result : Ada.Strings.Unbounded.Unbounded_String;
         Fmt    : constant String := EL.Objects.To_String (UI.Get_Value);
      begin
         --  Get the values associated with the parameters.
         for I in Params'Range loop
            Values (I) := Params (I).Get_Value (Context);
         end loop;
         Formats.Format (Fmt, Values, Result);
         UI.Write_Output (Context => Context,
                          Value   => EL.Objects.To_Object (Result));
      end;
   end Encode_Begin;

begin
   Set_Text_Attributes (TEXT_ATTRIBUTE_NAMES);
   Set_Text_Attributes (LABEL_ATTRIBUTE_NAMES);
   Set_Interactive_Attributes (LABEL_ATTRIBUTE_NAMES);
end ASF.Components.Html.Text;
