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
with ASF.Utils;
package body ASF.Components.Html.Text is

   use EL.Objects;

   TEXT_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   LABEL_ATTRIBUTE_NAMES : Util.Strings.String_Set.Set;

   --  ------------------------------
   --  Get the local value of the component without evaluating
   --  the associated Value_Expression.
   --  ------------------------------
   overriding
   function Get_Local_Value (UI : in UIOutput) return EL.Objects.Object is
   begin
      return UI.Value;
   end Get_Local_Value;

   --  ------------------------------
   --  Get the value to write on the output.
   --  ------------------------------
   overriding
   function Get_Value (UI    : in UIOutput) return EL.Objects.Object is
   begin
      if not EL.Objects.Is_Null (UI.Value) then
         return UI.Value;
      else
         return UI.Get_Attribute (UI.Get_Context.all, "value");
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value to write on the output.
   --  ------------------------------
   overriding
   procedure Set_Value (UI    : in out UIOutput;
                        Value : in EL.Objects.Object) is
   begin
      UI.Value := Value;
   end Set_Value;

   --  ------------------------------
   --  Get the converter that is registered on the component.
   --  ------------------------------
   overriding
   function Get_Converter (UI : in UIOutput)
                           return access ASF.Converters.Converter'Class is
   begin
      return UI.Converter;
   end Get_Converter;

   --  ------------------------------
   --  Set the converter to be used on the component.
   --  ------------------------------
   overriding
   procedure Set_Converter (UI        : in out UIOutput;
                            Converter : access ASF.Converters.Converter'Class) is
   begin
      if Converter = null then
         UI.Converter := null;
      else
         UI.Converter := Converter.all'Unchecked_Access;
      end if;
   end Set_Converter;

   --  ------------------------------
   --  Get the value of the component and apply the converter on it if there is one.
   --  ------------------------------
   function Get_Formatted_Value (UI      : in UIOutput;
                                 Context : in Faces_Context'Class) return String is
      Value : constant EL.Objects.Object := UIOutput'Class (UI).Get_Value;
   begin
      if UI.Converter /= null then
         return UI.Converter.To_String (Context   => Context,
                                        Component => UI,
                                        Value     => Value);
      else
         return EL.Objects.To_String (Value);
      end if;
   end Get_Formatted_Value;

   procedure Write_Output (UI      : in UIOutput;
                           Context : in out Faces_Context'Class;
                           Value   : in String) is
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
                          Value   => UIOutput'Class (UI).Get_Formatted_Value (Context));
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
                          Value   => To_String (Result));
      end;
   end Encode_Begin;

begin
   Utils.Set_Text_Attributes (TEXT_ATTRIBUTE_NAMES);
   Utils.Set_Text_Attributes (LABEL_ATTRIBUTE_NAMES);
   Utils.Set_Interactive_Attributes (LABEL_ATTRIBUTE_NAMES);
end ASF.Components.Html.Text;
