-----------------------------------------------------------------------
--  html -- ASF HTML Components
--  Copyright (C) 2009, 2010, 2011, 2012, 2017, 2018, 2022 Stephane Carrez
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
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with EL.Objects;

with ASF.Components.Core;
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
                           return ASF.Converters.Converter_Access is
   begin
      return UI.Converter;
   end Get_Converter;

   --  ------------------------------
   --  Set the converter to be used on the component.
   --  ------------------------------
   overriding
   procedure Set_Converter (UI        : in out UIOutput;
                            Converter : in ASF.Converters.Converter_Access;
                            Release   : in Boolean := False) is
      use type ASF.Converters.Converter_Access;
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => ASF.Converters.Converter'Class,
                                        Name   => ASF.Converters.Converter_Access);
   begin
      if UI.Release_Converter then
         Free (UI.Converter);
      end if;
      if Converter = null then
         UI.Converter := null;
         UI.Release_Converter := False;
      else
         UI.Converter := Converter.all'Unchecked_Access;
         UI.Release_Converter := Release;
      end if;
   end Set_Converter;

   --  ------------------------------
   --  Get the value of the component and apply the converter on it if there is one.
   --  ------------------------------
   function Get_Formatted_Value (UI      : in UIOutput;
                                 Context : in Faces_Context'Class) return String is
      Value : constant EL.Objects.Object := UIOutput'Class (UI).Get_Value;
   begin
      return UIOutput'Class (UI).Get_Formatted_Value (Value, Context);
   end Get_Formatted_Value;

   --  ------------------------------
   --  Get the converter associated with the component
   --  ------------------------------
   function Get_Converter (UI      : in UIOutput;
                           Context : in Faces_Context'Class)
                           return access ASF.Converters.Converter'Class is
      use type ASF.Converters.Converter_Access;
      Result : constant ASF.Converters.Converter_Access := UIOutput'Class (UI).Get_Converter;
   begin
      if Result /= null then
         return Result;
      else
         declare
            Name : constant EL.Objects.Object
              := UIOutput'Class (UI).Get_Attribute (Name    => CONVERTER_NAME,
                                                    Context => Context);
         begin
            return Context.Get_Converter (Name);
         end;
      end if;
   end Get_Converter;

   --  ------------------------------
   --  Format the value by applying the To_String converter on it if there is one.
   --  ------------------------------
   function Get_Formatted_Value (UI      : in UIOutput;
                                 Value   : in Util.Beans.Objects.Object;
                                 Context : in Contexts.Faces.Faces_Context'Class) return String is
      use type ASF.Converters.Converter_Access;
   begin
      if UI.Converter /= null then
         return UI.Converter.To_String (Context   => Context,
                                        Component => UI,
                                        Value     => Value);
      else
         declare
            Converter : constant access ASF.Converters.Converter'Class
              := UI.Get_Converter (Context);
         begin
            if Converter /= null then
               return Converter.To_String (Context   => Context,
                                           Component => UI,
                                           Value     => Value);
            elsif not Is_Null (Value) then
               return EL.Objects.To_String (Value);

            else
               return "";
            end if;
         end;
      end if;

      --  If the converter raises an exception, report an error in the logs.
      --  At this stage, we know the value and we can report it in this log message.
      --  We must queue the exception in the context, so this is done later.
   exception
      when E : others =>
         UI.Log_Error ("Error when converting value '{0}': {1}: {2}",
                       Util.Beans.Objects.To_String (Value),
                       Ada.Exceptions.Exception_Name (E),
                       Ada.Exceptions.Exception_Message (E));
         raise;
   end Get_Formatted_Value;

   procedure Write_Output (UI      : in UIOutput;
                           Context : in out Faces_Context'Class;
                           Value   : in String) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
      Escape : constant Object := UI.Get_Attribute (Context, "escape");
   begin
      Writer.Start_Optional_Element ("span");
      UI.Render_Attributes (Context, TEXT_ATTRIBUTE_NAMES, Writer);
      if Is_Null (Escape) or else To_Boolean (Escape) then
         Writer.Write_Text (Value);
      else
         Writer.Write_Raw (Value);
      end if;
      Writer.End_Optional_Element ("span");
   end Write_Output;

   overriding
   procedure Encode_Begin (UI      : in UIOutput;
                           Context : in out Faces_Context'Class) is
   begin
      if UI.Is_Rendered (Context) then
         UI.Write_Output (Context => Context,
                          Value   => UIOutput'Class (UI).Get_Formatted_Value (Context));

      end if;

      --  Queue any block converter exception that could be raised.
   exception
      when E : others =>
         Context.Queue_Exception (E);
   end Encode_Begin;

   overriding
   procedure Finalize (UI : in out UIOutput) is
   begin
      UI.Set_Converter (null);
      UIHtmlComponent (UI).Finalize;
   end Finalize;

   --  ------------------------------
   --  Label Component
   --  ------------------------------

   overriding
   procedure Encode_Begin (UI      : in UIOutputLabel;
                           Context : in out Faces_Context'Class) is
      Writer : Response_Writer_Access;
   begin
      if UI.Is_Rendered (Context) then
         Writer := Context.Get_Response_Writer;
         Writer.Start_Element ("label");
         UI.Render_Attributes (Context, LABEL_ATTRIBUTE_NAMES, Writer);
         declare
            Value : Util.Beans.Objects.Object := UI.Get_Attribute (Name    => "for",
                                                                   Context => Context);
         begin
            if not Util.Beans.Objects.Is_Null (Value) then
               Writer.Write_Attribute ("for", Value);
            end if;

            Value := UIOutputLabel'Class (UI).Get_Value;
            if not Util.Beans.Objects.Is_Null (Value) then
               declare
                  S : constant String
                    := UIOutputLabel'Class (UI).Get_Formatted_Value (Value, Context);
               begin
                  if UI.Get_Attribute (Name => "escape", Context => Context, Default => True) then
                     Writer.Write_Text (S);
                  else
                     Writer.Write_Raw (S);
                  end if;
               end;
            end if;

            --  Queue and block any converter exception that could be raised.
         exception
            when E : others =>
               Context.Queue_Exception (E);
         end;
      end if;
   end Encode_Begin;

   overriding
   procedure Encode_End (UI      : in UIOutputLabel;
                         Context : in out Faces_Context'Class) is
      Writer : Response_Writer_Access;
   begin
      if UI.Is_Rendered (Context) then
         Writer := Context.Get_Response_Writer;
         Writer.End_Element ("label");
      end if;
   end Encode_End;

   --  ------------------------------
   --  OutputFormat Component
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIOutputFormat;
                           Context : in out Faces_Context'Class) is
      use ASF.Components.Core;
      use ASF.Utils;
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      declare
         Params : constant UIParameter_Access_Array := Get_Parameters (UI);
         Values : ASF.Utils.Object_Array (Params'Range);
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
