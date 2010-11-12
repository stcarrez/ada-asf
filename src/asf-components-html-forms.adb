-----------------------------------------------------------------------
--  html.forms -- ASF HTML Form Components
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
with ASF.Components.Html.Text;
package body ASF.Components.Html.Forms is

   FORM_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   INPUT_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   --  ------------------------------
   --  Input Component
   --  ------------------------------

   overriding
   procedure Encode_Begin (UI      : in UIInput;
                           Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer : constant ResponseWriter_Access := Context.Get_Response_Writer;
         Value  : constant EL.Objects.Object := UI.Get_Value;
      begin
         Writer.Start_Element ("input");
         Writer.Write_Attribute (Name => "type", Value => "text");
         Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
         if not EL.Objects.Is_Null (Value) then
            Writer.Write_Attribute (Name => "value", Value => Value);
         end if;
         UI.Render_Attributes (Context, INPUT_ATTRIBUTE_NAMES, Writer);
         Writer.End_Element ("input");
      end;
   end Encode_Begin;

   --  ------------------------------
   --  Button Component
   --  ------------------------------

   --  ------------------------------
   --  Get the value to write on the output.
   --  ------------------------------
   function Get_Value (UI    : in UICommand) return EL.Objects.Object is
   begin
      return UI.Get_Attribute (UI.Get_Context.all, "value");
   end Get_Value;

   --  ------------------------------
   --  Set the value to write on the output.
   --  ------------------------------
   procedure Set_Value (UI    : in out UICommand;
                        Value : in EL.Objects.Object) is
   begin
      UI.Value := Value;
   end Set_Value;

   overriding
   procedure Encode_Begin (UI      : in UICommand;
                           Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer : constant ResponseWriter_Access := Context.Get_Response_Writer;
         Value  : constant EL.Objects.Object := UI.Get_Value;
      begin
         Writer.Start_Element ("input");
         Writer.Write_Attribute (Name => "type", Value => "submit");
         Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
         if not EL.Objects.Is_Null (Value) then
            Writer.Write_Attribute (Name => "value", Value => Value);
         end if;
         UI.Render_Attributes (Context, INPUT_ATTRIBUTE_NAMES, Writer);
         Writer.End_Element ("input");
      end;
   end Encode_Begin;

   --  ------------------------------
   --  Form Component
   --  ------------------------------

   --  Get the action URL to set on the HTML form
   function Get_Action (UI      : in UIForm;
                        Context : in Faces_Context'Class) return String is
   begin
      return "";
   end Get_Action;

   overriding
   procedure Encode_Begin (UI      : in UIForm;
                           Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer : constant ResponseWriter_Access := Context.Get_Response_Writer;
      begin
         Writer.Start_Element ("form");
         Writer.Write_Attribute (Name => "method", Value => "post");
         Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
         Writer.Write_Attribute (Name => "action", Value => UI.Get_Action (Context));
         UI.Render_Attributes (Context, FORM_ATTRIBUTE_NAMES, Writer);

      end;
   end Encode_Begin;

   overriding
   procedure Encode_End (UI      : in UIForm;
                         Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer : constant ResponseWriter_Access := Context.Get_Response_Writer;
      begin
         Writer.End_Element ("form");
      end;
   end Encode_End;

begin
   Set_Text_Attributes (FORM_ATTRIBUTE_NAMES);
   Set_Text_Attributes (INPUT_ATTRIBUTE_NAMES);
   Set_Interactive_Attributes (INPUT_ATTRIBUTE_NAMES);
   Set_Interactive_Attributes (FORM_ATTRIBUTE_NAMES);
   Set_Input_Attributes (INPUT_ATTRIBUTE_NAMES);
end ASF.Components.Html.Forms;
