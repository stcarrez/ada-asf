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
with Util.Log.Loggers;
with ASF.Components.Html.Text;
package body ASF.Components.Html.Forms is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Components.Html.Forms");

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

   overriding
   procedure Process_Decodes (UI      : in out UIInput;
                              Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Id  : constant Unbounded_String := UI.Get_Client_Id;
         Val : constant String := Context.Get_Parameter (To_String (Id));
      begin
         Log.Info ("Set input parameter {0} -> {1}", Id, Val);
         UI.Submitted_Value := EL.Objects.To_Object (Val);
      end;
   end Process_Decodes;

   overriding
   procedure Process_Updates (UI      : in out UIInput;
                              Context : in out Faces_Context'Class) is
      VE    : constant EL.Expressions.Value_Expression := UI.Get_Value_Expression ("value");
   begin
      VE.Set_Value (Value => UI.Submitted_Value, Context => Context.Get_ELContext.all);
   end Process_Updates;

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
         Id     : constant Unbounded_String := UI.Get_Client_Id;
      begin
         Writer.Start_Element ("form");
         Writer.Write_Attribute (Name => "method", Value => "post");
         Writer.Write_Attribute (Name => "name", Value => Id);
         Writer.Write_Attribute (Name => "action", Value => UI.Get_Action (Context));
         UI.Render_Attributes (Context, FORM_ATTRIBUTE_NAMES, Writer);

         Writer.Start_Element ("input");
         Writer.Write_Attribute (Name => "type", Value => "hidden");
         Writer.Write_Attribute (Name => "name", Value => Id);
         Writer.Write_Attribute (Name => "value", Value => "1");
         Writer.End_Element ("input");
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

   overriding
   procedure Decode (UI      : in out UIForm;
                     Context : in out Faces_Context'Class) is
      Id  : constant Unbounded_String := UI.Get_Client_Id;
      Val : constant String := Context.Get_Parameter (To_String (Id));
   begin
      UI.Is_Submitted := Val /= "";
   end Decode;

   overriding
   procedure Process_Decodes (UI      : in out UIForm;
                              Context : in out Faces_Context'Class) is
      Child : UIComponent_Access;
   begin
      --  Do not decode the component nor its children if the component is not rendered.
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      UIComponent'Class (UI).Decode (Context);

      --  If the form is submitted, process the children.
      --  Otherwise, none of the parameters are for this form.
      if UI.Is_Submitted then
         Log.Info ("Decoding form {0}", UI.Get_Client_Id);

         UI.Decode_Children (Context);
      end if;
   end Process_Decodes;

begin
   Set_Text_Attributes (FORM_ATTRIBUTE_NAMES);
   Set_Text_Attributes (INPUT_ATTRIBUTE_NAMES);
   Set_Interactive_Attributes (INPUT_ATTRIBUTE_NAMES);
   Set_Interactive_Attributes (FORM_ATTRIBUTE_NAMES);
   Set_Input_Attributes (INPUT_ATTRIBUTE_NAMES);
end ASF.Components.Html.Forms;
