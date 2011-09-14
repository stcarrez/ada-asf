-----------------------------------------------------------------------
--  html.forms -- ASF HTML Form Components
--  Copyright (C) 2010, 2011 Stephane Carrez
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

with Util.Beans.Objects;
with Util.Log.Loggers;
with Ada.Exceptions;
with ASF.Utils;
with ASF.Converters;
with ASF.Components.Utils;
with ASF.Events.Actions;
with ASF.Applications.Main;
package body ASF.Components.Html.Forms is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Components.Html.Forms");

   FORM_ATTRIBUTE_NAMES   : Util.Strings.String_Set.Set;

   INPUT_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   TEXTAREA_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   --  ------------------------------
   --  Input Component
   --  ------------------------------

   --  ------------------------------
   --  Create an UIInput secret component
   --  ------------------------------
   function Create_Input_Secret return ASF.Components.Base.UIComponent_Access is
      Result : constant UIInput_Access := new UIInput;
   begin
      Result.Is_Secret := True;
      return Result.all'Access;
   end Create_Input_Secret;

   --  ------------------------------
   --  Check if this component has the required attribute set.
   --  ------------------------------
   function Is_Required (UI      : in UIInput;
                         Context : in Faces_Context'Class) return Boolean is
      Attr : constant EL.Objects.Object := UI.Get_Attribute (Name    => REQUIRED_NAME,
                                                             Context => Context);
   begin
      if EL.Objects.Is_Null (Attr) then
         return True;
      end if;
      return EL.Objects.To_Boolean (Attr);
   end Is_Required;

   --  ------------------------------
   --  Get the value of the component.  If the component has a submitted value, returns it.
   --  If the component has a local value which is not null, returns it.
   --  Otherwise, if we have a Value_Expression evaluate and returns the value.
   --  ------------------------------
   overriding
   function Get_Value (UI : in UIInput) return EL.Objects.Object is
   begin
      if not Util.Beans.Objects.Is_Null (UI.Submitted_Value) then
         return UI.Submitted_Value;
      else
         return Text.UIOutput (UI).Get_Value;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Render the input element.
   --  ------------------------------
   procedure Render_Input (UI      : in UIInput;
                           Context : in out Faces_Context'Class) is
      Writer : constant ResponseWriter_Access := Context.Get_Response_Writer;
      Value  : constant EL.Objects.Object := UIInput'Class (UI).Get_Value;
   begin
      Writer.Start_Element ("input");
      if UI.Is_Secret then
         Writer.Write_Attribute (Name => "type", Value => "password");
      else
         Writer.Write_Attribute (Name => "type", Value => "text");
      end if;
      Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
      if not EL.Objects.Is_Null (Value) then
         declare
            Convert : constant access Converters.Converter'Class
              := UIInput'Class (UI).Get_Converter;
         begin
            if Convert /= null and Util.Beans.Objects.Is_Null (UI.Submitted_Value) then
               Writer.Write_Attribute (Name  => "value",
                                       Value => Convert.To_String (Value => Value,
                                                                   Component => UI,
                                                                   Context => Context));
            else
               Writer.Write_Attribute (Name => "value", Value => Value);
            end if;
         end;
      end if;
      UI.Render_Attributes (Context, INPUT_ATTRIBUTE_NAMES, Writer);
      Writer.End_Element ("input");
   end Render_Input;

   overriding
   procedure Encode_Begin (UI      : in UIInput;
                           Context : in out Faces_Context'Class) is
   begin
      if UI.Is_Rendered (Context) then
         UI.Render_Input (Context);
      end if;
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
         if not UI.Is_Secret then
            Log.Info ("Set input parameter {0} -> {1}", Id, Val);
         end if;
         UI.Submitted_Value := UI.Convert_Value (Val, Context);
         UI.Is_Valid := True;

      exception
         when E : others =>
            UI.Is_Valid := False;
            Log.Info (Utils.Get_Line_Info (UI)
                      & ": Exception raised when converting value {0} for component {1}: {2}",
                      Val, To_String (Id), Ada.Exceptions.Exception_Name (E));
      end;
   end Process_Decodes;

   --  ------------------------------
   --  Perform the component tree processing required by the <b>Process Validations</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows:
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Validators</b> of all facets and children.
   --  <ul>
   --  ------------------------------
   procedure Process_Validators (UI      : in out UIInput;
                                 Context : in out Faces_Context'Class) is
   begin
      --  Do not validate the component nor its children if the component is not rendered.
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      UIInput'Class (UI).Validate (Context);
   end Process_Validators;

   --  ------------------------------
   --  Validate the submitted value.
   --  <ul>
   --     <li>Retreive the submitted value
   --     <li>If the value is null, exit without further processing.
   --     <li>Validate the value by calling <b>Validate_Value</b>
   --  </ul>
   --  ------------------------------
   procedure Validate (UI      : in out UIInput;
                       Context : in out Faces_Context'Class) is
   begin
      Log.Info ("validating input field {0}", UI.Get_Client_Id);

      if not EL.Objects.Is_Null (UI.Submitted_Value) then
         UIInput'Class (UI).Validate_Value (UI.Submitted_Value, Context);

         --  Render the response after the current phase if something is wrong.
         if not UI.Is_Valid then
            Context.Render_Response;
         end if;
      end if;
   end Validate;

   --  ------------------------------
   --  Set the <b>valid</b> property:
   --  <ul>
   --     <li>If the <b>required</b> property is true, ensure the
   --         value is not empty
   --     <li>Call the <b>Validate</b> procedure on each validator
   --         registered on this component.
   --     <li>Set the <b>valid</b> property if all validator passed.
   --  </ul>
   --  ------------------------------
   procedure Validate_Value (UI      : in out UIInput;
                             Value   : in EL.Objects.Object;
                             Context : in out Faces_Context'Class) is
      use type ASF.Validators.Validator_Access;
   begin
      if EL.Objects.Is_Empty (Value) and UI.Is_Required (Context) and UI.Is_Valid then
         UI.Add_Message (Name    => REQUIRED_MESSAGE_NAME,
                         Default => REQUIRED_MESSAGE_ID,
                         Arg1    => UI.Get_Label (Context),
                         Context => Context);
         UI.Is_Valid := False;
      end if;

      if UI.Is_Valid and not EL.Objects.Is_Empty (Value) then
         for I in UI.Validators'Range loop
            exit when UI.Validators (I).Validator = null;
            UI.Validators (I).Validator.Validate (Context, UI, Value);
         end loop;
      end if;

   exception
      when ASF.Validators.Invalid_Value =>
         UI.Is_Valid := False;
   end Validate_Value;

   overriding
   procedure Process_Updates (UI      : in out UIInput;
                              Context : in out Faces_Context'Class) is
      VE    : constant EL.Expressions.Value_Expression := UI.Get_Value_Expression (VALUE_NAME);
   begin
      if UI.Is_Valid then
         VE.Set_Value (Value => UI.Submitted_Value, Context => Context.Get_ELContext.all);
      end if;

   exception
      when E : others =>
         UI.Is_Valid := False;
         UI.Add_Message (CONVERTER_MESSAGE_NAME, "convert", Context);
         Log.Info (Utils.Get_Line_Info (UI)
                   & ": Exception raised when updating value {0} for component {1}: {2}",
                   EL.Objects.To_String (UI.Submitted_Value),
                   To_String (UI.Get_Client_Id), Ada.Exceptions.Exception_Name (E));
   end Process_Updates;

   --  ------------------------------
   --  Add the validator to be used on the component.  The ASF implementation limits
   --  to 5 the number of validators that can be set on a component (See UIInput).
   --  The validator instance will be freed when the editable value holder is deleted
   --  unless <b>Shared</b> is true.
   --  ------------------------------
   overriding
   procedure Add_Validator (UI        : in out UIInput;
                            Validator : in ASF.Validators.Validator_Access;
                            Shared    : in Boolean := False) is
      use type ASF.Validators.Validator_Access;
   begin
      for I in UI.Validators'Range loop
         if UI.Validators (I).Validator = null then
            UI.Validators (I).Validator := Validator;
            UI.Validators (I).Shared    := Shared;
            return;
         end if;
      end loop;
      Base.Log_Error (UI, "Too many validators added (max {0})",
                      Positive'Image (UI.Validators'Length));
   end Add_Validator;

   --  ------------------------------
   --  Render the textarea element.
   --  ------------------------------
   overriding
   procedure Render_Input (UI      : in UIInputTextarea;
                           Context : in out Faces_Context'Class) is
      Writer : constant ResponseWriter_Access := Context.Get_Response_Writer;
      Value  : constant EL.Objects.Object := UIInput'Class (UI).Get_Value;
   begin
      Writer.Start_Element ("textarea");
      Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
      UI.Render_Attributes (Context, TEXTAREA_ATTRIBUTE_NAMES, Writer);
      if not EL.Objects.Is_Null (Value) then
         declare
            Convert : constant access Converters.Converter'Class
              := UIInput'Class (UI).Get_Converter;
         begin
            if Convert /= null and Util.Beans.Objects.Is_Null (UI.Submitted_Value) then
               Writer.Write_Text (Text => Convert.To_String (Value => Value,
                                                             Component => UI,
                                                             Context => Context));
            else
               Writer.Write_Text (Value => Value);
            end if;
         end;
      end if;
      Writer.End_Element ("textarea");
   end Render_Input;

   --  ------------------------------
   --  Button Component
   --  ------------------------------

   --  ------------------------------
   --  Get the value to write on the output.
   --  ------------------------------
   function Get_Value (UI    : in UICommand) return EL.Objects.Object is
   begin
      return UI.Get_Attribute (UI.Get_Context.all, VALUE_NAME);
   end Get_Value;

   --  ------------------------------
   --  Set the value to write on the output.
   --  ------------------------------
   procedure Set_Value (UI    : in out UICommand;
                        Value : in EL.Objects.Object) is
   begin
      UI.Value := Value;
   end Set_Value;

   --  ------------------------------
   --  Get the action method expression to invoke if the command is pressed.
   --  ------------------------------
   function Get_Action_Expression (UI      : in UICommand;
                                   Context : in Faces_Context'Class)
                                   return EL.Expressions.Method_Expression is
      pragma Unreferenced (Context);
   begin
      return UI.Get_Method_Expression (Name => ACTION_NAME);
   end Get_Action_Expression;

   overriding
   procedure Process_Decodes (UI      : in out UICommand;
                              Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Id  : constant Unbounded_String := UI.Get_Client_Id;
         Val : constant String := Context.Get_Parameter (To_String (Id));
      begin
         Log.Info ("Check command input parameter {0} -> {1}", Id, Val);
         if Val /= "" then
            ASF.Events.Actions.Post_Event (UI     => UI,
                                           Method => UI.Get_Action_Expression (Context));
         end if;

      exception
         when EL.Expressions.Invalid_Expression =>
            null;
      end;
   end Process_Decodes;

   --  ------------------------------
   --  Broadcast the event to the event listeners installed on this component.
   --  Listeners are called in the order in which they were added.
   --  ------------------------------
   overriding
   procedure Broadcast (UI      : in out UICommand;
                        Event   : not null access ASF.Events.Faces_Event'Class;
                        Context : in out Faces_Context'Class) is
      pragma Unreferenced (UI);

      use ASF.Events.Actions;

      App  : constant access Applications.Main.Application'Class := Context.Get_Application;
      Disp : constant Action_Listener_Access := App.Get_Action_Listener;
   begin
      if Disp /= null and Event.all in Action_Event'Class then
         Disp.Process_Action (Event   => Action_Event (Event.all),
                              Context => Context);
      end if;
   end Broadcast;

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

   --  ------------------------------
   --  Check whether the form is submitted.
   --  ------------------------------
   function Is_Submitted (UI : in UIForm) return Boolean is
   begin
      return UI.Is_Submitted;
   end Is_Submitted;

   --  ------------------------------
   --  Called during the <b>Apply Request</b> phase to indicate that this
   --  form is submitted.
   --  ------------------------------
   procedure Set_Submitted (UI : in out UIForm) is
   begin
      UI.Is_Submitted := True;
   end Set_Submitted;

   --  Get the action URL to set on the HTML form
   function Get_Action (UI      : in UIForm;
                        Context : in Faces_Context'Class) return String is
      pragma Unreferenced (UI, Context);
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
      if Val /= "" then
         UIForm'Class (UI).Set_Submitted;
      end if;
   end Decode;

   overriding
   procedure Process_Decodes (UI      : in out UIForm;
                              Context : in out Faces_Context'Class) is
   begin
      --  Do not decode the component nor its children if the component is not rendered.
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      Base.UIComponent'Class (UI).Decode (Context);

      --  If the form is submitted, process the children.
      --  Otherwise, none of the parameters are for this form.
      if UI.Is_Submitted then
         Log.Info ("Decoding form {0}", UI.Get_Client_Id);

         UI.Decode_Children (Context);
      end if;
   end Process_Decodes;

begin
   ASF.Utils.Set_Text_Attributes (FORM_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Text_Attributes (INPUT_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Interactive_Attributes (INPUT_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Interactive_Attributes (FORM_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Input_Attributes (INPUT_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Textarea_Attributes (TEXTAREA_ATTRIBUTE_NAMES);
end ASF.Components.Html.Forms;
