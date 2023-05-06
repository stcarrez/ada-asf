-----------------------------------------------------------------------
--  asf-components-html-forms -- ASF HTML Form Components
--  Copyright (C) 2010 - 2022 Stephane Carrez
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

with Ada.Unchecked_Deallocation;

with Util.Beans.Objects;
with Util.Log.Loggers;
with Ada.Exceptions;
with ASF.Utils;
with ASF.Parts;
with ASF.Parts.Upload_Method;
with ASF.Converters;
with ASF.Requests;
with ASF.Components.Base;
with ASF.Components.Utils;
with ASF.Components.Root;
with ASF.Events.Faces.Actions;
with ASF.Applications.Main;
with ASF.Applications.Views;
package body ASF.Components.Html.Forms is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Components.Html.Forms");

   FORM_ATTRIBUTE_NAMES      : Util.Strings.String_Set.Set;

   INPUT_ATTRIBUTE_NAMES     : Util.Strings.String_Set.Set;

   TEXTAREA_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   FILE_ATTRIBUTE_NAMES      : Util.Strings.String_Set.Set;

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => ASF.Validators.Validator'Class,
                                      Name   => ASF.Validators.Validator_Access);

   --  ------------------------------
   --  Input Component
   --  ------------------------------

   --  ------------------------------
   --  Find the form component which contains the input component.
   --  Returns null if the input is not within a form component.
   --  ------------------------------
   function Get_Form (UI : in UIInput) return UIForm_Access is
      use type ASF.Components.Base.UIComponent_Access;

      Parent : ASF.Components.Base.UIComponent_Access := UI.Get_Parent;
   begin
      while Parent /= null loop
         if Parent.all in UIForm'Class then
            return UIForm'Class (Parent.all)'Access;
         end if;
         Parent := Parent.Get_Parent;
      end loop;
      return null;
   end Get_Form;

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
   --  Set the input component as a password field.
   --  ------------------------------
   procedure Set_Secret (UI    : in out UIInput;
                         Value : in Boolean) is
   begin
      UI.Is_Secret := Value;
   end Set_Secret;

   --  ------------------------------
   --  Render the input element.
   --  ------------------------------
   procedure Render_Input (UI       : in UIInput;
                           Context  : in out Faces_Context'Class;
                           Write_Id : in Boolean := True) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
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
            if Convert /= null and then Util.Beans.Objects.Is_Null (UI.Submitted_Value) then
               Writer.Write_Attribute (Name  => "value",
                                       Value => Convert.To_String (Value => Value,
                                                                   Component => UI,
                                                                   Context => Context));
            else
               Writer.Write_Attribute (Name => "value", Value => Value);
            end if;
         end;
      end if;
      UI.Render_Attributes (Context, INPUT_ATTRIBUTE_NAMES, Writer, Write_Id);
      Writer.End_Element ("input");
   end Render_Input;

   overriding
   procedure Encode_Begin (UI      : in UIInput;
                           Context : in out Faces_Context'Class) is
   begin
      if UI.Is_Rendered (Context) then
         UIInput'Class (UI).Render_Input (Context);
      end if;
   end Encode_Begin;

   --  ------------------------------
   --  Get the input parameter from the submitted context.  This operation is called by
   --  <tt>Process_Decodes</tt> to retrieve the request parameter associated with the component.
   --  ------------------------------
   function Get_Parameter (UI      : in UIInput;
                           Context : in Faces_Context'Class) return String is
      Id  : constant Unbounded_String := UI.Get_Client_Id;
   begin
      return Context.Get_Parameter (To_String (Id));
   end Get_Parameter;

   --  ------------------------------
   --  Convert the string into a value.  If a converter is specified on the component,
   --  use it to convert the value.
   --  ------------------------------
   function Convert_Value (UI      : in UIInput;
                           Value   : in String;
                           Context : in Faces_Context'Class) return EL.Objects.Object is
      Convert : constant access ASF.Converters.Converter'Class
        := UIInput'Class (UI).Get_Converter (Context);
   begin
      if Convert = null then
         return EL.Objects.To_Object (Value);
      else
         return Convert.To_Object (Context   => Context,
                                   Component => UI,
                                   Value     => Value);
      end if;
   end Convert_Value;

   overriding
   procedure Process_Decodes (UI      : in out UIInput;
                              Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Id  : constant Unbounded_String := UI.Get_Client_Id;
         Val : constant String := UIInput'Class (UI).Get_Parameter (Context);
      begin
         --  SCz 2017-05-25: we don't know with AWS whether a parameter is
         --  submitted or not.  It returns the empty string when a parameter
         --  is not found.  If the value is empty and not required,
         --  don't set the valid flag so that we get the JSF behavior.
         if Val'Length = 0 and then not UI.Is_Required (Context) then
            return;
         end if;
         if not UI.Is_Secret then
            Log.Info ("Set input parameter {0} -> {1}", Id, Val);
         else
            Log.Info ("Set secret input parameter {0} -> XXXXXX", Id);
         end if;
         UI.Is_Submitted := True;
         UI.Submitted_Value := UIInput'Class (UI).Convert_Value (Val, Context);
         UI.Is_Valid := True;

      exception
         when E : others =>
            UI.Is_Valid := False;
            UI.Add_Message (Name    => CONVERTER_MESSAGE_NAME,
                            Default => ERROR_MESSAGE_ID,
                            Arg1    => UI.Get_Label (Context),
                            Context => Context);
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
   overriding
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
      Id : constant String := Ada.Strings.Unbounded.To_String (UI.Get_Client_Id);
   begin
      Log.Debug ("Validating input field {0}", Id);

      if UI.Is_Submitted then --  not EL.Objects.Is_Null (UI.Submitted_Value) then
         UIInput'Class (UI).Validate_Value (UI.Submitted_Value, Context);

         --  Render the response after the current phase if something is wrong.
         if not UI.Is_Valid then
            Log.Info ("{0}: submitted value for {1} is invalid", Utils.Get_Line_Info (UI), Id);
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
      if EL.Objects.Is_Empty (Value)
        and then UI.Is_Required (Context)
        and then UI.Is_Valid
      then
         UI.Add_Message (Name    => REQUIRED_MESSAGE_NAME,
                         Default => REQUIRED_MESSAGE_ID,
                         Arg1    => UI.Get_Label (Context),
                         Context => Context);
         UI.Is_Valid := False;
      end if;

      if UI.Is_Valid and then not EL.Objects.Is_Empty (Value) then
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
   begin
      if UI.Is_Valid then
         declare
            VE    : constant EL.Expressions.Value_Expression
              := UI.Get_Value_Expression (VALUE_NAME);
         begin
            VE.Set_Value (Value => UI.Submitted_Value, Context => Context.Get_ELContext.all);
         end;
      end if;

   exception
      when E : others =>
         UI.Is_Valid := False;
         UI.Add_Message (Name    => CONVERTER_MESSAGE_NAME,
                         Default => ERROR_MESSAGE_ID,
                         Context => Context);
         Log.Info (Utils.Get_Line_Info (UI)
                   & ": Exception raised when updating value {0} for component {1}: {2}",
                   EL.Objects.To_String (UI.Submitted_Value),
                   To_String (UI.Get_Client_Id), Ada.Exceptions.Exception_Name (E));

         --  Render the response after the current phase if something is wrong.
         Context.Render_Response;
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
      if Shared then
         declare
            V : ASF.Validators.Validator_Access := Validator;
         begin
            Free (V);
         end;
      end if;
   end Add_Validator;

   --  ------------------------------
   --  Delete the UI input instance.
   --  ------------------------------
   overriding
   procedure Finalize (UI : in out UIInput) is
      use type ASF.Validators.Validator_Access;
   begin
      --  If there are any validator that is not shared, delete the instance.
      for I in UI.Validators'Range loop
         exit when UI.Validators (I).Validator = null;
         if not UI.Validators (I).Shared then
            Free (UI.Validators (I).Validator);
         end if;
      end loop;
      UIHtmlComponent (UI).Finalize;
   end Finalize;

   --  ------------------------------
   --  Render the textarea element.
   --  ------------------------------
   overriding
   procedure Render_Input (UI       : in UIInputTextarea;
                           Context  : in out Faces_Context'Class;
                           Write_Id : in Boolean := True) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
      Value  : constant EL.Objects.Object := UIInput'Class (UI).Get_Value;
   begin
      Writer.Start_Element ("textarea");
      Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
      UI.Render_Attributes (Context, TEXTAREA_ATTRIBUTE_NAMES, Writer, Write_Id);
      if not EL.Objects.Is_Null (Value) then
         declare
            Convert : constant access Converters.Converter'Class
              := UIInput'Class (UI).Get_Converter;
         begin
            if Convert /= null and then Util.Beans.Objects.Is_Null (UI.Submitted_Value) then
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
   --  Input_Hidden Component
   --  ------------------------------
   --  Render the inputHidden element.
   overriding
   procedure Render_Input (UI       : in UIInput_Hidden;
                           Context  : in out Faces_Context'Class;
                           Write_Id : in Boolean := True) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
      Value  : constant EL.Objects.Object := UIInput'Class (UI).Get_Value;
   begin
      Writer.Start_Element ("input");
      Writer.Write_Attribute (Name => "type", Value => "hidden");
      Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
      if not EL.Objects.Is_Null (Value) then
         declare
            Convert : constant access Converters.Converter'Class
              := UIInput'Class (UI).Get_Converter;
         begin
            if Convert /= null and then Util.Beans.Objects.Is_Null (UI.Submitted_Value) then
               Writer.Write_Attribute (Name  => "value",
                                       Value => Convert.To_String (Value => Value,
                                                                   Component => UI,
                                                                   Context => Context));
            else
               Writer.Write_Attribute (Name => "value", Value => Value);
            end if;
         end;
      end if;
      if Write_Id and then not UI.Is_Generated_Id then
         Writer.Write_Attribute ("id", UI.Get_Client_Id);
      end if;
      Writer.End_Element ("input");
   end Render_Input;

   --  ------------------------------
   --  Render the input file element.
   --  ------------------------------
   overriding
   procedure Render_Input (UI       : in UIInput_File;
                           Context  : in out Faces_Context'Class;
                           Write_Id : in Boolean := True) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
   begin
      Writer.Start_Element ("input");
      UI.Render_Attributes (Context, FILE_ATTRIBUTE_NAMES, Writer, Write_Id);
      Writer.Write_Attribute (Name => "type", Value => "file");
      Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
      Writer.End_Element ("input");
   end Render_Input;

   --  ------------------------------
   --  Validate the submitted value.
   --  <ul>
   --     <li>Retreive the submitted value
   --     <li>If the value is null, exit without further processing.
   --     <li>Validate the value by calling <b>Validate_Value</b>
   --  </ul>
   --  ------------------------------
   overriding
   procedure Validate (UI      : in out UIInput_File;
                       Context : in out Faces_Context'Class) is
      procedure Process_Part (Part : in ASF.Parts.Part'Class);

      procedure Process_Part (Part : in ASF.Parts.Part'Class) is
         pragma Unreferenced (Part);
      begin
         UI.Is_Valid := True;
      end Process_Part;

      Req : constant ASF.Requests.Request_Access := Context.Get_Request;
      Id  : constant String := To_String (UI.Get_Client_Id);
   begin
      Log.Debug ("Validating input file {0}", Id);

      UI.Is_Valid := False;
      Req.Process_Part (Id, Process_Part'Access);

      if not UI.Is_Valid and then UI.Is_Required (Context) then
         UI.Add_Message (Name    => REQUIRED_MESSAGE_NAME,
                         Default => REQUIRED_MESSAGE_ID,
                         Arg1    => UI.Get_Label (Context),
                         Context => Context);
         UI.Is_Valid := False;

         --  Render the response after the current phase if something is wrong.
         Context.Render_Response;
      end if;
   end Validate;

   overriding
   procedure Process_Updates (UI      : in out UIInput_File;
                              Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         procedure Process_Part (Part : in ASF.Parts.Part'Class);

         Id  : constant String := To_String (UI.Get_Client_Id);
         Req : constant ASF.Requests.Request_Access := Context.Get_Request;
         ME  : constant EL.Expressions.Method_Expression
           := UI.Get_Method_Expression (VALUE_NAME);

         procedure Process_Part (Part : in ASF.Parts.Part'Class) is
         begin
            ASF.Parts.Upload_Method.Execute (ME, Part, Context.Get_ELContext.all);
         end Process_Part;

      begin
         Req.Process_Part (Id, Process_Part'Access);
         UI.Is_Valid := True;
      end;

   exception
      when E : others =>
         UI.Is_Valid := False;
         UI.Add_Message (Name    => CONVERTER_MESSAGE_NAME,
                         Default => ERROR_MESSAGE_ID,
                         Context => Context);
         Log.Info (Utils.Get_Line_Info (UI)
                   & ": Exception raised when updating value {0} for component {1}: {2}",
                   EL.Objects.To_String (UI.Submitted_Value),
                   To_String (UI.Get_Client_Id), Ada.Exceptions.Exception_Name (E));
         Context.Queue_Exception (E);
   end Process_Updates;

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
         if Val /= "" then
            Log.Info ("Command {0} was submitted with {1}", Id, Val);
            ASF.Events.Faces.Actions.Post_Event (UI     => UI,
                                                 Method => UI.Get_Action_Expression (Context));
         else
            Log.Debug ("Command {0} was not submitted", Id);
         end if;

      exception
         when E : EL.Expressions.Invalid_Expression =>
            Log.Info ("{0}: Command action looks invalid: {1}",
                      Utils.Get_Line_Info (UI), Ada.Exceptions.Exception_Message (E));
      end;
   end Process_Decodes;

   --  ------------------------------
   --  Broadcast the event to the event listeners installed on this component.
   --  Listeners are called in the order in which they were added.
   --  ------------------------------
   overriding
   procedure Broadcast (UI      : in out UICommand;
                        Event   : not null access ASF.Events.Faces.Faces_Event'Class;
                        Context : in out Faces_Context'Class) is
      pragma Unreferenced (UI);

      use ASF.Events.Faces.Actions;

      App  : constant access Applications.Main.Application'Class := Context.Get_Application;
      Disp : constant Action_Listener_Access := App.Get_Action_Listener;
   begin
      if Disp /= null and then Event.all in Action_Event'Class then
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
         Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
         Value  : constant EL.Objects.Object := UI.Get_Value;
      begin
         Writer.Start_Element ("input");
         Writer.Write_Attribute (Name => "type", Value => "submit");
         Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
         if not EL.Objects.Is_Null (Value) then
            Writer.Write_Attribute (Name => "value", Value => Value);
         end if;
         UI.Render_Attributes (Context, INPUT_ATTRIBUTE_NAMES, Writer);
         if Context.Is_Ajax_Request then
            Writer.Write_Attribute (Name  => "onclick",
                                    Value => "return ASF.Submit(this);");
         end if;
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

   --  ------------------------------
   --  Get the action URL to set on the HTML form
   --  ------------------------------
   function Get_Action (UI      : in UIForm;
                        Context : in Faces_Context'Class) return String is
      pragma Unreferenced (UI);

      App          : constant ASF.Contexts.Faces.Application_Access := Context.Get_Application;
      View_Handler : constant access Applications.Views.View_Handler'Class := App.Get_View_Handler;
      View         : constant ASF.Components.Root.UIViewRoot := Context.Get_View_Root;
   begin
      return View_Handler.Get_Action_URL (Context, ASF.Components.Root.Get_View_Id (View));
   end Get_Action;

   --  ------------------------------
   --  Get the CSRF token validity.  Returns 0 if the form has no CSRF token.
   --  ------------------------------
   function Get_Token_Validity (UI : in UIForm;
                                Context : in Faces_Context'Class) return Natural is
      Validity : constant String := UI.Get_Attribute ("validity", Context);
   begin
      if Validity = "all" then
         return 0;
      elsif Validity'Length = 0 then
         return 24 * 3600;
      else
         return Natural'Value (Validity);
      end if;

   exception
      when Constraint_Error =>
         UI.Log_Error ("Invalid validity value: '{0}'", Validity);
         return 24 * 3600;
   end Get_Token_Validity;

   --  ------------------------------
   --  Create the CSRF token for the form submission and the given form ID.
   --  ------------------------------
   function Create_Token (UI : in UIForm;
                          Id : in String;
                          Context : in Faces_Context'Class) return String is
      Validity : constant Natural := UI.Get_Token_Validity (Context);
   begin
      if Validity = 0 then
         return "1";
      end if;

      return Context.Create_Token (Id, Duration (Validity));
   end Create_Token;

   overriding
   procedure Encode_Begin (UI      : in UIForm;
                           Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
         Id     : constant Unbounded_String := UI.Get_Client_Id;
         Token  : constant String := UI.Create_Token (To_String (Id), Context);
      begin
         Writer.Start_Element ("form");
         Writer.Write_Attribute (Name => "method", Value => "post");
         Writer.Write_Attribute (Name => "name", Value => Id);
         Writer.Write_Attribute (Name => "action", Value => UI.Get_Action (Context));
         UI.Render_Attributes (Context, FORM_ATTRIBUTE_NAMES, Writer);

         Writer.Start_Element ("input");
         Writer.Write_Attribute (Name => "type", Value => "hidden");
         Writer.Write_Attribute (Name => "name", Value => Id);
         Writer.Write_Attribute (Name => "value", Value => Token);
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
         Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
      begin
         Writer.End_Element ("form");
      end;
   end Encode_End;

   overriding
   procedure Decode (UI      : in out UIForm;
                     Context : in out Faces_Context'Class) is
      Id    : constant Unbounded_String := UI.Get_Client_Id;
      Ident : constant String := To_String (Id);
      Val   : constant String := Context.Get_Parameter (Ident);
   begin
      if Val /= "" then
         declare
            Validity : constant Natural := UI.Get_Token_Validity (Context);
         begin
            Log.Debug ("Submission of form {0}", Id);
            if Validity > 0 and then not Context.Verify_Token (Ident, Val) then
               UI.Is_Valid := False;
               UI.Add_Message (Name    => EXPIRED_MESSAGE_NAME,
                               Default => EXPIRED_MESSAGE_ID,
                               Context => Context);
               UI.Log_Error ("Token verification failed for form '{0}'", Ident);
            else
               UI.Is_Valid := True;
            end if;
            UIForm'Class (UI).Set_Submitted;
         end;
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
      if not UI.Is_Submitted then
         Log.Debug ("Form {0} was not submitted", UI.Get_Client_Id);

      elsif UI.Is_Valid then
         Log.Info ("Decoding form {0}", UI.Get_Client_Id);

         UI.Decode_Children (Context);
      else
         Log.Info ("Submitted form {0} has invalid CSRF token", UI.Get_Client_Id);
      end if;
   end Process_Decodes;

begin
   ASF.Utils.Set_Text_Attributes (FORM_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Text_Attributes (INPUT_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Text_Attributes (TEXTAREA_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Interactive_Attributes (INPUT_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Interactive_Attributes (FORM_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Form_Attributes (FORM_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Input_Attributes (INPUT_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Textarea_Attributes (TEXTAREA_ATTRIBUTE_NAMES);
   ASF.Utils.Set_File_Attributes (FILE_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Interactive_Attributes (FILE_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Text_Attributes (FILE_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Input_Attributes (FILE_ATTRIBUTE_NAMES);
end ASF.Components.Html.Forms;
