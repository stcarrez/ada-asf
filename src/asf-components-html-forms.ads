-----------------------------------------------------------------------
--  asf-components-html-forms -- ASF HTML Form Components
--  Copyright (C) 2010 - 2023 Stephane Carrez
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
with ASF.Components.Holders;
with ASF.Components.Html.Text;
with ASF.Validators;
with ASF.Events.Faces;
with EL.Objects;
with EL.Expressions;
package ASF.Components.Html.Forms is

   --  A UIComponent can have several validators.  To keep the implementation light and
   --  simple, a fixed array is used.  Most components will have one or two validators.
   --  Define the number of validators per component (UIInput).
   MAX_VALIDATORS_PER_COMPONENT : constant Positive := 5;

   --  Message displayed when the submitted value is required but is empty.
   REQUIRED_MESSAGE_ID          : constant String := "asf.faces.component.UIInput.REQUIRED";

   --  Message displayed when setting a value on the bean fails during the update values phase.
   ERROR_MESSAGE_ID             : constant String := "asf.faces.component.UIInput.ERROR";

   --  Message displayed when the form submussion contains an invalid CSRF token.
   EXPIRED_MESSAGE_ID           : constant String := "asf.faces.component.UIForm.EXPIRED";

   --  ------------------------------
   --  Form Component
   --  ------------------------------
   type UIForm is new UIHtmlComponent with private;
   type UIForm_Access is access all UIForm'Class;

   --  Check whether the form is submitted.
   function Is_Submitted (UI : in UIForm) return Boolean;

   --  Called during the <b>Apply Request</b> phase to indicate that this
   --  form is submitted.
   procedure Set_Submitted (UI : in out UIForm);

   --  Get the action URL to set on the HTML form
   function Get_Action (UI      : in UIForm;
                        Context : in Faces_Context'Class) return String;

   --  Get the CSRF token validity.  Returns 0 if the form has no CSRF token.
   function Get_Token_Validity (UI : in UIForm;
                                Context : in Faces_Context'Class) return Natural;

   --  Create the CSRF token for the form submission and the given form ID.
   function Create_Token (UI : in UIForm;
                          Id : in String;
                          Context : in Faces_Context'Class) return String;

   overriding
   procedure Encode_Begin (UI      : in UIForm;
                           Context : in out Faces_Context'Class);

   overriding
   procedure Encode_End (UI      : in UIForm;
                         Context : in out Faces_Context'Class);

   overriding
   procedure Decode (UI      : in out UIForm;
                     Context : in out Faces_Context'Class);

   overriding
   procedure Process_Decodes (UI      : in out UIForm;
                              Context : in out Faces_Context'Class);

   --  ------------------------------
   --  Input Component
   --  ------------------------------
   type UIInput is new Text.UIOutput and Holders.Editable_Value_Holder with private;
   type UIInput_Access is access all UIInput'Class;

   --  Check if this component has the required attribute set.
   function Is_Required (UI      : in UIInput;
                         Context : in Faces_Context'Class) return Boolean;

   --  Find the form component which contains the input component.
   --  Returns null if the input is not within a form component.
   function Get_Form (UI : in UIInput) return UIForm_Access;

   --  Get the value of the component.  If the component has a submitted value, returns it.
   --  If the component has a local value which is not null, returns it.
   --  Otherwise, if we have a Value_Expression evaluate and returns the value.
   overriding
   function Get_Value (UI    : in UIInput) return EL.Objects.Object;

   --  Convert the string into a value.  If a converter is specified on the component,
   --  use it to convert the value.
   function Convert_Value (UI      : in UIInput;
                           Value   : in String;
                           Context : in Faces_Context'Class) return EL.Objects.Object;

   --  Get the input parameter from the submitted context.  This operation is called by
   --  <tt>Process_Decodes</tt> to retrieve the request parameter associated with the component.
   function Get_Parameter (UI      : in UIInput;
                           Context : in Faces_Context'Class) return String;

   --  Set the input component as a password field.
   procedure Set_Secret (UI    : in out UIInput;
                         Value : in Boolean);

   --  Render the input element.
   procedure Render_Input (UI       : in UIInput;
                           Context  : in out Faces_Context'Class;
                           Write_Id : in Boolean := True);

   overriding
   procedure Encode_Begin (UI      : in UIInput;
                           Context : in out Faces_Context'Class);

   overriding
   procedure Process_Decodes (UI      : in out UIInput;
                              Context : in out Faces_Context'Class);

   --  Perform the component tree processing required by the <b>Process Validations</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows:
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Validators</b> of all facets and children.
   --  <ul>
   overriding
   procedure Process_Validators (UI      : in out UIInput;
                                 Context : in out Faces_Context'Class);

   overriding
   procedure Process_Updates (UI      : in out UIInput;
                              Context : in out Faces_Context'Class);

   --  Validate the submitted value.
   --  <ul>
   --     <li>Retreive the submitted value
   --     <li>If the value is null, exit without further processing.
   --     <li>Validate the value by calling <b>Validate_Value</b>
   --  </ul>
   procedure Validate (UI      : in out UIInput;
                       Context : in out Faces_Context'Class);

   --  Set the <b>valid</b> property:
   --  <ul>
   --     <li>If the <b>required</b> property is true, ensure the
   --         value is not empty
   --     <li>Call the <b>Validate</b> procedure on each validator
   --         registered on this component.
   --     <li>Set the <b>valid</b> property if all validator passed.
   --  </ul>
   procedure Validate_Value (UI      : in out UIInput;
                             Value   : in EL.Objects.Object;
                             Context : in out Faces_Context'Class);

   --  Add the validator to be used on the component.  The ASF implementation limits
   --  to 5 the number of validators that can be set on a component (See UIInput).
   --  The validator instance will be freed when the editable value holder is deleted
   --  unless <b>Shared</b> is true.
   overriding
   procedure Add_Validator (UI        : in out UIInput;
                            Validator : in ASF.Validators.Validator_Access;
                            Shared    : in Boolean := False);

   --  Delete the UI input instance.
   overriding
   procedure Finalize (UI : in out UIInput);

   --  ------------------------------
   --  InputTextarea Component
   --  ------------------------------
   type UIInputTextarea is new UIInput with private;
   type UIInputTextarea_Access is access all UIInputTextarea'Class;

   --  Render the textarea element.
   overriding
   procedure Render_Input (UI       : in UIInputTextarea;
                           Context  : in out Faces_Context'Class;
                           Write_Id : in Boolean := True);

   --  ------------------------------
   --  Input_Hidden Component
   --  ------------------------------
   type UIInput_Hidden is new UIInput with private;
   type UIInput_Hidden_Access is access all UIInput_Hidden'Class;

   --  Render the inputHidden element.
   overriding
   procedure Render_Input (UI       : in UIInput_Hidden;
                           Context  : in out Faces_Context'Class;
                           Write_Id : in Boolean := True);

   --  ------------------------------
   --  InputFile Component
   --  ------------------------------
   type UIInput_File is new UIInput with private;
   type UIInput_File_Access is access all UIInput_File'Class;

   --  Render the input file element.
   overriding
   procedure Render_Input (UI       : in UIInput_File;
                           Context  : in out Faces_Context'Class;
                           Write_Id : in Boolean := True);

   --  Validate the submitted file.
   --  <ul>
   --     <li>Retreive the submitted value
   --     <li>If the value is null, exit without further processing.
   --     <li>Validate the value by calling <b>Validate_Value</b>
   --  </ul>
   overriding
   procedure Validate (UI      : in out UIInput_File;
                       Context : in out Faces_Context'Class);

   overriding
   procedure Process_Updates (UI      : in out UIInput_File;
                              Context : in out Faces_Context'Class);

   --  ------------------------------
   --  Button Component
   --  ------------------------------
   type UICommand is new UIHtmlComponent with private;

   overriding
   procedure Encode_Begin (UI      : in UICommand;
                           Context : in out Faces_Context'Class);

   --  Get the value to write on the output.
   function Get_Value (UI    : in UICommand) return EL.Objects.Object;

   --  Set the value to write on the output.
   procedure Set_Value (UI    : in out UICommand;
                        Value : in EL.Objects.Object);

   --  Get the action method expression to invoke if the command is pressed.
   function Get_Action_Expression (UI      : in UICommand;
                                   Context : in Faces_Context'Class)
                                  return EL.Expressions.Method_Expression;

   overriding
   procedure Process_Decodes (UI      : in out UICommand;
                              Context : in out Faces_Context'Class);

   --  Broadcast the event to the event listeners installed on this component.
   --  Listeners are called in the order in which they were added.
   overriding
   procedure Broadcast (UI      : in out UICommand;
                        Event   : not null access ASF.Events.Faces.Faces_Event'Class;
                        Context : in out Faces_Context'Class);

private

   type Validator is record
      Validator : ASF.Validators.Validator_Access := null;
      Shared    : Boolean := False;
   end record;

   type Validator_Array is array (1 .. MAX_VALIDATORS_PER_COMPONENT) of Validator;

   type UIInput is new Text.UIOutput and Holders.Editable_Value_Holder with record
      Submitted_Value : EL.Objects.Object;
      Is_Valid        : Boolean := False;
      Is_Secret       : Boolean := False;
      Is_Submitted    : Boolean := False;
      Validators      : Validator_Array;
   end record;

   type UIInputTextarea is new UIInput with record
      Rows  : Natural;
      Cols  : Natural;
   end record;

   type UIInput_Hidden is new UIInput with null record;

   type UIInput_File is new UIInput with null record;

   type UICommand is new UIHtmlComponent with record
      Value : EL.Objects.Object;
   end record;

   type UIForm is new UIHtmlComponent with record
      Is_Submitted : Boolean := False;
      Is_Valid     : Boolean := False;
   end record;

end ASF.Components.Html.Forms;
