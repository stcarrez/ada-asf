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
with ASF.Components.Base;
with ASF.Components.Html.Text;
with ASF.Events;
with EL.Objects;
with EL.Expressions;
package ASF.Components.Html.Forms is

   --  ------------------------------
   --  Input Component
   --  ------------------------------
   type UIInput is new Text.UIOutput with private;
   type UIInput_Access is access all UIInput'Class;

   --  Create an UIInput secret component
   function Create_Input_Secret return ASF.Components.Base.UIComponent_Access;

   --  Check if this component has the required attribute set.
   function Is_Required (UI      : in UIInput;
                         Context : in Faces_Context'Class) return Boolean;

   --  Get the value of the component.  If the component has a submitted value, returns it.
   --  If the component has a local value which is not null, returns it.
   --  Otherwise, if we have a Value_Expression evaluate and returns the value.
   overriding
   function Get_Value (UI    : in UIInput) return EL.Objects.Object;

   overriding
   procedure Encode_Begin (UI      : in UIInput;
                           Context : in out Faces_Context'Class);

   overriding
   procedure Process_Decodes (UI      : in out UIInput;
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
                        Event   : not null access ASF.Events.Faces_Event'Class;
                        Context : in out Faces_Context'Class);

   --  ------------------------------
   --  Label Component
   --  ------------------------------
   type UIForm is new UIHtmlComponent with private;

   --  Check whether the form is submitted.
   function Is_Submitted (UI : in UIForm) return Boolean;

   --  Called during the <b>Apply Request</b> phase to indicate that this
   --  form is submitted.
   procedure Set_Submitted (UI : in out UIForm);

   --  Get the action URL to set on the HTML form
   function Get_Action (UI      : in UIForm;
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

private

   type UIInput is new Text.UIOutput with record
      Submitted_Value : EL.Objects.Object;
      Is_Valid        : Boolean;
      Is_Secret       : Boolean := False;
   end record;

   type UICommand is new UIHtmlComponent with record
      Value : EL.Objects.Object;
   end record;

   type UIForm is new UIHtmlComponent with record
      Is_Submitted : Boolean := False;
   end record;

end ASF.Components.Html.Forms;
