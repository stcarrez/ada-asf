-----------------------------------------------------------------------
--  asf-validators -- ASF Validators
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
with EL.Objects;
with ASF.Components.Base;
with ASF.Contexts.Faces;

--  = Validators =
--  The validators participate in the validation of submitted values during
--  the request validation phase.  They are responsible for checking whether the
--  input parameter complies with the validation rules associated with the input
--  field.  The validator is expected to raise an exception and an error message
--  is then associated with the faulty input field.
--
--  The validator is described in the XHTML file by using one of the following
--  JSF core components:
--
--  | Component              | Validation type                                     |
--  |------------------------|-----------------------------------------------------|
--  |  f:validateLength      | The input field must have a minimun/maximum length  |
--  |  f:validateLongRange   | The input field must be a number in the given range |
--  |  f:validator           | An Ada registered or custom validator is used       |
--  |  f:validateRegex       | The input field must match the regular expression   |
--
--  A validator instance must implement the `ASF.Validators.Validator` interface.
--  It only needs to implement the `Validate` procedure which gets the UI
--  component, the faces request context and the submitted value.  It must verify
--  the value according to the validator's rule and the UI component.  When the
--  value is incorrect, it must set an error message in the UI component so that
--  some user friendly message is reported.  In case of error, it must also
--  raise the `Invalid_Value` exception.
--
--  @include asf-validators-texts.ads
--  @include asf-validators-numbers.ads
--
package ASF.Validators is

   Invalid_Value : exception;

   --  ------------------------------
   --  Validator
   --  ------------------------------
   --  The <b>Validator</b> implements a procedure to verify the validity of
   --  an input parameter.  The <b>validate</b> procedure is called after the
   --  converter.  The validator instance must be registered in
   --  the component factory (See <b>ASF.Factory.Component_Factory</b>).
   --  Unlike the Java implementation, the instance will be shared by multiple
   --  views and requests.  The validator is also responsible for adding the necessary
   --  error message in the faces context.
   type Validator is limited interface;
   type Validator_Access is access all Validator'Class;

   --  Verify that the value matches the validation rules defined by the validator.
   --  If some error are found, the procedure should create a <b>FacesMessage</b>
   --  describing the problem and add that message to the current faces context.
   --  The procedure can examine the state and modify the component tree.
   --  It must raise the <b>Invalid_Value</b> exception if the value is not valid.
   procedure Validate (Valid     : in Validator;
                       Context   : in out ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in out ASF.Components.Base.UIComponent'Class;
                       Value     : in EL.Objects.Object) is abstract;

end ASF.Validators;
