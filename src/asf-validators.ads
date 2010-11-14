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
with ASF.Components;
with ASF.Contexts.Faces;

--  The <b>ASF.Validators</b> defines an interface used by the validation model
--  to check during the validation phase that the submitted values are valid.
--
--  See JSR 314 - JavaServer Faces Specification 3.5 Validation Model
--  (To_String is the JSF getAsString method and To_Object is the JSF getAsObject method)
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
                       Component : in out ASF.Components.UIComponent'Class;
                       Value     : in EL.Objects.Object) is abstract;

end ASF.Validators;
