-----------------------------------------------------------------------
--  asf-validators-texts -- ASF Texts Validators
--  Copyright (C) 2011 Stephane Carrez
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

--  The <b>ASF.Validators.Texts</b> defines various text oriented validators.
package ASF.Validators.Texts is

   --  ------------------------------
   --  Length_Validator
   --  ------------------------------
   --  The <b>Length_Validator</b> implements the length validator whereby the given
   --  value must have a minimum length and a maximum length.
   type Length_Validator is new Validator with private;
   type Length_Validator_Access is access all Length_Validator'Class;

   --  Create a length validator.
   function Create_Length_Validator (Minimum : in Natural;
                                     Maximum : in Natural) return Validator_Access;

   --  Verify that the value's length is between the validator minimum and maximum
   --  boundaries.
   --  If some error are found, the procedure should create a <b>FacesMessage</b>
   --  describing the problem and add that message to the current faces context.
   --  The procedure can examine the state and modify the component tree.
   --  It must raise the <b>Invalid_Value</b> exception if the value is not valid.
   procedure Validate (Valid     : in Length_Validator;
                       Context   : in out ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in out ASF.Components.Base.UIComponent'Class;
                       Value     : in EL.Objects.Object);

private

   type Length_Validator is new Validator with record
      Minimum : Natural;
      Maximum : Natural;
   end record;

end ASF.Validators.Texts;
