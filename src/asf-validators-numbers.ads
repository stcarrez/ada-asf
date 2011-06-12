-----------------------------------------------------------------------
--  asf-validators-numbers -- ASF Number Validators
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

--  The <b>ASF.Validators.Numbers</b> defines various number oriented validators.
package ASF.Validators.Numbers is

   --  ------------------------------
   --  Range Validator
   --  ------------------------------
   --  The <b>Range_Validator</b> implements a long range validator whereby the given
   --  value must be between a minimum and a maximum.
   type Range_Validator is new Validator with private;
   type Range_Validator_Access is access all Range_Validator'Class;

   --  Create a range validator to check that the value is between the minimum and the maximum.
   function Create_Range_Validator (Minimum : in Long_Long_Integer;
                                    Maximum : in Long_Long_Integer) return Validator_Access;

   --  Verify that the value is between the validator minimum and maximum
   --  boundaries.
   --  If some error are found, the procedure should create a <b>FacesMessage</b>
   --  describing the problem and add that message to the current faces context.
   --  The procedure can examine the state and modify the component tree.
   --  It must raise the <b>Invalid_Value</b> exception if the value is not valid.
   procedure Validate (Valid     : in Range_Validator;
                       Context   : in out ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in out ASF.Components.Base.UIComponent'Class;
                       Value     : in EL.Objects.Object);

private

   type Range_Validator is new Validator with record
      Minimum : Long_Long_Integer;
      Maximum : Long_Long_Integer;
   end record;

end ASF.Validators.Numbers;
