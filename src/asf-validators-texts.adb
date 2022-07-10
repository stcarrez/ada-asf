-----------------------------------------------------------------------
--  asf-validators-texts -- ASF Texts Validators
--  Copyright (C) 2011, 2020, 2022 Stephane Carrez
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

--  The <b>ASF.Validators.Texts</b> defines various text oriented validators.
package body ASF.Validators.Texts is

   --  ------------------------------
   --  Length_Validator
   --  ------------------------------

   --  ------------------------------
   --  Create a length validator.
   --  ------------------------------
   function Create_Length_Validator (Minimum : in Natural;
                                     Maximum : in Natural) return Validator_Access is
      Result : constant Length_Validator_Access := new Length_Validator;
   begin
      Result.Minimum := Minimum;
      Result.Maximum := Maximum;
      return Result.all'Access;
   end Create_Length_Validator;

   --  ------------------------------
   --  Verify that the value's length is between the validator minimum and maximum
   --  boundaries.
   --  If some error are found, the procedure should create a <b>FacesMessage</b>
   --  describing the problem and add that message to the current faces context.
   --  The procedure can examine the state and modify the component tree.
   --  It must raise the <b>Invalid_Value</b> exception if the value is not valid.
   --  ------------------------------
   overriding
   procedure Validate (Valid     : in Length_Validator;
                       Context   : in out ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in out ASF.Components.Base.UIComponent'Class;
                       Value     : in EL.Objects.Object) is
   begin
      if EL.Objects.Is_Null (Value) then
         return;
      end if;

      declare
         S : constant String := EL.Objects.To_String (Value);
      begin
         if S'Length > Valid.Maximum then
            Component.Add_Message (Name    => ASF.Components.VALIDATOR_MESSAGE_NAME,
                                   Default => MAXIMUM_MESSAGE_ID,
                                   Arg1    => Util.Beans.Objects.To_Object (Valid.Maximum),
                                   Arg2    => Component.Get_Label (Context),
                                   Context => Context);
            raise Invalid_Value;
         end if;
         if S'Length < Valid.Minimum then
            Component.Add_Message (Name    => ASF.Components.VALIDATOR_MESSAGE_NAME,
                                   Default => MINIMUM_MESSAGE_ID,
                                   Arg1    => Util.Beans.Objects.To_Object (Valid.Minimum),
                                   Arg2    => Component.Get_Label (Context),
                                   Context => Context);
            raise Invalid_Value;
         end if;
      end;
   end Validate;

   --  ------------------------------
   --  Regex_Validator
   --  ------------------------------

   --  ------------------------------
   --  Create a regex validator.
   --  ------------------------------
   function Create_Regex_Validator (Pattern : in GNAT.Regpat.Pattern_Matcher)
                                   return Validator_Access is
      Result : constant Regex_Validator_Access := new Regex_Validator (Pattern.Size);
   begin
      Result.Pattern := Pattern;
      return Result.all'Access;
   end Create_Regex_Validator;

   --  ------------------------------
   --  Verify that the value's length is between the validator minimum and maximum
   --  boundaries.
   --  If some error are found, the procedure should create a <b>FacesMessage</b>
   --  describing the problem and add that message to the current faces context.
   --  The procedure can examine the state and modify the component tree.
   --  It must raise the <b>Invalid_Value</b> exception if the value is not valid.
   --  ------------------------------
   overriding
   procedure Validate (Valid     : in Regex_Validator;
                       Context   : in out ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in out ASF.Components.Base.UIComponent'Class;
                       Value     : in EL.Objects.Object) is
   begin
      if EL.Objects.Is_Null (Value) then
         return;
      end if;

      declare
         S : constant String := EL.Objects.To_String (Value);
      begin
         if not GNAT.Regpat.Match (Valid.Pattern, S) then
            Component.Add_Message (Name    => ASF.Components.VALIDATOR_MESSAGE_NAME,
                                   Default => REGEX_MESSAGE_ID,
                                   Arg1    => Component.Get_Label (Context),
                                   Context => Context);
            raise Invalid_Value;
         end if;
      end;
   end Validate;

end ASF.Validators.Texts;
