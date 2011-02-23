-----------------------------------------------------------------------
--  volume - Bean to compute the cylinder volume
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

with Ada.Strings.Fixed;
with Ada.Text_Io.Editing;

with ASF.Events.Actions;
package body Volume is

   use Ada.Strings.Unbounded;

   Pic : constant Ada.Text_IO.Editing.Picture := Ada.Text_IO.Editing.To_Picture ("ZZZZZ9.99");

   package Float_Output is new Ada.Text_IO.Editing.Decimal_Output(My_Float);

   package Run_Binding is
     new ASF.Events.Actions.Action_Method.Bind (Bean => Compute_Bean,
                                                Method      => Run,
                                                Name        => "run");

   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Run_Binding.Proxy'Unchecked_Access, Run_Binding.Proxy'Unchecked_Access);

   overriding
   function Get_Method_Bindings (From : in Compute_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
   begin
      return Binding_Array'Unchecked_Access;
   end Get_Method_Bindings;

   procedure Run (From    : in out Compute_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Outcome := To_Unbounded_String ("compute");
   end Run;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   function Get_Value (From : Compute_Bean;
                       Name : String) return EL.Objects.Object is
      V : My_Float;
   begin
      if Name = "radius" and From.Radius >= 0.0 then
         return EL.Objects.To_Object (Float (From.Radius));

      elsif Name = "height" and From.Height >= 0.0 then
         return EL.Objects.To_Object (Float (From.Height));

      elsif Name = "volume" and From.Radius >= 0.0 and From.Height >= 0.0 then
         V := (From.Radius * From.Radius);
         V := V * From.Height;
         V := V * 3.141;
         return EL.Objects.To_Object (Float_Output.Image (V, Pic));
      else
         return EL.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   procedure Set_Value (From  : in out Compute_Bean;
                        Name  : in String;
                        Value : in EL.Objects.Object) is
   begin
      if Name = "radius" then
         From.Radius := My_Float (EL.Objects.To_Float (Value));
      elsif Name = "height" then
         From.Height := My_Float (EL.Objects.To_Float (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Convert the object value into a string.  The object value is associated
   --  with the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   --  ------------------------------
   function To_String (Convert   : in Float_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in EL.Objects.Object) return String is
      pragma Unreferenced (Convert, Context, Component);

      F : constant My_Float := My_Float (EL.Objects.To_Float (Value));
      R : constant String   := Float_Output.Image (F, Pic);
   begin
      return Ada.Strings.Fixed.Trim (R, Ada.Strings.Both);
   end To_String;

   --  ------------------------------
   --  Convert the string into an object for the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   --  ------------------------------
   function To_Object (Convert   : in Float_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in String) return EL.Objects.Object is
      pragma Unreferenced (Convert, Context, Component);
   begin
      return EL.Objects.To_Object (Float (My_Float'Value (Value)));
   end To_Object;

end Volume;
