-----------------------------------------------------------------------
--  volume - Bean to compute the cylinder volume
--  Copyright (C) 2010, 2011, 2012, 2022 Stephane Carrez
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
with Ada.Text_IO.Editing;
with Ada.Numerics;

with ASF.Events.Faces.Actions;
package body Volume is

   use Ada.Strings.Unbounded;

   Pic : constant Ada.Text_IO.Editing.Picture := Ada.Text_IO.Editing.To_Picture ("ZZZZZZZZ9.99");

   package Float_Output is new Ada.Text_IO.Editing.Decimal_Output (My_Float);

   package Run_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Compute_Bean,
                                                      Method => Run,
                                                      Name   => "run");

   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Run_Binding.Proxy'Unchecked_Access, Run_Binding.Proxy'Unchecked_Access);

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Compute_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Binding_Array'Unchecked_Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Compute the volume of the cylinder.
   --  ------------------------------
   procedure Run (From    : in out Compute_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      V : My_Float;
   begin
      V := (From.Radius * From.Radius);
      V := V * From.Height;
      From.Volume := V * 3.141;
      Outcome := To_Unbounded_String ("compute");
   end Run;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : Compute_Bean;
                       Name : String) return Util.Beans.Objects.Object is
   begin
      if Name = "radius" and then From.Radius >= 0.0 then
         return Util.Beans.Objects.To_Object (Float (From.Radius));

      elsif Name = "height" and then From.Height >= 0.0 then
         return Util.Beans.Objects.To_Object (Float (From.Height));

      elsif Name = "volume" and then From.Volume >= 0.0 then
         return Util.Beans.Objects.To_Object (Float (From.Volume));

      elsif Name = "pi" then
         return Util.Beans.Objects.To_Object (Float (Ada.Numerics.Pi));

      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Compute_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "radius" then
         From.Radius := My_Float (Util.Beans.Objects.To_Float (Value));
      elsif Name = "height" then
         From.Height := My_Float (Util.Beans.Objects.To_Float (Value));
      end if;
   end Set_Value;

   --  ------------------------------
   --  Convert the object value into a string.  The object value is associated
   --  with the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   --  ------------------------------
   overriding
   function To_String (Convert   : in Float_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in Util.Beans.Objects.Object) return String is
      pragma Unreferenced (Convert, Context, Component);

      F : constant My_Float := My_Float (Util.Beans.Objects.To_Float (Value));
      R : constant String   := Float_Output.Image (F, Pic);
   begin
      return Ada.Strings.Fixed.Trim (R, Ada.Strings.Both);
   end To_String;

   --  ------------------------------
   --  Convert the string into an object for the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   --  ------------------------------
   overriding
   function To_Object (Convert   : in Float_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (Convert, Context, Component);
   begin
      return Util.Beans.Objects.To_Object (Float (My_Float'Value (Value)));
   end To_Object;

end Volume;
