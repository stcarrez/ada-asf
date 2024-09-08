-----------------------------------------------------------------------
--  volume - A simple bean example
--  Copyright (C) 2010, 2011, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Beans.Methods;

with ASF.Components.Base;
with ASF.Contexts.Faces;
with ASF.Converters;
package Volume is

   type My_Float is delta 0.01 digits 10;

   type Compute_Bean is new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with record
      Height : My_Float := -1.0;
      Radius : My_Float := -1.0;
      Volume : My_Float := -1.0;
   end record;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : Compute_Bean;
                       Name : String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Compute_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Compute the volume of the cylinder.
   procedure Run (From    : in out Compute_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Compute_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   type Float_Converter is new ASF.Converters.Converter with null record;

   overriding
   function To_String (Convert   : in Float_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in Util.Beans.Objects.Object) return String;

   overriding
   function To_Object (Convert   : in Float_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in String) return Util.Beans.Objects.Object;

end Volume;
