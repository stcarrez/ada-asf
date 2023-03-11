-----------------------------------------------------------------------
--  asf-converters-numbers -- Floating point number converters
--  Copyright (C) 2010, 2019, 2023 Stephane Carrez
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
with Ada.Exceptions;
with Ada.Strings.Maps;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Util.Properties.Bundles;
with Util.Beans.Objects;
with Util.Log.Loggers;

with ASF.Applications.Main;
package body ASF.Converters.Numbers is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Converters.Numbers");

   Local_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set ("$#V_");

   Euro : constant Wide_Wide_String := "€";

   type Float_Number is delta 0.0000000001 digits 18;

   package Formatter is
     new Ada.Text_IO.Editing.Decimal_Output (Num => Float_Number);

   --  ------------------------------
   --  Get the locale that must be used to format the number.
   --  ------------------------------
   function Get_Locale (Convert   : in Number_Converter;
                        Context   : in ASF.Contexts.Faces.Faces_Context'Class)
                        return Util.Locales.Locale is
      use type Util.Locales.Locale;
   begin
      if Convert.Locale /= Util.Locales.NULL_LOCALE then
         return Convert.Locale;
      else
         return Context.Get_Locale;
      end if;
   end Get_Locale;

   --  ------------------------------
   --  Convert the object value into a string.  The object value is associated
   --  with the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   --  ------------------------------
   overriding
   function To_String (Convert   : in Number_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in EL.Objects.Object) return String is
      pragma Unreferenced (Component);

      Bundle  : ASF.Locales.Bundle;
   begin
      if Convert.Depend_On_Local then
         declare
            Locale : constant Util.Locales.Locale
              := Number_Converter'Class (Convert).Get_Locale (Context);
         begin
            ASF.Applications.Main.Load_Bundle (Context.Get_Application.all,
                                               Name   => "asf",
                                               Locale => Util.Locales.To_String (Locale),
                                               Bundle => Bundle);

         exception
            when E : Util.Properties.Bundles.NO_BUNDLE =>
               Log.Error ("Cannot localize numbers: {0}", Ada.Exceptions.Exception_Message (E));
         end;
      end if;

      --  Convert the value as a date here so that we can raise an Invalid_Conversion exception.
      declare
         Currency   : constant String
           := Number_Converter'Class (Convert).Get_Currency (Bundle);
         Separator  : constant Character
           := Number_Converter'Class (Convert).Get_Separator (Bundle);
         Radix_Mark : constant Character
           := Number_Converter'Class (Convert).Get_Radix_Mark (Bundle);
         Fill : constant Character
           := Number_Converter'Class (Convert).Get_Fill (Bundle);
         Val     : constant Float_Number
           := Float_Number (Util.Beans.Objects.To_Long_Long_Float (Value));
         Result  : constant String
           := Formatter.Image (Val, Convert.Picture, Currency, Fill, Separator, Radix_Mark);
      begin
         return Result;
      end;

   exception
      when E : others =>
         raise Invalid_Conversion with Ada.Exceptions.Exception_Message (E);

   end To_String;

   --  ------------------------------
   --  Convert the string into an object for the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   --  ------------------------------
   overriding
   function To_Object (Convert   : in Number_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in String) return EL.Objects.Object is
      pragma Unreferenced (Convert, Context, Value);
   begin
      Component.Log_Error ("Conversion of string to a floating point is not implemented");
      raise ASF.Converters.Invalid_Conversion with "Not implemented";
      return Util.Beans.Objects.Null_Object;
   end To_Object;

   --  ------------------------------
   --  Get the currency to be used from the resource bundle in the user's current locale.
   --  ------------------------------
   function Get_Currency (Converter : in Number_Converter;
                          Bundle    : in ASF.Locales.Bundle) return String is
      pragma Unreferenced (Converter);
   begin
      return Bundle.Get ("faces.numbers.currency",
                         Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Euro));
   end Get_Currency;

   --  ------------------------------
   --  Get the separator to be used from the resource bundle in the user's current locale.
   --  ------------------------------
   function Get_Separator (Converter : in Number_Converter;
                           Bundle    : in ASF.Locales.Bundle) return Character is
      pragma Unreferenced (Converter);

      S : constant String := Bundle.Get ("faces.numbers.separator", ",");
   begin
      return (if S'Length = 0 then ',' else S (S'First));
   end Get_Separator;

   --  ------------------------------
   --  Get the radix mark to be used from the resource bundle in the user's current locale.
   --  ------------------------------
   function Get_Radix_Mark (Converter : in Number_Converter;
                            Bundle    : in ASF.Locales.Bundle) return Character is
      pragma Unreferenced (Converter);

      S : constant String := Bundle.Get ("faces.numbers.radix_mark", ".");
   begin
      return (if S'Length = 0 then '.' else S (S'First));
   end Get_Radix_Mark;

   --  ------------------------------
   --  Get the fill character to be used from the resource bundle in the user's current locale.
   --  ------------------------------
   function Get_Fill (Converter : in Number_Converter;
                      Bundle    : in ASF.Locales.Bundle) return Character is
      pragma Unreferenced (Converter);

      S : constant String := Bundle.Get ("faces.numbers.fill", " ");
   begin
      return (if S'Length = 0 then ' ' else S (S'First));
   end Get_Fill;

   --  ------------------------------
   --  Set the picture that must be used for the conversion.
   --  ------------------------------
   procedure Set_Picture (Converter : in out Number_Converter;
                          Picture   : in String) is
   begin
      Converter.Picture := Ada.Text_IO.Editing.To_Picture (Picture);
      Converter.Depend_On_Local
        := (for some C of Picture => Ada.Strings.Maps.Is_In (C, Local_Set));
   end Set_Picture;

end ASF.Converters.Numbers;
