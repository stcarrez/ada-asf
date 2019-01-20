-----------------------------------------------------------------------
--  asf-converters-numbers -- Floating point number converters
--  Copyright (C) 2010, 2019 Stephane Carrez
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
with ASF.Locales;
private with Util.Locales;
private with Ada.Text_IO.Editing;

--  The `ASF.Converters.Numbers` provides a floating point number converter.
--  It can be used to print floating point numbers in various formats.
package ASF.Converters.Numbers is

   type Number_Converter is new Converter with private;
   type Number_Converter_Access is access all Number_Converter'Class;

   --  Convert the object value into a string.  The object value is associated
   --  with the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   overriding
   function To_String (Convert   : in Number_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in EL.Objects.Object) return String;

   --  Convert the string into an object for the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   overriding
   function To_Object (Convert   : in Number_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in String) return EL.Objects.Object;

   --  Set the picture that must be used for the conversion.
   procedure Set_Picture (Converter : in out Number_Converter;
                          Picture   : in String);

   --  Get the currency to be used from the resource bundle in the user's current locale.
   function Get_Currency (Converter : in Number_Converter;
                          Bundle    : in ASF.Locales.Bundle) return String;

   --  Get the separator to be used from the resource bundle in the user's current locale.
   function Get_Separator (Converter : in Number_Converter;
                           Bundle    : in ASF.Locales.Bundle) return Character;

   --  Get the radix mark to be used from the resource bundle in the user's current locale.
   function Get_Radix_Mark (Converter : in Number_Converter;
                            Bundle    : in ASF.Locales.Bundle) return Character;

   --  Get the fill character to be used from the resource bundle in the user's current locale.
   function Get_Fill (Converter : in Number_Converter;
                      Bundle    : in ASF.Locales.Bundle) return Character;

private

   type Number_Converter is new Converter with record
      Picture         : Ada.Text_IO.Editing.Picture;
      Locale          : Util.Locales.Locale;
      Depend_On_Local : Boolean := False;
   end record;

   --  Get the locale that must be used to format the number.
   function Get_Locale (Convert   : in Number_Converter;
                        Context   : in ASF.Contexts.Faces.Faces_Context'Class)
                        return Util.Locales.Locale;

end ASF.Converters.Numbers;
