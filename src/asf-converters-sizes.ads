-----------------------------------------------------------------------
--  asf-converters-sizes -- Size converter
--  Copyright (C) 2012, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Beans.Objects;
with ASF.Components.Base;
with ASF.Contexts.Faces;

--  == Size converter ==
--  The `ASF.Converters.Sizes` defines a converter to display a file size in bytes,
--  kilo bytes, mega bytes or giga bytes.
package ASF.Converters.Sizes is

   --  ------------------------------
   --  Converter
   --  ------------------------------
   --  The <b>Size_Converter</b> translates the object value which holds an integer value
   --  into a printable size representation.
   type Size_Converter is new Converter with null record;
   type Size_Converter_Access is access all Size_Converter'Class;

   --  Convert the object value into a string.  The object value is associated
   --  with the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   overriding
   function To_String (Convert   : in Size_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in Util.Beans.Objects.Object) return String;

   --  Convert the date string into an object for the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   overriding
   function To_Object (Convert   : in Size_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in String) return Util.Beans.Objects.Object;

end ASF.Converters.Sizes;
