-----------------------------------------------------------------------
--  asf-converters -- ASF Converters
--  Copyright (C) 2010, 2023 Stephane Carrez
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

--  = Converters =
--  The `ASF.Converters` package defines an interface used by the conversion model
--  to translate an object into a string when formatting the response and translate
--  a string into an object during the apply request or validation phases (JSF postback).
--
--  The `Converter` interface defines two functions for the convertion of a object
--  to a string and (`To_String`) and convert back a string to an object (`To_Object`).
--  See JSR 314 - JavaServer Faces Specification 3.3.2 Converter
--  (To_String is the JSF getAsString method and To_Object is the JSF getAsObject method)
--
--  @include asf-converters-dates.ads
--  @include asf-converters-numbers.ads
--  @include asf-converters-sizes.ads
package ASF.Converters is

   Invalid_Conversion : exception;

   --  ------------------------------
   --  Converter
   --  ------------------------------
   --  The `Converter` must implement two functions to convert a string into
   --  an object and the opposite.  The converter instance must be registered in
   --  the component factory (See `ASF.Factory.Component_Factory`).
   --  Unlike the Java implementation, the instance will be shared by multiple
   --  views and requests.
   type Converter is limited interface;
   type Converter_Access is access all Converter'Class;

   --  Convert the object value into a string.  The object value is associated
   --  with the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   function To_String (Convert   : in Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in EL.Objects.Object) return String is abstract;

   --  Convert the string into an object for the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   function To_Object (Convert   : in Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in String) return EL.Objects.Object is abstract;

end ASF.Converters;
