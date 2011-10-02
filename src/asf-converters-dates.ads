-----------------------------------------------------------------------
--  asf-converters-dates -- Date Converters
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
with Util.Beans.Objects;
with ASF.Components.Base;
with ASF.Contexts.Faces;

--  The <b>ASF.Converters.Dates</b> defines the date converter to format a date object
--  into a localized representation.
--
--  See JSR 314 - JavaServer Faces Specification 9.4.3 <f:convertDateTime>
--  (To_String is the JSF getAsString method and To_Object is the JSF getAsObject method)
package ASF.Converters.Dates is

   --  ------------------------------
   --  Converter
   --  ------------------------------
   --  The <b>Date_Converter</b> translates the object value which holds an Ada.Calendar
   --  into a printable date representation.  It translates a string into an Ada Calendar time.
   --  Unlike the Java implementation, the instance will be shared by multiple
   --  views and requests.
   type Date_Converter is new Converter with null record;
   type Date_Converter_Access is access all Date_Converter'Class;

   --  Convert the object value into a string.  The object value is associated
   --  with the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   function To_String (Convert   : in Date_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in Util.Beans.Objects.Object) return String;

   --  Convert the date string into an object for the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   function To_Object (Convert   : in Date_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in String) return Util.Beans.Objects.Object;

   --  Get the date format pattern that must be used for formatting a date on the given component.
   function Get_Pattern (Convert   : in Date_Converter;
                         Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                         Component : in ASF.Components.Base.UIComponent'Class) return String;

end ASF.Converters.Dates;
