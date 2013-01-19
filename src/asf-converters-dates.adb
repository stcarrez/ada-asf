-----------------------------------------------------------------------
--  asf-converters-dates -- Date Converters
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
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

with Ada.Calendar;
with Ada.Exceptions;

with Util.Properties.Bundles;
with Util.Beans.Objects.Time;
with Util.Dates.Formats;
with Util.Log.Loggers;

with ASF.Applications.Main;
with ASF.Locales;
package body ASF.Converters.Dates is

   use Util.Log;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Converters.Dates");

   --  ------------------------------
   --  Get the date format pattern that must be used for formatting a date on the given component.
   --  ------------------------------
   function Get_Pattern (Convert   : in Date_Converter;
                         Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                         Component : in ASF.Components.Base.UIComponent'Class) return String is
      pragma Unreferenced (Convert);

      Pattern : constant String := Component.Get_Attribute (Context => Context,
                                                            Name    => "format",
                                                            Default => "%x");
   begin
      return Pattern;
   end Get_Pattern;

   --  ------------------------------
   --  Convert the object value into a string.  The object value is associated
   --  with the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   --  ------------------------------
   function To_String (Convert   : in Date_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in Util.Beans.Objects.Object) return String is
      Bundle  : ASF.Locales.Bundle;
      Pattern : constant String := Date_Converter'Class (Convert).Get_Pattern (Context, Component);
   begin
      begin
         ASF.Applications.Main.Load_Bundle (Context.Get_Application.all,
                                            Name   => "dates",
                                            Locale => "en",
                                            Bundle => Bundle);

      exception
         when E : Util.Properties.Bundles.NO_BUNDLE =>
            Log.Error ("Cannot localize dates: {0}", Ada.Exceptions.Exception_Message (E));
      end;

      --  Convert the value as a date here so that we can raise an Invalid_Conversion exception.
      declare
         Date    : constant Ada.Calendar.Time := Util.Beans.Objects.Time.To_Time (Value);
         Result  : constant String := Util.Dates.Formats.Format (Pattern, Date, Bundle);
      begin
         return Result;
      end;
   exception
      when E : others =>
         raise Invalid_Conversion with Ada.Exceptions.Exception_Message (E);
   end To_String;

   --  ------------------------------
   --  Convert the date string into an object for the specified component.
   --  If the string cannot be converted, the Invalid_Conversion exception should be raised.
   --  ------------------------------
   function To_Object (Convert   : in Date_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (Convert, Context, Component, Value);
   begin
      Log.Error ("String to date conversion is not yet implemented");
      return Util.Beans.Objects.Null_Object;
   end To_Object;

end ASF.Converters.Dates;
