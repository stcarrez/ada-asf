-----------------------------------------------------------------------
--  asf-converters-dates -- Date Converters
--  Copyright (C) 2011, 2012, 2013, 2014, 2018, 2022 Stephane Carrez
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
package body ASF.Converters.Dates is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Converters.Dates");

   function Get_Date_Format (Mode   : in Style_Type;
                             Bundle : in ASF.Locales.Bundle) return String;
   function Get_Time_Format (Mode   : in Style_Type;
                             Bundle : in ASF.Locales.Bundle) return String;

   --  ------------------------------
   --  Get the format to print the date.
   --  ------------------------------
   function Get_Date_Format (Mode   : in Style_Type;
                             Bundle : in ASF.Locales.Bundle) return String is
   begin
      case Mode is
         when DEFAULT =>
            return Bundle.Get ("faces.dates.default_date_format", "%x");

         when SHORT =>
            return Bundle.Get ("faces.dates.short_date_format", "%d/%m/%Y");

         when MEDIUM =>
            return Bundle.Get ("faces.dates.medium_date_format", "%b %d, %Y");

         when LONG =>
            return Bundle.Get ("faces.dates.long_date_format", "%B %d, %Y");

         when FULL =>
            return Bundle.Get ("faces.dates.full_date_format", "%A, %B %d, %Y");

      end case;
   end Get_Date_Format;

   --  ------------------------------
   --  Get the format to print the time.
   --  ------------------------------
   function Get_Time_Format (Mode   : in Style_Type;
                             Bundle : in ASF.Locales.Bundle) return String is
   begin
      case Mode is
         when DEFAULT =>
            return Bundle.Get ("faces.dates.default_time_format", "%X");

         when SHORT =>
            return Bundle.Get ("faces.dates.short_time_format", "%H:%M");

         when MEDIUM =>
            return Bundle.Get ("faces.dates.medium_time_format", "%H:%M");

         when LONG =>
            return Bundle.Get ("faces.dates.long_time_format", "%H:%M:%S");

         when FULL =>
            return Bundle.Get ("faces.dates.full_time_format", "%H:%M:%S %Z");

      end case;
   end Get_Time_Format;

   --  ------------------------------
   --  Get the date format pattern that must be used for formatting a date on the given component.
   --  ------------------------------
   function Get_Pattern (Convert   : in Date_Converter;
                         Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                         Bundle    : in ASF.Locales.Bundle;
                         Component : in ASF.Components.Base.UIComponent'Class) return String is
   begin
      case Convert.Format is
         when BOTH =>
            return Get_Date_Format (Convert.Time_Style, Bundle);

         when TIME =>
            return Get_Time_Format (Convert.Time_Style, Bundle);

         when DATE =>
            return Get_Date_Format (Convert.Date_Style, Bundle);

         when CONVERTER_PATTERN =>
            return Ada.Strings.Unbounded.To_String (Convert.Pattern);

         when COMPONENT_FORMAT =>
            return Component.Get_Attribute (Context => Context,
                                            Name    => "format",
                                            Default => "%x");

      end case;
   end Get_Pattern;

   --  ------------------------------
   --  Get the locale that must be used to format the date.
   --  ------------------------------
   function Get_Locale (Convert   : in Date_Converter;
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
   function To_String (Convert   : in Date_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in Util.Beans.Objects.Object) return String is
      Locale  : constant Util.Locales.Locale
        := Date_Converter'Class (Convert).Get_Locale (Context);
      Bundle  : ASF.Locales.Bundle;
   begin
      begin
         ASF.Applications.Main.Load_Bundle (Context.Get_Application.all,
                                            Name   => "asf",
                                            Locale => Util.Locales.To_String (Locale),
                                            Bundle => Bundle);

      exception
         when E : Util.Properties.Bundles.NO_BUNDLE =>
            Log.Error ("Cannot localize dates: {0}", Ada.Exceptions.Exception_Message (E));
      end;

      --  Convert the value as a date here so that we can raise an Invalid_Conversion exception.
      declare
         Pattern : constant String
           := Date_Converter'Class (Convert).Get_Pattern (Context, Bundle, Component);
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
   overriding
   function To_Object (Convert   : in Date_Converter;
                       Context   : in ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in ASF.Components.Base.UIComponent'Class;
                       Value     : in String) return Util.Beans.Objects.Object is
      Locale  : constant Util.Locales.Locale
        := Date_Converter'Class (Convert).Get_Locale (Context);
      Bundle  : ASF.Locales.Bundle;
   begin
      begin
         ASF.Applications.Main.Load_Bundle (Context.Get_Application.all,
                                            Name   => "asf",
                                            Locale => Util.Locales.To_String (Locale),
                                            Bundle => Bundle);

      exception
         when E : Util.Properties.Bundles.NO_BUNDLE =>
            Log.Error ("Cannot localize dates: {0}", Ada.Exceptions.Exception_Message (E));
      end;

      --  Convert the string to a date here so that we can raise an Invalid_Conversion exception.
      declare
         Pattern : constant String
           := Date_Converter'Class (Convert).Get_Pattern (Context, Bundle, Component);
         Date    : Util.Dates.Date_Record;
      begin
         Log.Debug ("Date conversion '{0}' with pattern '{1}'", Value, Pattern);
         Date := Util.Dates.Formats.Parse (Pattern => Pattern, Date => Value, Bundle => Bundle);
         return Util.Beans.Objects.Time.To_Object (Date.Date);

      exception
         when E : others =>
            Log.Error ("Date '{0}' does not match pattern '{1}'", Value, Pattern);
            raise Invalid_Conversion with Ada.Exceptions.Exception_Message (E);

      end;
   end To_Object;

   --  ------------------------------
   --  Create a date converter.
   --  ------------------------------
   function Create_Date_Converter (Date    : in Style_Type;
                                   Time    : in Style_Type;
                                   Format  : in Format_Type;
                                   Locale  : in String;
                                   Pattern : in String) return Date_Converter_Access is
      use Ada.Strings.Unbounded;

      L : constant Util.Locales.Locale := Util.Locales.Get_Locale (Locale);
   begin
      if Pattern'Length > 0 then
         return new Date_Converter '(Date_Style => Date,
                                     Time_Style => Time,
                                     Locale     => L,
                                     Format     => CONVERTER_PATTERN,
                                     Pattern    => To_Unbounded_String (Pattern));
      else
         return new Date_Converter '(Date_Style => Date,
                                     Time_Style => Time,
                                     Format     => Format,
                                     Locale     => L,
                                     Pattern    => Null_Unbounded_String);
      end if;
   end Create_Date_Converter;

end ASF.Converters.Dates;
