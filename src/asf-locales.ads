-----------------------------------------------------------------------
--  asf-locales -- Locale support
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2021 Stephane Carrez
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

with Util.Beans.Basic;
with Util.Strings.Maps;
with Util.Properties.Bundles;
with Util.Locales;
with ASF.Beans;
with ASF.Requests;

--  The <b>ASF.Locales</b> package manages everything related to the locales.
--  It allows to register bundles that contains localized messages and be able
--  to use them in the facelet views.
package ASF.Locales is

   --  To keep the implementation simple, the maximum list of supported locales by the
   --  application is limited to 32.  Most applications support 1 or 2 languages.
   MAX_SUPPORTED_LOCALES : constant Positive := 32;

   type Bundle is new Util.Properties.Bundles.Manager
     and Util.Beans.Basic.Readonly_Bean with null record;
   type Bundle_Access is access all Bundle;

   type Factory is limited private;

   --  Initialize the locale support by using the configuration properties.
   --  Properties matching the pattern: <b>bundles</b>.<i>var-name</i>=<i>bundle-name</i>
   --  are used to register bindings linking a facelet variable <i>var-name</i>
   --  to the resource bundle <i>bundle-name</i>.
   procedure Initialize (Fac    : in out Factory;
                         Beans  : in out ASF.Beans.Bean_Factory;
                         Config : in Util.Properties.Manager'Class);

   --  Register a bundle and bind it to a facelet variable.
   procedure Register (Fac    : in out Factory;
                       Beans  : in out ASF.Beans.Bean_Factory;
                       Name   : in String;
                       Bundle : in String);

   --  Load the resource bundle identified by the <b>Name</b> and for the given
   --  <b>Locale</b>.
   procedure Load_Bundle (Fac    : in out Factory;
                          Name   : in String;
                          Locale : in String;
                          Result : out Bundle);

   --  Compute the locale that must be used according to the <b>Accept-Language</b> request
   --  header and the application supported locales.
   function Calculate_Locale (Fac : in Factory;
                              Req : in ASF.Requests.Request'Class)
                              return Util.Locales.Locale;

   --  Get the list of supported locales for this application.
   function Get_Supported_Locales (From : in Factory)
                                   return Util.Locales.Locale_Array;

   --  Add the locale to the list of supported locales.
   procedure Add_Supported_Locale (Into   : in out Factory;
                                   Locale : in Util.Locales.Locale);

   --  Get the default locale defined by the application.
   function Get_Default_Locale (From : in Factory) return Util.Locales.Locale;

   --  Set the default locale defined by the application.
   procedure Set_Default_Locale (Into   : in out Factory;
                                 Locale : in Util.Locales.Locale);

private

   type Factory is limited record
      Factory : aliased Util.Properties.Bundles.Loader;
      Bundles : Util.Strings.Maps.Map;

      --  The default locale used by the application.
      Default_Locale  : Util.Locales.Locale := Util.Locales.ENGLISH;

      --  Number of supported locales.
      Nb_Locales      : Natural := 0;

      --  The list of supported locales.
      Locales         : Util.Locales.Locale_Array (1 .. MAX_SUPPORTED_LOCALES);
   end record;

   type Factory_Access is access all Factory;

end ASF.Locales;
