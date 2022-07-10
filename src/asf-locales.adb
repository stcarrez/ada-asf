-----------------------------------------------------------------------
--  asf-locales -- Locale support
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2017, 2021, 2022 Stephane Carrez
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

with Util.Strings.Vectors;
with ASF.Contexts.Faces;
with Ada.Strings.Unbounded;
package body ASF.Locales is

   use Util.Properties.Bundles;

   type Locale_Binding (Len : Natural) is new ASF.Beans.Class_Binding with record
      Loader  : Loader_Access;
      Scope   : ASF.Beans.Scope_Type;
      Name    : String (1 .. Len);
   end record;
   type Locale_Binding_Access is access all Locale_Binding;

   overriding
   procedure Create (Factory : in Locale_Binding;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Result  : out Util.Beans.Basic.Readonly_Bean_Access);

   --  ------------------------------
   --  Initialize the locale support by using the configuration properties.
   --  Properties matching the pattern: <b>bundle</b>.<i>var-name</i>=<i>bundle-name</i>
   --  are used to register bindings linking a facelet variable <i>var-name</i>
   --  to the resource bundle <i>bundle-name</i>.
   --  ------------------------------
   procedure Initialize (Fac    : in out Factory;
                         Beans  : in out ASF.Beans.Bean_Factory;
                         Config : in Util.Properties.Manager'Class) is
      Names : Util.Strings.Vectors.Vector;
      Dir   : constant String := Config.Get ("bundle.dir", "bundles");
   begin
      Config.Get_Names (Names, "bundle.var.");
      Util.Properties.Bundles.Initialize (Fac.Factory, Dir);
      for Name of Names loop --  I in Names'Range loop
         declare
            Value  : constant String := Config.Get (Name);
         begin
            Register (Fac, Beans, Name (Name'First + 11 .. Name'Last), Value);
         end;
      end loop;
   end Initialize;

   --  ------------------------------
   --  Compute the locale that must be used according to the <b>Accept-Language</b> request
   --  header and the application supported locales.
   --  ------------------------------
   function Calculate_Locale (Fac : in Factory;
                              Req : in ASF.Requests.Request'Class)
                              return Util.Locales.Locale is
      use Util.Locales;
      use type ASF.Requests.Quality_Type;

      procedure Process_Locales (Locale  : in Util.Locales.Locale;
                                 Quality : in ASF.Requests.Quality_Type);

      Found_Locale  : Util.Locales.Locale := Fac.Default_Locale;
      Found_Quality : ASF.Requests.Quality_Type := 0.0;

      procedure Process_Locales (Locale  : in Util.Locales.Locale;
                                 Quality : in ASF.Requests.Quality_Type) is
      begin
         if Found_Quality >= Quality then
            return;
         end if;
         for I in 1 .. Fac.Nb_Locales loop
            --  We need a match on the language.  The variant/country can be ignored and will
            --  be honored by the resource bundle.
            if Fac.Locales (I) = Locale
              or else Get_Language (Fac.Locales (I)) = Get_Language (Locale)
            then
               Found_Locale := Locale;
               Found_Quality := Quality;
               return;
            end if;
         end loop;
      end Process_Locales;

   begin
      if Fac.Nb_Locales > 0 then
         Req.Accept_Locales (Process_Locales'Access);
      end if;
      return Found_Locale;
   end Calculate_Locale;

   procedure Register (Fac    : in out Factory;
                       Beans  : in out ASF.Beans.Bean_Factory;
                       Name   : in String;
                       Bundle : in String) is
      L : constant Locale_Binding_Access := new Locale_Binding (Len => Bundle'Length);
      P : ASF.Beans.Parameter_Bean_Ref.Ref;
   begin
      L.Loader := Fac.Factory'Unchecked_Access;
      L.Scope  := ASF.Beans.REQUEST_SCOPE;
      L.Name   := Bundle;
      ASF.Beans.Register (Beans, Name, L.all'Access, P);
   end Register;

   --  Load the resource bundle identified by the <b>Name</b> and for the given
   --  <b>Locale</b>.
   procedure Load_Bundle (Fac    : in out Factory;
                          Name   : in String;
                          Locale : in String;
                          Result : out Bundle) is
   begin
      Load_Bundle (Factory => Fac.Factory,
                   Locale  => Locale,
                   Name    => Name,
                   Bundle  => Result);
   end Load_Bundle;

   --  ------------------------------
   --  Get the list of supported locales for this application.
   --  ------------------------------
   function Get_Supported_Locales (From : in Factory)
                                   return Util.Locales.Locale_Array is
   begin
      return From.Locales (1 .. From.Nb_Locales);
   end Get_Supported_Locales;

   --  ------------------------------
   --  Add the locale to the list of supported locales.
   --  ------------------------------
   procedure Add_Supported_Locale (Into   : in out Factory;
                                   Locale : in Util.Locales.Locale) is
   begin
      Into.Nb_Locales := Into.Nb_Locales + 1;
      Into.Locales (Into.Nb_Locales) := Locale;
   end Add_Supported_Locale;

   --  ------------------------------
   --  Get the default locale defined by the application.
   --  ------------------------------
   function Get_Default_Locale (From : in Factory) return Util.Locales.Locale is
   begin
      return From.Default_Locale;
   end Get_Default_Locale;

   --  ------------------------------
   --  Set the default locale defined by the application.
   --  ------------------------------
   procedure Set_Default_Locale (Into   : in out Factory;
                                 Locale : in Util.Locales.Locale) is
   begin
      Into.Default_Locale := Locale;
   end Set_Default_Locale;

   overriding
   procedure Create (Factory : in Locale_Binding;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Result  : out Util.Beans.Basic.Readonly_Bean_Access) is
      pragma Unreferenced (Name);
      use type ASF.Contexts.Faces.Faces_Context_Access;

      Context : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      B       : constant Bundle_Access := new Bundle;
   begin
      if Context = null then
         Load_Bundle (Factory => Factory.Loader.all,
                      Locale  => "en",
                      Name    => Factory.Name,
                      Bundle  => B.all);
      else
         Load_Bundle (Factory => Factory.Loader.all,
                      Locale  => Util.Locales.To_String (Context.Get_Locale),
                      Name    => Factory.Name,
                      Bundle  => B.all);
      end if;
      Result := B.all'Access;
   end Create;

end ASF.Locales;
