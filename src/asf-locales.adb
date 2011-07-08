-----------------------------------------------------------------------
--  asf-factory -- Component and tag factory
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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


with Ada.Strings.Unbounded;
package body ASF.Locales is

   use Util.Properties.Bundles;

   type Locale_Binding is new ASF.Beans.Class_Binding with record
      Loader  : Loader_Access;
      Scope   : ASF.Beans.Scope_Type;
      Name    : Util.Strings.Name_Access;
   end record;
   type Locale_Binding_Access is access all Locale_Binding;

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
      Names : constant Util.Properties.Name_Array := Config.Get_Names ("bundle.var.");
      Dir   : constant String := Config.Get ("bundle.dir", "bundles");
   begin
      Util.Properties.Bundles.Initialize (Fac.Factory, Dir);
      for I in Names'Range loop
         declare
            Name   : Util.Properties.Value renames Names (I);
            Value  : constant String := Config.Get (Name);
            Bundle : constant String := Ada.Strings.Unbounded.To_String (Name);
         begin
            Register (Fac, Beans, Bundle (Bundle'First + 11 .. Bundle'Last), Value);
         end;
      end loop;
   end Initialize;

   procedure Register (Fac    : in out Factory;
                       Beans  : in out ASF.Beans.Bean_Factory;
                       Name   : in String;
                       Bundle : in String) is
      L : constant Locale_Binding_Access := new Locale_Binding;
   begin
      L.Loader := Fac.Factory'Unchecked_Access;
      L.Scope  := ASF.Beans.REQUEST_SCOPE;
      L.Name   := new String '(Bundle);
      ASF.Beans.Register (Beans, Name, L.all'Access);
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

   procedure Create (Factory : in Locale_Binding;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Result  : out Util.Beans.Basic.Readonly_Bean_Access) is
      pragma Unreferenced (Name);

      B      : constant Bundle_Access := new Bundle;
      Locale : constant String := "en";
   begin
      Load_Bundle (Factory => Factory.Loader.all,
                   Locale  => Locale,
                   Name    => Factory.Name.all,
                   Bundle  => B.all);
      Result := B.all'Access;
   end Create;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   function Get_Value (From : Bundle;
                       Name : String) return Util.Beans.Objects.Object is
      Value : constant String := From.Get (Name, Name);
   begin
      return Util.Beans.Objects.To_Object (Value);
   end Get_Value;

end ASF.Locales;
