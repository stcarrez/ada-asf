-----------------------------------------------------------------------
--  asf-factory -- Component and tag factory
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with EL.Beans;
with Ada.Strings.Unbounded;
package body ASF.Locales is

   use Util.Properties.Bundles;

   type Bundle is new Util.Properties.Bundles.Manager
     and EL.Beans.Readonly_Bean with null record;
   type Bundle_Access is access all Bundle;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Value (From : Bundle;
                       Name : String) return EL.Objects.Object;

   type Locale_Binding is new ASF.Beans.Binding with record
      Loader  : Loader_Access;
      Free    : ASF.Beans.Free_Bean_Access;
      Scope   : ASF.Beans.Scope_Type;
      Name    : Util.Strings.Name_Access;
   end record;
   type Locale_Binding_Access is access all Locale_Binding;

   procedure Create (Factory : in Locale_Binding;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Result  : out EL.Beans.Readonly_Bean_Access;
                     Free    : out ASF.Beans.Free_Bean_Access;
                     Scope   : out ASF.Beans.Scope_Type);

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
      L.Free   := null;
      L.Scope  := ASF.Beans.REQUEST_SCOPE;
      L.Name   := new String '(Bundle);
      ASF.Beans.Register (Beans, Name, L.all'Access);
   end Register;

   procedure Create (Factory : in Locale_Binding;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Result  : out EL.Beans.Readonly_Bean_Access;
                     Free    : out ASF.Beans.Free_Bean_Access;
                     Scope   : out ASF.Beans.Scope_Type) is
      pragma Unreferenced (Name);

      B      : constant Bundle_Access := new Bundle;
      Locale : constant String := "en";
   begin
      Load_Bundle (Factory => Factory.Loader.all,
                   Locale  => Locale,
                   Name    => Factory.Name.all,
                   Bundle  => B.all);
      Result := B.all'Access;
      Scope  := ASF.Beans.REQUEST_SCOPE;
      Free   := null;
   end Create;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Value (From : Bundle;
                       Name : String) return EL.Objects.Object is
      Value : constant String := From.Get (Name, Name);
   begin
      return EL.Objects.To_Object (Value);
   end Get_Value;

end ASF.Locales;
