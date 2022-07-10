-----------------------------------------------------------------------
--  asf-factory -- Component and tag factory
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2022 Stephane Carrez
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
with Util.Log.Loggers;
with Ada.Strings.Hash;
package body ASF.Factory is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Factory");

   --  ------------------------------
   --  Compute a hash for the tag name.
   --  ------------------------------
   function Hash (Key : in Tag_Name) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;

      H1 : constant Ada.Containers.Hash_Type := Ada.Strings.Hash (Key.URI.all);
      H2 : constant Ada.Containers.Hash_Type := Ada.Strings.Hash (Key.Name.all);
   begin
      return H1 xor H2;
   end Hash;

   --  ------------------------------
   --  Returns true if both tag names are identical.
   --  ------------------------------
   overriding
   function "=" (Left, Right : in Tag_Name) return Boolean is
   begin
      return Left.URI.all = Right.URI.all and then Left.Name.all = Right.Name.all;
   end "=";

   --  ------------------------------
   --  Find the create function in bound to the name in the given URI namespace.
   --  Returns null if no such binding exist.
   --  ------------------------------
   function Find (Factory : in Component_Factory;
                  URI     : in String;
                  Name    : in String) return Binding_Type is
      Key : constant Tag_Name := Tag_Name '(URI  => URI'Unrestricted_Access,
                                            Name => Name'Unrestricted_Access);
      Pos : constant Factory_Maps.Cursor := Factory.Map.Find (Key);
   begin
      if Factory_Maps.Has_Element (Pos) then
         return Factory_Maps.Element (Pos);
      else
         return Null_Binding;
      end if;
   end Find;

   --  ------------------------------
   --  Register a binding library in the factory
   --  ------------------------------
   procedure Register (Factory  : in out Component_Factory;
                       Bindings : in Factory_Bindings_Access) is
   begin
      Log.Info ("Register bindings: {0}", Bindings.URI.all);

      for I in Bindings.Bindings'Range loop
         declare
            Key : constant Tag_Name := Tag_Name '(URI  => Bindings.URI,
                                                  Name => Bindings.Bindings (I).Name);
         begin
            Factory.Map.Include (Key, Bindings.Bindings (I));
         end;
      end loop;
   end Register;

   procedure Register (Factory   : in out Component_Factory;
                       URI       : in ASF.Views.Nodes.Name_Access;
                       Name      : in ASF.Views.Nodes.Name_Access;
                       Tag       : in ASF.Views.Nodes.Tag_Node_Create_Access;
                       Create    : in ASF.Views.Nodes.Create_Access) is
      Key  : constant Tag_Name := Tag_Name '(URI  => URI, Name => Name);
      Bind : constant Binding_Type := Binding_Type '(Name      => Name,
                                                     Tag       => Tag,
                                                     Component => Create);
   begin
      Factory.Map.Include (Key, Bind);
   end Register;

   --  ------------------------------
   --  Register the converter instance under the given name.
   --  ------------------------------
   procedure Register (Factory   : in out Component_Factory;
                       Name      : in String;
                       Converter : in ASF.Converters.Converter_Access) is
   begin
      Log.Info ("Register converter: {0}", Name);

      Factory.Converters.Include (EL.Objects.To_Object (Name), Converter);
   end Register;

   --  ------------------------------
   --  Find the converter instance that was registered under the given name.
   --  Returns null if no such converter exist.
   --  ------------------------------
   function Find (Factory : in Component_Factory;
                  Name    : in EL.Objects.Object) return ASF.Converters.Converter_Access is
      Pos : constant Converter_Maps.Cursor := Factory.Converters.Find (Name);
   begin
      if Converter_Maps.Has_Element (Pos) then
         return Converter_Maps.Element (Pos);
      else
         return null;
      end if;
   end Find;

   --  ------------------------------
   --  Register the validator instance under the given name.
   --  ------------------------------
   procedure Register (Factory   : in out Component_Factory;
                       Name      : in String;
                       Validator : in ASF.Validators.Validator_Access) is
   begin
      Log.Info ("Register validator: {0}", Name);

      Factory.Validators.Include (EL.Objects.To_Object (Name), Validator);
   end Register;

   --  ------------------------------
   --  Find the validator instance that was registered under the given name.
   --  Returns null if no such validator exist.
   --  ------------------------------
   function Find (Factory : in Component_Factory;
                  Name    : in EL.Objects.Object) return ASF.Validators.Validator_Access is
      Pos : constant Validator_Maps.Cursor := Factory.Validators.Find (Name);
   begin
      if Validator_Maps.Has_Element (Pos) then
         return Validator_Maps.Element (Pos);
      else
         return null;
      end if;
   end Find;

end ASF.Factory;
