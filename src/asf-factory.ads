-----------------------------------------------------------------------
--  asf-factory -- Component and tag factory
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2018, 2022 Stephane Carrez
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

with ASF.Views.Nodes;
with ASF.Converters;
with ASF.Validators;
with EL.Objects;

private with Util.Beans.Objects.Hash;
private with Ada.Containers;
private with Ada.Containers.Hashed_Maps;

--  The <b>ASF.Factory</b> is the main factory for building the facelet
--  node tree and defining associated component factory.  A binding library
--  must be registered before the application starts.  The binding library
--  must not be changed (this is a read-only static definition of names
--  associated with create functions).
package ASF.Factory is

   --  ------------------------------
   --  List of bindings
   --  ------------------------------
   --  The binding array defines a set of XML entity names that represent
   --  a library accessible through a XML name-space.  The binding array
   --  must be sorted on the binding name.  The <b>Check</b> procedure will
   --  verify this assumption when the bindings are registered in the factory.
   type Binding_Array is array (Natural range <>) of aliased ASF.Views.Nodes.Binding_Type;
   type Binding_Array_Access is access constant Binding_Array;

   type Factory_Bindings is limited record
      URI      : ASF.Views.Nodes.Name_Access;
      Bindings : Binding_Array_Access;
   end record;
   type Factory_Bindings_Access is access constant Factory_Bindings;

   --  ------------------------------
   --  Component Factory
   --  ------------------------------
   --  The <b>Component_Factory</b> is the main entry point to register bindings
   --  and resolve them when an XML file is read.
   type Component_Factory is limited private;

   --  Register a binding library in the factory.
   procedure Register (Factory  : in out Component_Factory;
                       Bindings : in Factory_Bindings_Access);

   procedure Register (Factory   : in out Component_Factory;
                       URI       : in ASF.Views.Nodes.Name_Access;
                       Name      : in ASF.Views.Nodes.Name_Access;
                       Tag       : in ASF.Views.Nodes.Tag_Node_Create_Access;
                       Create    : in ASF.Views.Nodes.Create_Access);

   --  Find the create function in bound to the name in the given URI name-space.
   --  Returns null if no such binding exist.
   function Find (Factory : in Component_Factory;
                  URI     : in String;
                  Name    : in String) return ASF.Views.Nodes.Binding_Type;

   --  ------------------------------
   --  Converter Factory
   --  ------------------------------
   --  The <b>Converter_Factory</b> registers the converters which can be used
   --  to convert a value into a string or the opposite.

   --  Register the converter instance under the given name.
   procedure Register (Factory   : in out Component_Factory;
                       Name      : in String;
                       Converter : in ASF.Converters.Converter_Access);

   --  Find the converter instance that was registered under the given name.
   --  Returns null if no such converter exist.
   function Find (Factory : in Component_Factory;
                  Name    : in EL.Objects.Object) return ASF.Converters.Converter_Access;

   --  ------------------------------
   --  Validator Factory
   --  ------------------------------

   --  Register the validator instance under the given name.
   procedure Register (Factory   : in out Component_Factory;
                       Name      : in String;
                       Validator : in ASF.Validators.Validator_Access);

   --  Find the validator instance that was registered under the given name.
   --  Returns null if no such validator exist.
   function Find (Factory : in Component_Factory;
                  Name    : in EL.Objects.Object) return ASF.Validators.Validator_Access;

private

   use ASF.Converters;
   use ASF.Validators;
   use ASF.Views.Nodes;

   --  The tag name defines a URI with the name.
   type Tag_Name is record
      URI  : ASF.Views.Nodes.Name_Access;
      Name : ASF.Views.Nodes.Name_Access;
   end record;

   --  Compute a hash for the tag name.
   function Hash (Key : in Tag_Name) return Ada.Containers.Hash_Type;

   --  Returns true if both tag names are identical.
   overriding
   function "=" (Left, Right : in Tag_Name) return Boolean;

   --  Tag library map indexed on the library namespace.
   package Factory_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => Tag_Name,
                                 Element_Type    => Binding_Type,
                                 Hash            => Hash,
                                 Equivalent_Keys => "=");

   --  Converter map indexed on the converter name.
   --  The key is an EL.Objects.Object to minimize the conversions when searching
   --  for a converter.
   package Converter_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => EL.Objects.Object,
                                 Element_Type    => Converter_Access,
                                 Hash            => EL.Objects.Hash,
                                 Equivalent_Keys => EL.Objects."=");

   --  Validator map indexed on the validator name.
   --  The key is an EL.Objects.Object to minimize the conversions when searching
   --  for a validator.
   package Validator_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => EL.Objects.Object,
                                 Element_Type    => Validator_Access,
                                 Hash            => EL.Objects.Hash,
                                 Equivalent_Keys => EL.Objects."=");

   type Component_Factory is limited record
      Map        : Factory_Maps.Map;
      Converters : Converter_Maps.Map;
      Validators : Validator_Maps.Map;
   end record;

end ASF.Factory;
