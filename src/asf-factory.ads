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

with ASF.Views.Nodes;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers;

--  The <b>ASF.Factory</b> is the main factory for building the facelet
--  node tree and defining associated component factory.  A binding library
--  must be registered before the application starts.  The binding library
--  must not be changed (this is a read-only static definition of names
--  associated with create functions).
package ASF.Factory is

   use ASF;

   Unknown_Name : exception;

   --  Binding name
   type Name_Access is access constant String;

   --  ------------------------------
   --  Binding definition.
   --  ------------------------------
   --  The binding links an XHTML entity name to a tag node implementation
   --  and a component creation handler.  When the XHTML entity is found,
   --  the associated binding is search and when found the node is created
   --  by using the <b>Tag</b> create function.
   type Binding is record
      Name      : Name_Access;
      Component : ASF.Views.Nodes.Create_Access;
      Tag       : ASF.Views.Nodes.Tag_Node_Create_Access;
   end record;

   --  ------------------------------
   --  List of bindings
   --  ------------------------------
   --  The binding array defines a set of XML entity names that represent
   --  a library accessible through a XML name-space.  The binding array
   --  must be sorted on the binding name.  The <b>Check</b> procedure will
   --  verify this assumption when the bindings are registered in the factory.
   type Binding_Array is array (Natural range <>) of Binding;
   type Binding_Array_Access is access constant Binding_Array;

   type Factory_Bindings is record
      URI      : Name_Access;
      Bindings : Binding_Array_Access;
   end record;
   type Factory_Bindings_Access is access constant Factory_Bindings;

   --  Find the create function associated with the name.
   --  Returns null if there is no binding associated with the name.
   function Find (Factory : Factory_Bindings;
                  Name    : String) return Binding;

   --  Check the definition of the component factory.
   procedure Check (Factory : in Factory_Bindings);

   --  ------------------------------
   --  Component Factory
   --  ------------------------------
   --  The <b>Component_Factory</b> is the main entry point to register bindings
   --  and resolve them when an XML file is read.
   type Component_Factory is limited private;

   --  Register a binding library in the factory.
   procedure Register (Factory  : in out Component_Factory;
                       Bindings : in Factory_Bindings_Access);

   --  Find the create function in bound to the name in the given URI name-space.
   --  Returns null if no such binding exist.
   function Find (Factory : Component_Factory;
                  URI     : String;
                  Name    : String) return Binding;

private

   function Hash (Key : Name_Access) return Ada.Containers.Hash_Type;

   function Equivalent_Keys (Left, Right : Name_Access) return Boolean;

   --  Tag library map indexed on the library namespace.
   package Factory_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => Name_Access,
                                            Element_Type    => Factory_Bindings_Access,
                                            Hash            => Hash,
                                            Equivalent_Keys => Equivalent_Keys);

   type Component_Factory is limited record
      Map : Factory_Maps.Map;
   end record;

end ASF.Factory;
