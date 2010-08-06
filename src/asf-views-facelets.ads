-----------------------------------------------------------------------
--  asf-views-facelets -- Facelets representation and management
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

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with ASF.Views.Nodes;
with ASF.Contexts.Facelets;
with ASF.Factory;
with ASF.Components;
with Util.Strings;

--  The <b>ASF.Views.Facelets</b> package contains the facelet factory
--  responsible for getting the facelet tree from a facelet name.
--  The facelets (or *.xhtml) files are loaded by the reader to form
--  a tag node tree which is cached by the factory.  The facelet factory
--  is shared by multiple requests and threads.
package ASF.Views.Facelets is

   type Facelet is private;
   type Facelet_Access is access all Facelet;

   --  ------------------------------
   --  Facelet factory
   --  ------------------------------
   --  The facelet factory allows to retrieve the node tree to build the
   --  component tree.  The node tree can be shared by several component trees.
   --  The node tree is initialized from the <b>XHTML</b> view file.  It is put
   --  in a cache to avoid loading and parsing the file several times.
   type Facelet_Factory is limited private;

   --  Get the facelet identified by the given name.  If the facelet is already
   --  loaded, the cached value is returned.  The facelet file is searched in
   --  a set of directories configured in the facelet factory.
   procedure Find_Facelet (Factory : in out Facelet_Factory;
                           Name    : in String;
                           Context : in ASF.Contexts.Facelets.Facelet_Context'Class;
                           Result  : out Facelet);

   --  Create the component tree from the facelet view.
   procedure Build_View (View    : in Facelet;
                         Context : in out ASF.Contexts.Facelets.Facelet_Context'Class;
                         Root    : in ASF.Components.UIComponent_Access);

   --  Set the search directories for facelet files.
   procedure Set_Search_Directory (Factory : in out Facelet_Factory;
                                   Paths   : in String);

   --  Find the facelet file in one of the facelet directories.
   --  Returns the path to be used for reading the facelet file.
   function Find_Facelet_Path (Factory : Facelet_Factory;
                               Name    : String) return String;

   --  Register the component factory bindings in the facelet factory.
   procedure Register (Factory  : in out Facelet_Factory;
                       Bindings : in ASF.Factory.Factory_Bindings_Access);

   --  Register a module and directory where the module files are stored.
   procedure Register_Module (Factory : in out Facelet_Factory;
                              Name    : in String;
                              Paths   : in String);

   --  Clear the facelet cache
   procedure Clear_Cache (Factory : in out Facelet_Factory);

private

   use Ada.Strings.Unbounded;

   type Facelet is record
      Root : ASF.Views.Nodes.Tag_Node_Access;
      Path : Unbounded_String;
      File : Util.Strings.Name_Access;
   end record;

   --  Tag library map indexed on the library namespace.
   package Facelet_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => Unbounded_String,
                                            Element_Type    => Facelet,
                                            Hash            => Ada.Strings.Unbounded.Hash,
                                            Equivalent_Keys => "=");

   use Facelet_Maps;

   --  Lock for accessing the shared cache
   protected type RW_Lock is
      entry Read;

      procedure Release_Read;

      entry Write;

      procedure Release_Write;
   private
      Readable     : Boolean := True;
      Reader_Count : Natural := 0;
   end RW_Lock;

   type Facelet_Factory is limited record
      Paths   : Unbounded_String := To_Unbounded_String ("");
      Lock    : RW_Lock;
      Map     : Facelet_Maps.Map;

      Path_Map : Util.Strings.String_Map.Map;

      --  The component factory
      Factory  : aliased ASF.Factory.Component_Factory;
   end record;

end ASF.Views.Facelets;
