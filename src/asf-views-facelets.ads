-----------------------------------------------------------------------
--  asf-views-facelets -- Facelets representation and management
--  Copyright (C) 2009, 2010, 2011, 2014, 2015, 2019, 2021 Stephane Carrez
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
with ASF.Views.Nodes;
with ASF.Contexts.Facelets;
with ASF.Factory;
with ASF.Components.Base;
with Ada.Finalization;
private with Ada.Strings.Unbounded;
private with Ada.Containers.Hashed_Sets;
private with Util.Refs;
private with Ada.Strings.Hash;

--  The <b>ASF.Views.Facelets</b> package contains the facelet factory
--  responsible for getting the facelet tree from a facelet name.
--  The facelets (or *.xhtml) files are loaded by the reader to form
--  a tag node tree which is cached by the factory.  The facelet factory
--  is shared by multiple requests and threads.
package ASF.Views.Facelets is

   type Facelet is private;

   --  Returns True if the facelet is null/empty.
   function Is_Null (F : Facelet) return Boolean;

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
                           Result  : out Facelet;
                           Ignore  : in Boolean := False);

   --  Create the component tree from the facelet view.
   procedure Build_View (View    : in Facelet;
                         Context : in out ASF.Contexts.Facelets.Facelet_Context'Class;
                         Root    : in ASF.Components.Base.UIComponent_Access);

   --  Initialize the facelet factory.
   --  Set the search directories for facelet files.
   --  Set the ignore white space configuration when reading XHTML files.
   --  Set the ignore empty lines configuration when reading XHTML files.
   --  Set the escape unknown tags configuration when reading XHTML files.
   procedure Initialize (Factory             : in out Facelet_Factory;
                         Components          : access ASF.Factory.Component_Factory;
                         Paths               : in String;
                         Ignore_White_Spaces : in Boolean;
                         Ignore_Empty_Lines  : in Boolean;
                         Escape_Unknown_Tags : in Boolean);

   --  Find the facelet file in one of the facelet directories.
   --  Returns the path to be used for reading the facelet file.
   function Find_Facelet_Path (Factory : in Facelet_Factory;
                               Name    : in String) return String;

   --  Clear the facelet cache
   procedure Clear_Cache (Factory : in out Facelet_Factory);

private

   use Ada.Strings.Unbounded;

   CHECK_FILE_DELAY : constant := 10.0;

   type Facelet_Type (Len : Natural) is limited new Util.Refs.Ref_Entity with record
      Root        : ASF.Views.Nodes.Tag_Node_Access;
      File        : ASF.Views.File_Info_Access;
      Modify_Time : Ada.Calendar.Time;
      Check_Time  : Ada.Calendar.Time;
      Name        : String (1 .. Len);
   end record;
   type Facelet_Access is access all Facelet_Type;

   package Ref is
      new Util.Refs.Indefinite_References (Facelet_Type, Facelet_Access);

   type Facelet is record
      Facelet : Facelet_Access;
   end record;

   Empty : constant Facelet := (others => <>);

   function Hash (Item : in Facelet_Access) return Ada.Containers.Hash_Type is
      (Ada.Strings.Hash (Item.Name));

   function Compare (Left, Right : in Facelet_Access) return Boolean is
      (Left.Name = Right.Name);

   --  Tag library map indexed on the library namespace.
   package Facelet_Sets is new
     Ada.Containers.Hashed_Sets (Element_Type        => Facelet_Access,
                                 Hash                => Hash,
                                 Equivalent_Elements => Compare);

   use Facelet_Sets;

   protected type Facelet_Cache is
      --  Find the facelet entry associated with the given name.
      function Find (Name : in String) return Facelet_Access;

      --  Insert or replace the facelet entry associated with the given name.
      procedure Insert (Facelet : in Facelet_Access);

      --  Clear the cache.
      procedure Clear;
   private
      Map     : Facelet_Sets.Set;
   end Facelet_Cache;

   type Facelet_Factory is new Ada.Finalization.Limited_Controlled with record
      Paths    : Unbounded_String := To_Unbounded_String ("");

      --  The facelet cache.
      Map      : Facelet_Cache;

      --  The component factory
      Factory  : access ASF.Factory.Component_Factory;

      --  Whether the unknown tags are escaped using XML escape rules.
      Escape_Unknown_Tags : Boolean := True;

      --  Whether white spaces can be ignored.
      Ignore_White_Spaces : Boolean := True;

      --  Whether empty lines should be ignored (when white spaces are kept).
      Ignore_Empty_Lines : Boolean := True;
   end record;

   --  Free the storage held by the factory cache.
   overriding
   procedure Finalize (Factory : in out Facelet_Factory);

end ASF.Views.Facelets;
