-----------------------------------------------------------------------
--  asf-views-facelets -- Facelets representation and management
--  Copyright (C) 2009, 2010, 2011, 2014, 2015, 2017, 2019, 2020, 2021 Stephane Carrez
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

with Ada.Exceptions;
with Ada.Directories;
with Ada.Unchecked_Deallocation;
with ASF.Views.Nodes.Reader;
with Input_Sources.File;
with Sax.Readers;
with EL.Contexts.Default;
with Util.Files;
with Util.Log.Loggers;
package body ASF.Views.Facelets is

   use ASF.Views.Nodes;
   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Views.Facelets");

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => ASF.Views.File_Info,
                                      Name   => ASF.Views.File_Info_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => Facelet_Type,
                                      Name   => Facelet_Access);

   --  Find in the factory for the facelet with the given name.
   procedure Find (Factory : in out Facelet_Factory;
                   Name    : in String;
                   Result  : out Facelet);

   --  Load the facelet node tree by reading the facelet XHTML file.
   procedure Load (Factory : in out Facelet_Factory;
                   Name    : in String;
                   Context : in ASF.Contexts.Facelets.Facelet_Context'Class;
                   Result  : out Facelet;
                   Ignore  : in Boolean);

   --  Update the factory to store the facelet node tree
   procedure Update (Factory : in out Facelet_Factory;
                     Facelet : in Facelet_Access);

   --  ------------------------------
   --  Returns True if the facelet is null/empty.
   --  ------------------------------
   function Is_Null (F : Facelet) return Boolean is
   begin
      return F.Facelet = null;
   end Is_Null;

   --  ------------------------------
   --  Get the facelet identified by the given name.  If the facelet is already
   --  loaded, the cached value is returned.  The facelet file is searched in
   --  a set of directories configured in the facelet factory.
   --  ------------------------------
   procedure Find_Facelet (Factory : in out Facelet_Factory;
                           Name    : in String;
                           Context : in ASF.Contexts.Facelets.Facelet_Context'Class;
                           Result  : out Facelet;
                           Ignore  : in Boolean := False) is
   begin
      Log.Debug ("Find facelet {0}", Name);

      Find (Factory, Name, Result);
      if Result.Facelet = null then
         Load (Factory, Name, Context, Result, Ignore);
         if Result.Facelet = null then
            return;
         end if;
         Update (Factory, Result.Facelet);
      end if;
   end Find_Facelet;

   --  ------------------------------
   --  Create the component tree from the facelet view.
   --  ------------------------------
   procedure Build_View (View    : in Facelet;
                         Context : in out ASF.Contexts.Facelets.Facelet_Context'Class;
                         Root    : in ASF.Components.Base.UIComponent_Access) is
      Old : Unbounded_String;
   begin
      if View.Facelet /= null then
         Context.Set_Relative_Path (Path     => ASF.Views.Relative_Path (View.Facelet.File.all),
                                    Previous => Old);
         View.Facelet.Root.Build_Children (Parent => Root, Context => Context);
         Context.Set_Relative_Path (Path => Old);
      end if;
   end Build_View;

   --  ------------------------------
   --  Initialize the facelet factory.
   --  Set the search directories for facelet files.
   --  Set the ignore white space configuration when reading XHTML files.
   --  Set the ignore empty lines configuration when reading XHTML files.
   --  Set the escape unknown tags configuration when reading XHTML files.
   --  ------------------------------
   procedure Initialize (Factory             : in out Facelet_Factory;
                         Components          : access ASF.Factory.Component_Factory;
                         Paths               : in String;
                         Ignore_White_Spaces : in Boolean;
                         Ignore_Empty_Lines  : in Boolean;
                         Escape_Unknown_Tags : in Boolean) is
   begin
      Log.Info ("Set facelet search directory to: '{0}'", Paths);

      Factory.Factory := Components;
      Factory.Paths := To_Unbounded_String (Paths);
      Factory.Ignore_White_Spaces := Ignore_White_Spaces;
      Factory.Ignore_Empty_Lines  := Ignore_Empty_Lines;
      Factory.Escape_Unknown_Tags := Escape_Unknown_Tags;
   end Initialize;

   --  ------------------------------
   --  Find the facelet file in one of the facelet directories.
   --  Returns the path to be used for reading the facelet file.
   --  ------------------------------
   function Find_Facelet_Path (Factory : Facelet_Factory;
                               Name    : String) return String is
   begin
      return Util.Files.Find_File_Path (Name, To_String (Factory.Paths));
   end Find_Facelet_Path;

   --  ------------------------------
   --  Find in the factory for the facelet with the given name.
   --  ------------------------------
   procedure Find (Factory : in out Facelet_Factory;
                   Name    : in String;
                   Result  : out Facelet) is
      use Ada.Directories;
      use Ada.Calendar;
   begin
      Result.Facelet := Factory.Map.Find (Name);
      if Result.Facelet /= null then
         declare
            Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         begin
            if Result.Facelet.Check_Time < Now then
               if Modification_Time (Result.Facelet.File.Path) > Result.Facelet.Modify_Time then
                  Log.Info ("Ignoring cache because file '{0}' was modified",
                            Result.Facelet.File.Path);
                  Result.Facelet := null;
               else
                  Result.Facelet.Check_Time := Now + CHECK_FILE_DELAY;
               end if;
            end if;
         end;
      end if;
   end Find;

   --  ------------------------------
   --  Load the facelet node tree by reading the facelet XHTML file.
   --  ------------------------------
   procedure Load (Factory : in out Facelet_Factory;
                   Name    : in String;
                   Context : in ASF.Contexts.Facelets.Facelet_Context'Class;
                   Result  : out Facelet;
                   Ignore  : in Boolean) is
      Path   : constant String := Find_Facelet_Path (Factory, Name);
   begin
      if Path = "" or else not Ada.Directories.Exists (Path) then
         if not Ignore then
            Log.Warn ("Cannot read '{0}': file does not exist", Path);
         else
            Log.Debug ("No default navigation file for '{0}'", Path);
         end if;
         Result.Facelet := null;
         return;
      end if;

      declare
         use type Ada.Calendar.Time;
         Pos    : constant Integer := Path'Last - Name'Length + 1;
         Now    : constant Ada.Calendar.Time := Ada.Calendar.Clock + CHECK_FILE_DELAY;
         File   : File_Info_Access;
         Reader : ASF.Views.Nodes.Reader.Xhtml_Reader;
         Read   : Input_Sources.File.File_Input;
         Mtime  : Ada.Calendar.Time;
         Ctx    : aliased EL.Contexts.Default.Default_Context;
         Root   : ASF.Views.Nodes.Tag_Node_Access;
      begin
         if Pos <= Path'First then
            File := Create_File_Info (Path, Path'First);
         else
            File := Create_File_Info (Path, Pos);
         end if;
         Log.Info ("Loading facelet: '{0}' - {1} - {2}", Path, Name,
                   Natural'Image (File.Relative_Pos));

         Ctx.Set_Function_Mapper (Context.Get_Function_Mapper);
         Mtime  := Ada.Directories.Modification_Time (Path);
         Input_Sources.File.Open (Path, Read);

         --  If True, xmlns:* attributes will be reported in Start_Element
         Reader.Set_Feature (Sax.Readers.Namespace_Prefixes_Feature, False);
         Reader.Set_Feature (Sax.Readers.Validation_Feature, False);

         Reader.Set_Ignore_White_Spaces (Factory.Ignore_White_Spaces);
         Reader.Set_Escape_Unknown_Tags (Factory.Escape_Unknown_Tags);
         Reader.Set_Ignore_Empty_Lines (Factory.Ignore_Empty_Lines);
         begin
            Reader.Parse (File, Read, Factory.Factory, Ctx'Unchecked_Access);

         exception
            when ASF.Views.Nodes.Reader.Parsing_Error =>
               Free (File);

            when E : others =>
               Free (File);
               Log.Error ("Unexpected exception while reading: '{0}': {1}: {2}", Path,
                          Ada.Exceptions.Exception_Name (E), Ada.Exceptions.Exception_Message (E));
         end;
         Root := Reader.Get_Root;

         if File = null then
            if Root /= null then
               Root.Delete;
            end if;
            Result.Facelet := null;
         else
            Result.Facelet := new Facelet_Type '(Util.Refs.Ref_Entity with
                                                 Len         => Name'Length,
                                                 Root        => Root,
                                                 File        => File,
                                                 Modify_Time => Mtime,
                                                 Check_Time  => Now,
                                                 Name        => Name);
         end if;
         Input_Sources.File.Close (Read);
      end;
   end Load;

   --  ------------------------------
   --  Update the factory to store the facelet node tree
   --  ------------------------------
   procedure Update (Factory : in out Facelet_Factory;
                     Facelet : in Facelet_Access) is
   begin
      Factory.Map.Insert (Facelet);
   end Update;

   --  ------------------------------
   --  Clear the facelet cache
   --  ------------------------------
   procedure Clear_Cache (Factory : in out Facelet_Factory) is
   begin
      Log.Info ("Clearing facelet cache");

      Factory.Map.Clear;
   end Clear_Cache;

   protected body Facelet_Cache is

      --  ------------------------------
      --  Find the facelet entry associated with the given name.
      --  ------------------------------
      function Find (Name : in String) return Facelet_Access is
         Key    : aliased Facelet_Type := Facelet_Type '(Util.Refs.Ref_Entity with
                                                         Len    => Name'Length,
                                                         Name   => Name,
                                                         others => <>);
         Pos    : constant Facelet_Sets.Cursor := Map.Find (Key'Unchecked_Access);
      begin
         if Facelet_Sets.Has_Element (Pos) then
            return Element (Pos);
         else
            return null;
         end if;
      end Find;

      --  ------------------------------
      --  Insert or replace the facelet entry associated with the given name.
      --  ------------------------------
      procedure Insert (Facelet : in Facelet_Access) is
      begin
         Map.Include (Facelet);
      end Insert;

      --  ------------------------------
      --  Clear the cache.
      --  ------------------------------
      procedure Clear is
      begin
         loop
            declare
               Pos  : Facelet_Sets.Cursor := Map.First;
               Node : Facelet_Access;
            begin
               exit when not Has_Element (Pos);
               Node := Element (Pos);
               Map.Delete (Pos);
               Free (Node.File);
               ASF.Views.Nodes.Destroy (Node.Root);
               Free (Node);
            end;
         end loop;
      end Clear;

   end Facelet_Cache;

   --  ------------------------------
   --  Free the storage held by the factory cache.
   --  ------------------------------
   overriding
   procedure Finalize (Factory : in out Facelet_Factory) is
   begin
      Factory.Clear_Cache;
   end Finalize;

end ASF.Views.Facelets;
