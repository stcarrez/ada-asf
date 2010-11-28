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

with Ada.Strings.Fixed;
with Ada.Exceptions;
with Ada.Directories;
with Ada.IO_Exceptions;
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

   --  Find in the factory for the facelet with the given name.
   procedure Find (Factory : in out Facelet_Factory;
                   Name    : in Unbounded_String;
                   Result  : out Facelet);

   --  Load the facelet node tree by reading the facelet XHTML file.
   procedure Load (Factory : in out Facelet_Factory;
                   Name    : in String;
                   Context : in ASF.Contexts.Facelets.Facelet_Context'Class;
                   Result  : out Facelet);

   --  Update the factory to store the facelet node tree
   procedure Update (Factory : in out Facelet_Factory;
                     Name    : in Unbounded_String;
                     Item    : in Facelet);

   --  ------------------------------
   --  Returns True if the facelet is null/empty.
   --  ------------------------------
   function Is_Null (F : Facelet) return Boolean is
   begin
      return F.Root = null;
   end Is_Null;

   --  ------------------------------
   --  Get the facelet identified by the given name.  If the facelet is already
   --  loaded, the cached value is returned.  The facelet file is searched in
   --  a set of directories configured in the facelet factory.
   --  ------------------------------
   procedure Find_Facelet (Factory : in out Facelet_Factory;
                           Name    : in String;
                           Context : in ASF.Contexts.Facelets.Facelet_Context'Class;
                           Result  : out Facelet) is
      Res   : Facelet;
      Fname : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      Find (Factory, Fname, Res);
      if Res.Root = null then
         Load (Factory, Name, Context, Res);
         if Res.Root = null then
            Result.Root := null;
            return;
         end if;
         Update (Factory, Fname, Res);
      end if;
      Result.Root := Res.Root;
      Result.Path := Res.Path;
   end Find_Facelet;

   --  ------------------------------
   --  Create the component tree from the facelet view.
   --  ------------------------------
   procedure Build_View (View    : in Facelet;
                         Context : in out ASF.Contexts.Facelets.Facelet_Context'Class;
                         Root    : in ASF.Components.Base.UIComponent_Access) is
      Old : Unbounded_String;
   begin
      if View.Root /= null then
         Context.Set_Relative_Path (Path => View.Path, Previous => Old);
         View.Root.Build_Children (Parent => Root, Context => Context);
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
      use Util.Strings;

      Pos : constant Natural := Ada.Strings.Fixed.Index (Name, "/", Name'First + 1);
   begin
      if Pos > 0 then
         --  Get the module
         declare
            Module   : constant String := Name (Name'First + 1 .. Pos - 1);
            Path_Pos : constant Maps.Cursor
              := Factory.Path_Map.Find (Module);
         begin
            if Maps.Has_Element (Path_Pos) then
               Log.Info ("Looking module {0} in {1}", Module,
                         Maps.Element (Path_Pos));
               return Util.Files.Find_File_Path (Name (Pos + 1 .. Name'Last),
                                                 Maps.Element (Path_Pos));
            end if;
         end;
      end if;
      return Util.Files.Find_File_Path (Name, To_String (Factory.Paths));
   end Find_Facelet_Path;

   --  ------------------------------
   --  Register a module and directory where the module files are stored.
   --  ------------------------------
   procedure Register_Module (Factory : in out Facelet_Factory;
                              Name    : in String;
                              Paths   : in String) is
   begin
      Log.Info ("Search path for '{0}' is '{1}'", Name, Paths);

      Factory.Path_Map.Include (Name, Paths);
   end Register_Module;

   --  ------------------------------
   --  Find in the factory for the facelet with the given name.
   --  ------------------------------
   procedure Find (Factory : in out Facelet_Factory;
                   Name    : in Unbounded_String;
                   Result  : out Facelet) is
      use Ada.Directories;
      use Ada.Calendar;
   begin
      Result.Root := null;
      Factory.Lock.Read;
      declare
         Pos : constant Facelet_Maps.Cursor := Factory.Map.Find (Name);
      begin
         if Facelet_Maps.Has_Element (Pos) then
            Result := Element (Pos);
            if Modification_Time (Result.File.all) > Result.Modify_Time then
               Result.Root := null;
               Log.Info ("Ignoring cache because file '{0}' was modified",
                         Result.File.all);
            end if;
         end if;
      end;
      Factory.Lock.Release_Read;
   end Find;

   --  ------------------------------
   --  Load the facelet node tree by reading the facelet XHTML file.
   --  ------------------------------
   procedure Load (Factory : in out Facelet_Factory;
                   Name    : in String;
                   Context : in ASF.Contexts.Facelets.Facelet_Context'Class;
                   Result  : out Facelet) is
      use ASF.Views.Nodes.Reader;
      use Input_Sources.File;
      use Sax.Readers;
      use Ada.Exceptions;
      use Ada.Directories;

      Reader : Xhtml_Reader;
      Read   : File_Input;
      Path   : constant String := Find_Facelet_Path (Factory, Name);
      Ctx    : aliased EL.Contexts.Default.Default_Context;
      File   : String_Access := new String '(Path);
      Mtime  : Ada.Calendar.Time;
   begin
      Log.Info ("Loading facelet: '{0}'", Path);

      Ctx.Set_Function_Mapper (Context.Get_Function_Mapper);
      Mtime  := Modification_Time (Path);
      Open (Path, Read);

      --  If True, xmlns:* attributes will be reported in Start_Element
      Set_Feature (Reader, Namespace_Prefixes_Feature, False);
      Set_Feature (Reader, Validation_Feature, False);

      Set_Ignore_White_Spaces (Reader, Factory.Ignore_White_Spaces);
      Set_Escape_Unknown_Tags (Reader, Factory.Escape_Unknown_Tags);
      Set_Ignore_Empty_Lines (Reader, Factory.Ignore_Empty_Lines);
      Parse (Reader, File.all'Access,
             Read, Factory.Factory, Ctx'Unchecked_Access);
      Close (Read);

      Result := Facelet '(Root => Get_Root (Reader),
                          File => File.all'Access,
                          Modify_Time => Mtime,
                          Path => To_Unbounded_String (Containing_Directory (Path) & '/'));
   exception
      when Ada.IO_Exceptions.Name_Error =>
         Close (Read);
         Result.Root := Get_Root (Reader);
         if Result.Root /= null then
            Result.Root.Delete;
         end if;
         Free (File);
         Result.Root := null;
         Log.Error ("Cannot read '{0}': file does not exist", Path);

      when E : others =>
         Close (Read);
         Get_Root (Reader).Delete;
         Free (File);
         Result.Root := null;
         Log.Error ("Error while reading: '{0}': {1}: {2}", Path,
                   Exception_Name (E), Exception_Message (E));
   end Load;

   --  ------------------------------
   --  Update the factory to store the facelet node tree
   --  ------------------------------
   procedure Update (Factory : in out Facelet_Factory;
                     Name    : in Unbounded_String;
                     Item    : in Facelet) is
   begin
      Factory.Lock.Write;
      Factory.Map.Include (Name, Item);
      Factory.Lock.Release_Write;
   end Update;

   --  ------------------------------
   --  Clear the facelet cache
   --  ------------------------------
   procedure Clear_Cache (Factory : in out Facelet_Factory) is
   begin
      Log.Info ("Clearing facelet cache");

      Factory.Lock.Write;
      loop
         declare
            Pos  : Facelet_Maps.Cursor := Factory.Map.First;
            Node : Facelet;
         begin
            exit when not Has_Element (Pos);
            Node := Element (Pos);
            Factory.Map.Delete (Pos);
            Free (Node.File);
            ASF.Views.Nodes.Destroy (Node.Root);
         end;
      end loop;
      Factory.Lock.Release_Write;
   end Clear_Cache;

   protected body RW_Lock is
      entry Write when Reader_Count = 0 and Readable is
      begin
         Readable := False;
      end Write;

      procedure Release_Write is
      begin
         Readable := True;
      end Release_Write;

      entry Read when Readable is
      begin
         Reader_Count := Reader_Count + 1;
      end Read;

      procedure Release_Read is
      begin
         Reader_Count := Reader_Count - 1;
      end Release_Read;
   end RW_Lock;

   --  ------------------------------
   --  Free the storage held by the factory cache.
   --  ------------------------------
   overriding
   procedure Finalize (Factory : in out Facelet_Factory) is
   begin
      Factory.Clear_Cache;
   end Finalize;

end ASF.Views.Facelets;
