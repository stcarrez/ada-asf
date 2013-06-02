-----------------------------------------------------------------------
--  asf-views -- Views
--  Copyright (C) 2009, 2010, 2011, 2012, 2013 Stephane Carrez
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

--  The <b>ASF.Views</b> package defines the abstractions to represent
--  an XHTML view that can be instantiated to create the JSF component
--  tree.  The <b>ASF.Views</b> are read only once and they are shared
--  by the JSF component trees that are instantiated from it.
--
--  The XHTML view is read using a SAX parser which creates nodes
--  and attributes to represent the view.
--
--  The <b>ASF.Views</b> is composed of nodes represented by <b>Tag_Node</b>
--  and attributes represented by <b>Tag_Attribute</b>.  In a sense, this
--  is very close to an XML DOM tree.
package ASF.Views is

   pragma Preelaborate;

   --  ------------------------------
   --  Source line information
   --  ------------------------------
   type File_Info (<>) is limited private;
   type File_Info_Access is access all File_Info;

   --  Create a <b>File_Info</b> record to identify the file whose path is <b>Path</b>
   --  and whose relative path portion starts at <b>Relative_Position</b>.
   function Create_File_Info (Path              : in String;
                              Relative_Position : in Natural) return File_Info_Access;

   --  Get the relative path name
   function Relative_Path (File : in File_Info) return String;

   type Line_Info is private;

   --  Get the line number
   function Line (Info : in Line_Info) return Natural;
   pragma Inline (Line);

   --  Get the source file
   function File (Info : in Line_Info) return String;
   pragma Inline (File);

private
   type File_Info (Length : Natural) is limited record
      Relative_Pos : Natural;
      Path         : String (1 .. Length);
   end record;

   NO_FILE : aliased File_Info := File_Info '(Length => 0, Path => "", Relative_Pos => 0);

   type Line_Info is record
      Line   : Natural := 0;
      Column : Natural := 0;
      File   : File_Info_Access := NO_FILE'Access;
   end record;

end ASF.Views;
