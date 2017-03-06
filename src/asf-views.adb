-----------------------------------------------------------------------
--  asf-views -- Views
--  Copyright (C) 2011, 2017 Stephane Carrez
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

package body ASF.Views is

   --  ------------------------------
   --  Source line information
   --  ------------------------------

   --  ------------------------------
   --  Create a <b>File_Info</b> record to identify the file whose path is <b>Path</b>
   --  and whose relative path portion starts at <b>Relative_Position</b>.
   --  ------------------------------
   function Create_File_Info (Path              : in String;
                              Relative_Position : in Natural) return File_Info_Access is
   begin
      return new File_Info '(Length       => Path'Length,
                             Path         => Path,
                             Relative_Pos => Relative_Position - Path'First + 1);
   end Create_File_Info;

   --  ------------------------------
   --  Get the relative path name
   --  ------------------------------
   function Relative_Path (File : in File_Info) return String is
   begin
      return File.Path (File.Relative_Pos .. File.Path'Last);
   end Relative_Path;

   --  ------------------------------
   --  Get the line number
   --  ------------------------------
   function Line (Info : Line_Info) return Natural is
   begin
      return Info.Line;
   end Line;

   --  ------------------------------
   --  Get the source file
   --  ------------------------------
   function File (Info : Line_Info) return String is
   begin
      return Info.File.Path;
   end File;

end ASF.Views;
