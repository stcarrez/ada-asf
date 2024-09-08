-----------------------------------------------------------------------
--  asf-views -- Views
--  Copyright (C) 2011, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
