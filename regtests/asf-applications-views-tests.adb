-----------------------------------------------------------------------
--  Render Tests - Unit tests for ASF.Applications.Views
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with Ada.Text_IO;
with ASF.Applications.Main;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with Ada.Directories;
with Util.Tests;
with Util.Files;
with Util.Measures;
package body ASF.Applications.Views.Tests is

   use AUnit;
   use Ada.Strings.Unbounded;

   overriding
   procedure Set_Up (T : in out Test) is
   begin
      null;
   end Set_Up;
   --  Set up performed before each test case

   --  Test loading of facelet file
   procedure Test_Load_Facelet (T : in out Test) is

      use ASF;
      use ASF.Contexts.Faces;

      App      : Applications.Main.Application;
--        H        : Applications.Views.View_Handler;

      View_Name   : constant String := To_String (T.File);
      Result_File : constant String := To_String (T.Result);
      Conf        : Applications.Config;
      App_Factory : Applications.Main.Application_Factory;
   begin
      Conf.Load_Properties ("regtests/view.properties");
      App.Initialize (Conf, App_Factory);

      App.Set_Global ("function", "Test_Load_Facelet");
      for I in 1 .. 2 loop
         declare
            S : Util.Measures.Stamp;
            Req       : ASF.Requests.Mockup.Request;
            Rep       : ASF.Responses.Mockup.Response;
            Content   : Unbounded_String;
         begin

            Req.Set_Method ("GET");
            Req.Set_Path_Info (View_Name);
            Req.Set_Parameter ("file-name", To_String (T.Name));
            Req.Set_Header ("file", To_String (T.Name));
            App.Dispatch (Page     => View_Name,
                          Request  => Req,
                          Response => Rep);
            Util.Measures.Report (S, "Pass" & Integer'Image (I) & ": Render view "
                                  & View_Name);

            Rep.Read_Content (Content);
            Util.Files.Write_File (Result_File, Content);
            Util.Tests.Assert_Equal_Files (T       => T,
                                           Expect  => To_String (T.Expect),
                                           Test    => Result_File,
                                           Message => "Restore and render view");
         end;
      end loop;
   end Test_Load_Facelet;


   --  Test case name
   overriding
   function Name (T : Test) return Message_String is
   begin
      return Format ("Test " & To_String (T.Name));
   end Name;

   --  Perform the test.
   overriding
   procedure Run_Test (T : in out Test) is
   begin
      T.Test_Load_Facelet;
   end Run_Test;

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
      use Ada.Directories;

      Result_Dir  : constant String := "regtests/result/views";
      Dir         : constant String := "regtests/files/views";
      Expect_Dir  : constant String := "regtests/expect/views";
      Path        : constant String := Util.Tests.Get_Path (Dir);
      Expect_Path : constant String := Util.Tests.Get_Path (Expect_Dir);
      Result_Path : constant String := Util.Tests.Get_Test_Path (Result_Dir);
      Search      : Search_Type;
      Filter      : constant Filter_Type := (others => True);
      Ent         : Directory_Entry_Type;
   begin
      if Kind (Path) = Directory then
         Ada.Text_IO.Put_Line ("Cannot read test directory: " & Path);
      end if;

      Start_Search (Search, Directory => Path, Pattern => "*.xhtml", Filter => Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Simple    : constant String := Simple_Name (Ent);
            File_Path : constant String := Full_Name (Ent);
            Tst      : Test_Case_Access;
         begin
            if Simple /= "." and then Simple /= ".."
              and then Simple /= ".svn" then
               Tst := new Test;
               Tst.Name := To_Unbounded_String (Dir & "/" & Simple);
               Tst.File := To_Unbounded_String (File_Path);
               Tst.Expect := To_Unbounded_String (Expect_Path & "/" & Simple);
               Tst.Result := To_Unbounded_String (Result_Path & "/" & Simple);
               Suite.Add_Test (Tst);
            end if;
         end;
      end loop;
   end Add_Tests;

end ASF.Applications.Views.Tests;
