-----------------------------------------------------------------------
--  Render Tests - Unit tests for ASF.Applications.Views
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

with AUnit.Simple_Test_Cases;
with Ada.Text_IO;
with ASF.Applications.Views;
with ASF.Components.Core;
with EL.Contexts;
with EL.Contexts.Default;
with EL.Variables.Default;
with ASF.Contexts.Writer.Tests;
with Ada.Directories;
with Util.Tests;
with Util.Files;
with Util.Measures;
package body ASF.Applications.Views.Tests is

   use AUnit;
   use Ada.Strings.Unbounded;
   use ASF.Contexts.Writer.Tests;

   overriding
   procedure Set_Up (T : in out Test) is
   begin
      null;
   end Set_Up;
   --  Set up performed before each test case

   overriding
   procedure Tear_Down (T : in out Test) is
   begin
      null;
   end Tear_Down;
   --  Tear down performed after each test case

   --  Test loading of facelet file
   procedure Test_Load_Facelet (T : in out Test) is

      use ASF;
      use ASF.Contexts.Faces;

      use EL.Contexts.Default;
      use EL.Variables;
      use EL.Variables.Default;
      use EL.Contexts;

      H        : Applications.Views.View_Handler;
      Writer   : aliased Test_Writer;
      Context  : aliased Faces_Context;
      View     : Components.Core.UIViewRoot;
      ELContext : aliased EL.Contexts.Default.Default_Context;
      Variables : aliased Default_Variable_Mapper;
      Resolver  : aliased Default_ELResolver;

      View_Name   : constant String := To_String (T.File);
      Result_File : constant String := To_String (T.Result);
      S : Util.Measures.Stamp;
      Conf      : Applications.Config;
   begin
      Conf.Set ("view.file_ext", "");
      H.Initialize (Conf);

      Context.Set_Response_Writer (Writer'Unchecked_Access);
      Context.Set_ELContext (ELContext'Unchecked_Access);
      Writer.Initialize ("text/xml", "UTF-8", 8192);

      Set_Current (Context'Unchecked_Access);
      H.Restore_View (View_Name, Context, View);

      H.Render_View (Context, View);
      Writer.Flush;
      Util.Measures.Report (S, "Render view " & View_Name);

      Util.Files.Write_File (Result_File, Writer.Response);
      Util.Tests.Assert_Equal_Files (Expect  => To_String (T.Expect),
                                     Test    => Result_File,
                                     Message => "Restore and render view");
   end Test_Load_Facelet;

   type Test_Case is new AUnit.Simple_Test_Cases.Test_Case with record
      Name    : Unbounded_String;
      File    : Unbounded_String;
      Expect  : Unbounded_String;
      Result  : Unbounded_String;
      Fixture : Test;
   end record;
   type Test_Case_Access is access all Test_Case;

   --  Test case name
   overriding
   function Name (Test : Test_Case) return Message_String;

   --  Perform the test.
   overriding
   procedure Run_Test (Test : in out Test_Case);

   overriding
   procedure Set_Up (Test : in out Test_Case);

   --  Test case name
   overriding
   function Name (Test : Test_Case) return Message_String is
   begin
      return Format ("Test " & To_String (Test.Name));
   end Name;

   --  Perform the test.
   overriding
   procedure Run_Test (Test : in out Test_Case) is
   begin
      Test.Fixture.Test_Load_Facelet;
   end Run_Test;

   overriding
   procedure Set_Up (Test : in out Test_Case) is
   begin
      Test.Fixture.File   := Test.File;
      Test.Fixture.Expect := Test.Expect;
      Test.Fixture.Result := Test.Result;
      Test.Fixture.Set_Up;
   end Set_Up;

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
      use Ada.Directories;

      Result_Dir  : constant String := "regtests/result/views";
      Dir         : constant String := "regtests/files/views";
      Expect_Dir  : constant String := "regtests/expect/views";
      Path        : constant String := Util.Tests.Get_Path (Dir);
      Expect_Path : constant String := Util.Tests.Get_Path (Expect_Dir);
      Result_Path : constant String := Util.Tests.Get_Test_Path (Result_Dir);
      Search      : Search_Type;
      Filter      : Filter_Type := (others => True);
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
            Test      : Test_Case_Access;
         begin
            if Simple /= "." and then Simple /= ".."
              and then Simple /= ".svn" then
               Test := new Test_Case;
               Test.Name := To_Unbounded_String (Dir & "/" & Simple);
               Test.File := To_Unbounded_String (File_Path);
               Test.Expect := To_Unbounded_String (Expect_Path & "/" & Simple);
               Test.Result := To_Unbounded_String (Result_Path & "/" & Simple);
               Suite.Add_Test (Test);
            end if;
         end;
      end loop;
   end Add_Tests;

end ASF.Applications.Views.Tests;
