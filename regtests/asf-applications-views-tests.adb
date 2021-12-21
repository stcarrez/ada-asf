-----------------------------------------------------------------------
--  asf-applications-views-tests - Unit tests for ASF.Applications.Views
--  Copyright (C) 2009, 2010, 2011, 2012, 2014, 2015, 2018, 2019, 2021 Stephane Carrez
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
with ASF.Applications.Tests;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Servlets.Faces;
with ASF.Converters.Dates;
with ASF.Server;
with Ada.Directories;
with Ada.Unchecked_Deallocation;

with Util.Files;
with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Measures;
package body ASF.Applications.Views.Tests is

   use Ada.Strings.Unbounded;

   overriding
   procedure Set_Up (T : in out Test) is
   begin
      null;
   end Set_Up;
   --  Set up performed before each test case

   --  ------------------------------
   --  Test loading of facelet file
   --  ------------------------------
   procedure Test_Load_Facelet (T : in out Test) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => ASF.Converters.Dates.Date_Converter'Class,
                                        Name   => ASF.Converters.Dates.Date_Converter_Access);

      App         : aliased Applications.Main.Application;
      View_Name   : constant String := To_String (T.File);
      Result_File : constant String := To_String (T.Result);
      Conf        : Applications.Config;
      App_Factory : Applications.Main.Application_Factory;
      Dir         : constant String := "regtests/files";
      Path        : constant String := Util.Tests.Get_Path (Dir);
      Faces       : aliased ASF.Servlets.Faces.Faces_Servlet;
      List        : Util.Beans.Basic.Readonly_Bean_Access;
      List_Bean   : Util.Beans.Objects.Object;
      Form        : Util.Beans.Basic.Readonly_Bean_Access;
      Form_Bean   : Util.Beans.Objects.Object;
      C           : ASF.Converters.Dates.Date_Converter_Access;
      Container   : ASF.Server.Container;
   begin
      List := Applications.Tests.Create_Form_List;
      List_Bean := Util.Beans.Objects.To_Object (List);

      Form := Applications.Tests.Create_Form_Bean;
      Form_Bean := Util.Beans.Objects.To_Object (Form);

      Conf.Load_Properties ("regtests/view.properties");
      Conf.Set ("view.dir", Path);
      App.Initialize (Conf, App_Factory);
      App.Register_Application ("/");
      App.Add_Servlet ("faces", Faces'Unchecked_Access);
      App.Add_Mapping ("*.xhtml", "faces");
      C := ASF.Converters.Dates.Create_Date_Converter (Date    => ASF.Converters.Dates.DEFAULT,
                                                       Time    => ASF.Converters.Dates.DEFAULT,
                                                       Format  => ASF.Converters.Dates.TIME,
                                                       Locale  => "en",
                                                       Pattern => "");
      App.Add_Converter ("date-default-converter", C.all'Access);

      App.Set_Global ("function", "Test_Load_Facelet");
      App.Set_Global ("date", "2011-12-03 03:04:05.23");
      Container.Register_Application ("/asfunit", App'Unchecked_Access);
      Container.Start;
      for I in 1 .. 2 loop
         declare
            S : Util.Measures.Stamp;
            Req       : ASF.Requests.Mockup.Request;
            Reply     : aliased ASF.Responses.Mockup.Response;
            Content   : Unbounded_String;
         begin
            Req.Set_Method ("GET");
            Req.Set_Request_URI ("/asfunit/" & View_Name);
            Req.Set_Parameter ("file-name", To_String (T.Name));
            Req.Set_Header ("file", To_String (T.Name));
            Req.Set_Attribute ("list", List_Bean);
            Req.Set_Attribute ("form", Form_Bean);
            Container.Service (Req, Reply);
            Util.Measures.Report (S, "Pass" & Integer'Image (I) & ": Render view "
                                  & View_Name);

            Reply.Read_Content (Content);
            Util.Files.Write_File (Result_File, Content);
            Util.Tests.Assert_Equal_Files (T       => T,
                                           Expect  => To_String (T.Expect),
                                           Test    => Result_File,
                                           Message => "Restore and render view");
         end;
      end loop;
      Free (C);
   end Test_Load_Facelet;

   --  ------------------------------
   --  Test case name
   --  ------------------------------
   overriding
   function Name (T : Test) return Util.Tests.Message_String is
   begin
      return Util.Tests.Format ("Test " & To_String (T.Name));
   end Name;

   --  ------------------------------
   --  Perform the test.
   --  ------------------------------
   overriding
   procedure Run_Test (T : in out Test) is
   begin
      T.Test_Load_Facelet;
   end Run_Test;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
      use Ada.Directories;

      Result_Dir  : constant String := "views";
      Dir         : constant String := "regtests/files/views";
      Expect_Dir  : constant String := "regtests/expect/views";
      Path        : constant String := Util.Tests.Get_Path (Dir);
      Expect_Path : constant String := Util.Tests.Get_Path (Expect_Dir);
      Result_Path : constant String := Util.Tests.Get_Test_Path (Result_Dir);
      Search      : Search_Type;
      Filter      : constant Filter_Type := (others => True);
      Ent         : Directory_Entry_Type;
   begin
      if Kind (Path) /= Directory then
         Ada.Text_IO.Put_Line ("Cannot read test directory: " & Path);
      end if;

      Start_Search (Search, Directory => Path, Pattern => "*.xhtml", Filter => Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Ent);
         declare
            Simple    : constant String := Simple_Name (Ent);
            Tst      : Test_Case_Access;
         begin
            if Simple /= "." and then Simple /= ".."
              and then Simple /= ".svn"
            then
               Tst := new Test;
               Tst.Name := To_Unbounded_String (Dir & "/" & Simple);
               Tst.File := To_Unbounded_String ("views/" & Simple);
               Tst.Expect := To_Unbounded_String (Expect_Path & "/" & Simple);
               Tst.Result := To_Unbounded_String (Result_Path & "/" & Simple);
               Suite.Add_Test (Tst.all'Access);
            end if;
         end;
      end loop;
   end Add_Tests;

end ASF.Applications.Views.Tests;
