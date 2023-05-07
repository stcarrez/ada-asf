-----------------------------------------------------------------------
--  asf-tests - ASF Tests Framework
--  Copyright (C) 2011, 2012, 2013, 2015, 2017, 2018, 2019, 2023 Stephane Carrez
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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Util.Files;

with Servlet.Core;
with ASF.Servlets.Faces;
with ASF.Servlets.Ajax;
with Servlet.Filters;
with Servlet.Core.Files;
with Servlet.Core.Measures;
with ASF.Responses;

with ASF.Contexts.Faces;
with EL.Variables.Default;

package body ASF.Tests is

   CONTEXT_PATH : constant String := "/asfunit";

   App_Created : ASF.Applications.Main.Application_Access;
   App         : ASF.Applications.Main.Application_Access;
   Faces       : aliased ASF.Servlets.Faces.Faces_Servlet;
   Ajax        : aliased ASF.Servlets.Ajax.Ajax_Servlet;
   Files       : aliased Servlet.Core.Files.File_Servlet;
   Measures    : aliased Servlet.Core.Measures.Measure_Servlet;

   --  ------------------------------
   --  Initialize the awa test framework mockup.
   --  ------------------------------
   procedure Initialize (Props       : in Util.Properties.Manager;
                         Application : in ASF.Applications.Main.Application_Access := null;
                         Factory     : in out ASF.Applications.Main.Application_Factory'Class) is
      use type ASF.Applications.Main.Application_Access;

      C        : ASF.Applications.Config;
      Empty    : Util.Properties.Manager;
   begin
      if Application /= null then
         App := Application;
      else
         if App_Created = null then
            App_Created := new ASF.Applications.Main.Application;
         end if;
         App := App_Created;
      end if;

      C.Copy (Props);
      App.Initialize (C, Factory);
      App.Register ("layoutMsg", "layout");
      App.Set_Global ("contextPath", CONTEXT_PATH);

      --  Register the servlets and filters
      App.Add_Servlet (Name => "faces", Server => Faces'Access);
      App.Add_Servlet (Name => "ajax", Server => Ajax'Access);
      App.Add_Servlet (Name => "files", Server => Files'Access);
      App.Add_Servlet (Name => "measures", Server => Measures'Access);
      App.Add_Filter (Name => "measures",
                      Filter => Servlet.Filters.Filter'Class (Measures)'Access);

      --  Define servlet mappings
      App.Add_Mapping (Name => "faces", Pattern => "*.html");
      App.Add_Mapping (Name => "ajax", Pattern => "/ajax/*");
      App.Add_Mapping (Name => "files", Pattern => "*.css");
      App.Add_Mapping (Name => "files", Pattern => "*.js");
      App.Add_Mapping (Name => "files", Pattern => "*.html");
      App.Add_Mapping (Name => "files", Pattern => "*.txt");
      App.Add_Mapping (Name => "files", Pattern => "*.png");
      App.Add_Mapping (Name => "files", Pattern => "*.jpg");
      App.Add_Mapping (Name => "files", Pattern => "*.gif");
      App.Add_Mapping (Name => "files", Pattern => "*.pdf");
      App.Add_Mapping (Name => "files", Pattern => "*.properties");
      App.Add_Mapping (Name => "files", Pattern => "*.xhtml");
      App.Add_Mapping (Name => "measures", Pattern => "stats.xml");

      App.Add_Filter_Mapping (Name => "measures", Pattern => "*");
      App.Add_Filter_Mapping (Name => "measures", Pattern => "/ajax/*");
      App.Add_Filter_Mapping (Name => "measures", Pattern => "*.html");
      App.Add_Filter_Mapping (Name => "measures", Pattern => "*.xhtml");

      Servlet.Tests.Initialize (Empty, CONTEXT_PATH, App.all'Access);
   end Initialize;

   --  ------------------------------
   --  Get the test application.
   --  ------------------------------
   function Get_Application return ASF.Applications.Main.Application_Access is
      use type Servlet.Core.Servlet_Registry_Access;
      Result : constant Servlet.Core.Servlet_Registry_Access := Servlet.Tests.Get_Application;
   begin
      if Result = null then
         return App;
      elsif Result.all in ASF.Applications.Main.Application'Class then
         return ASF.Applications.Main.Application'Class (Result.all)'Access;
      else
         return App;
      end if;
   end Get_Application;

   --  ------------------------------
   --  Extract from the response output saved in `Filename` the form parameter
   --  that corresponds to the `Field` hidden field.
   --  ------------------------------
   function Extract (Field    : in String;
                     Filename : in String) return String is
      use Ada.Strings.Unbounded;
      procedure Process (Line : in String);

      Pattern : constant String := "<input type=""hidden"" name=""" & Field & """ value=""";
      Result  : Ada.Strings.Unbounded.Unbounded_String;

      procedure Process (Line : in String) is
         Pos, Sep : Natural;
      begin
         Pos := Ada.Strings.Fixed.Index (Line, Pattern);
         if Pos > 0 then
            Pos := Pos + Pattern'Length;
            Sep := Ada.Strings.Fixed.Index (Line, """", Pos);
            if Sep > 0 then
               Result := To_Unbounded_String (Line (Pos .. Sep - 1));
            end if;
         end if;
      end Process;
   begin
      Util.Files.Read_File (Util.Tests.Get_Test_Path (Filename), Process'Access);
      return To_String (Result);
   end Extract;

   --  ------------------------------
   --  Set in the request the CSRF form parameter identified by `Field` that
   --  can be extracted from the response output saved in `Filename`.
   --  ------------------------------
   procedure Set_CSRF (Request  : in out ASF.Requests.Mockup.Request'Class;
                       Field    : in String;
                       Filename : in String) is
      Token : constant String := Extract (Field, Filename);
   begin
      Request.Set_Parameter (Field, Token);
   end Set_CSRF;

   --  ------------------------------
   --  Cleanup the test instance.
   --  ------------------------------
   overriding
   procedure Tear_Down (T : in out EL_Test) is
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Contexts.Default.Default_Context'Class,
                                        EL.Contexts.Default.Default_Context_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Variables.Variable_Mapper'Class,
                                        EL.Variables.Variable_Mapper_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Contexts.Default.Default_ELResolver'Class,
                                        EL.Contexts.Default.Default_ELResolver_Access);
   begin
      ASF.Contexts.Faces.Restore (null);
      Free (T.ELContext);
      Free (T.Variables);
      Free (T.Root_Resolver);
   end Tear_Down;

   --  ------------------------------
   --  Setup the test instance.
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out EL_Test) is
   begin
      T.ELContext     := new EL.Contexts.Default.Default_Context;
      T.Root_Resolver := new EL.Contexts.Default.Default_ELResolver;
      T.Variables     := new EL.Variables.Default.Default_Variable_Mapper;
      T.ELContext.Set_Resolver (T.Root_Resolver.all'Access);
      T.ELContext.Set_Variable_Mapper (T.Variables.all'Access);
   end Set_Up;

end ASF.Tests;
