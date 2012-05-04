-----------------------------------------------------------------------
--  openid -- Example of OpenID 2.0 Authentication
--  Copyright (C) 2011, 2012 Stephane Carrez
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
with Ada.Exceptions;
with Ada.IO_Exceptions;

with ASF.Server.Web;
with ASF.Applications;
with ASF.Applications.Main;
with ASF.Servlets.Faces;
with ASF.Servlets.Files;
with ASF.Servlets.Measures;
with ASF.Filters.Dump;

with Util.Beans.Objects;
with Util.Log.Loggers;

with Users;
with AWS.Net.SSL;
with Security.Openid;
with Security.Openid.Servlets;
procedure Openid is

   use ASF.Applications;

   Log          : Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Openid");

   CONTEXT_PATH : constant String := "/openid";
   CONFIG_PATH  : constant String := "samples.properties";

   User         : aliased Users.User_Info;
   App          : aliased ASF.Applications.Main.Application;
   Factory      : ASF.Applications.Main.Application_Factory;
   C            : ASF.Applications.Config;

   --  Application servlets.
   Faces        : aliased ASF.Servlets.Faces.Faces_Servlet;
   Files        : aliased ASF.Servlets.Files.File_Servlet;
   Perf         : aliased ASF.Servlets.Measures.Measure_Servlet;
   Auth         : aliased Security.Openid.Servlets.Request_Auth_Servlet;
   Verify_Auth  : aliased Security.Openid.Servlets.Verify_Auth_Servlet;

   --  Debug filters.
   Dump         : aliased ASF.Filters.Dump.Dump_Filter;

   --  Web application server
   WS           : ASF.Server.Web.AWS_Container;
begin
   if not AWS.Net.SSL.Is_Supported then
      Log.Error ("SSL is not supported by AWS.");
      Log.Error ("SSL is required for the OpenID connector to connect to OpenID providers.");
      Log.Error ("Please, rebuild AWS with SSL support.");
      return;
   end if;
   begin
      C.Load_Properties (CONFIG_PATH);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Log.Error ("Cannot read application configuration file {0}", CONFIG_PATH);

   end;
   App.Initialize (C, Factory);
   App.Register ("samplesMsg", "samples");
   App.Set_Global ("contextPath", "/openid");

   --  Declare a global bean to identify this sample from within the XHTML files.
   App.Set_Global ("sampleName", "openid");

   App.Set_Global ("version", "0.1");
   App.Set_Global ("user", Util.Beans.Objects.To_Object (User'Unchecked_Access,
     Util.Beans.Objects.STATIC));

   --  Register the servlets and filters
   App.Add_Servlet (Name => "faces", Server => Faces'Unchecked_Access);
   App.Add_Servlet (Name => "files", Server => Files'Unchecked_Access);
   App.Add_Servlet (Name => "auth", Server => Auth'Unchecked_Access);
   App.Add_Servlet (Name => "verify-auth", Server => Verify_Auth'Unchecked_Access);
   App.Add_Servlet (Name => "perf", Server => Perf'Unchecked_Access);

   --  Register the filters
   App.Add_Filter (Name => "perf", Filter => Perf'Unchecked_Access);
   App.Add_Filter (Name => "dump", Filter => Dump'Unchecked_Access);

   --  Define servlet mappings
   App.Add_Mapping (Name => "faces", Pattern => "*.html");
   App.Add_Mapping (Name => "files", Pattern => "*.css");
   App.Add_Mapping (Name => "files", Pattern => "*.js");
   App.Add_Mapping (Name => "files", Pattern => "*.png");
   App.Add_Mapping (Name => "files", Pattern => "*.jpg");
   App.Add_Mapping (Name => "verify-auth", Pattern => "/auth/verify");
   App.Add_Mapping (Name => "auth", Pattern => "/auth/auth/*");
   App.Add_Mapping (Name => "perf", Pattern => "/statistics.xml");

   --  Install the debug filter.
   App.Add_Filter_Mapping (Name => "perf", Pattern => "*.html");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "*.html");

   App.Add_Filter_Mapping (Name => "perf", Pattern => "*.css");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "*.css");

   App.Add_Filter_Mapping (Name => "perf", Pattern => "*.png");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "*.png");

   App.Add_Filter_Mapping (Name => "perf", Pattern => "*.jpg");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "*.jpg");

   App.Add_Filter_Mapping (Name => "perf", Pattern => "/auth/verify");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "/auth/verify");

   App.Add_Filter_Mapping (Name => "perf", Pattern => "/auth/auth/*");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "/auth/auth/*");

   WS.Register_Application (CONTEXT_PATH, App'Unchecked_Access);

   Log.Info ("Connect you browser to: http://localhost:8080/openid/auth/login.html");
   WS.Start;

   delay 600.0;

   App.Close;
exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Exception in server: " &
                            Ada.Exceptions.Exception_Name (E) & ": " &
                            Ada.Exceptions.Exception_Message (E));
end Openid;
