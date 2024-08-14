-----------------------------------------------------------------------
--  asf_volume_server -- The volume_server application with Ada Server Faces
--  Copyright (C) 2010, 2011, 2012, 2015 Stephane Carrez
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

with Ada.IO_Exceptions;
with ASF.Server.Web;
with ASF.Servlets;
with ASF.Servlets.Faces;
with Servlet.Core.Files;
with Servlet.Core.Measures;
with ASF.Filters.Dump;
with ASF.Beans;
with ASF.Applications;
with ASF.Applications.Main;
with ASF.Applications.Main.Configs;
with Util.Beans.Objects;
with Util.Log.Loggers;

with Countries;
with Volume;
with Messages;
procedure Asf_Volume_Server is

   CONTEXT_PATH : constant String := "/volume";
   CONFIG_PATH  : constant String := "samples.properties";

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Openid");

   Factory : ASF.Applications.Main.Application_Factory;
   App     : aliased ASF.Applications.Main.Application;
   Faces   : aliased ASF.Servlets.Faces.Faces_Servlet;
   Files   : aliased Servlet.Core.Files.File_Servlet;
   Perf    : aliased Servlet.Core.Measures.Measure_Servlet;
   Dump    : aliased ASF.Filters.Dump.Dump_Filter;
   Bean    : aliased Volume.Compute_Bean;
   Conv    : aliased Volume.Float_Converter;
   WS      : ASF.Server.Web.AWS_Container;
   C       : ASF.Applications.Config;
   None    : ASF.Beans.Parameter_Bean_Ref.Ref;
begin
   C.Set (ASF.Applications.VIEW_EXT, ".html");
   C.Set (ASF.Applications.VIEW_DIR, "samples/web");
   C.Set ("web.dir", "samples/web");
   begin
      C.Load_Properties (CONFIG_PATH);
      Util.Log.Loggers.Initialize (CONFIG_PATH);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Log.Error ("Cannot read application configuration file {0}", CONFIG_PATH);

   end;
   App.Initialize (C, Factory);
   App.Set_Global ("contextPath", CONTEXT_PATH);
   App.Set_Global ("compute",
                   Util.Beans.Objects.To_Object (Bean'Unchecked_Access,
                                                 Util.Beans.Objects.STATIC));

   App.Set_Global ("countries", Util.Beans.Objects.To_Object (Countries.Create_Country_List));

   --  Declare a global bean to identify this sample from within the XHTML files.
   App.Set_Global ("sampleName", "volume");

   --  Register the servlets and filters
   App.Add_Servlet (Name => "faces", Server => Faces'Unchecked_Access);
   App.Add_Servlet (Name => "files", Server => Files'Unchecked_Access);
   App.Add_Filter (Name => "dump", Filter => Dump'Unchecked_Access);
   App.Add_Servlet (Name => "perf", Server => Perf'Unchecked_Access);
   App.Add_Filter (Name => "perf", Filter => Perf'Unchecked_Access);

   --  Define servlet mappings
   App.Add_Mapping (Name => "faces", Pattern => "*.html");
   App.Add_Mapping (Name => "files", Pattern => "*.css");
   App.Add_Mapping (Name => "files", Pattern => "*.js");
   App.Add_Mapping (Name => "files", Pattern => "*.png");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "*.html");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "*.js");

   App.Add_Converter (Name => "float", Converter => Conv'Unchecked_Access);

   App.Register_Class (Name => "Message_Bean", Handler => Messages.Create_Message_Bean'Access);
   App.Register_Class (Name => "Message_List", Handler => Messages.Create_Message_List'Access);
   App.Register (Name => "message", Class => "Message_Bean", Params => None);
   App.Register (Name => "messages", Class => "Message_List", Params => None);

   ASF.Applications.Main.Configs.Read_Configuration (App, "samples/web/WEB-INF/web.xml");
   WS.Register_Application (CONTEXT_PATH, App'Unchecked_Access);

   Log.Info ("Connect you browser to: http://localhost:8080/volume/compute.html");
   WS.Start;

   delay 6000.0;

end Asf_Volume_Server;
