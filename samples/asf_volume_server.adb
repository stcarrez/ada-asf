-----------------------------------------------------------------------
--  asf_volume_server -- The volume_server application with Ada Server Faces
--  Copyright (C) 2010 Stephane Carrez
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

with ASF.Server.Web;
with ASF.Servlets;
with ASF.Servlets.Faces;
with ASF.Servlets.Files;
with ASF.Filters.Dump;
with ASF.Applications;
with ASF.Applications.Main;
with EL.Objects;

with Volume;
procedure Asf_Volume_Server is

   CONTEXT_PATH : constant String := "/volume";

   Factory : ASF.Applications.Main.Application_Factory;
   App     : aliased ASF.Applications.Main.Application;
   Faces   : aliased ASF.Servlets.Faces.Faces_Servlet;
   Files   : aliased ASF.Servlets.Files.File_Servlet;
   Dump    : aliased ASF.Filters.Dump.Dump_Filter;
   Bean    : aliased Volume.Compute_Bean;
   Conv    : constant access Volume.Float_Converter := new Volume.Float_Converter;
   WS      : ASF.Server.Web.AWS_Container;
   C       : ASF.Applications.Config;
begin
   Bean.Radius := 1.2;
   Bean.Height := 2.0;
   C.Set (ASF.Applications.VIEW_EXT, ".html");
   C.Set (ASF.Applications.VIEW_DIR, "samples/web");
   C.Set ("web.dir", "samples/web");

   App.Initialize (C, Factory);
   App.Set_Global ("contextPath", CONTEXT_PATH);
   App.Set_Global ("compute", EL.Objects.To_Object (Bean'Unchecked_Access));

   --  Register the servlets and filters
   App.Add_Servlet (Name => "compute", Server => Faces'Unchecked_Access);
   App.Add_Servlet (Name => "files", Server => Files'Unchecked_Access);
   App.Add_Filter (Name => "dump", Filter => Dump'Unchecked_Access);

   --  Define servlet mappings
   App.Add_Mapping (Name => "compute", Pattern => "*.html");
   App.Add_Mapping (Name => "files", Pattern => "*.css");
   App.Add_Filter_Mapping (Name => "dump", Pattern => "*.html");

   App.Add_Converter (Name => "float", Converter => Conv.all'Unchecked_Access);

   WS.Register_Application (CONTEXT_PATH, App'Unchecked_Access);

   WS.Start;

   delay 600.0;

end Asf_Volume_Server;
