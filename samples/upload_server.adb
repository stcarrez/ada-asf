-----------------------------------------------------------------------
--  upload_server -- Example of server with a servlet
--  Copyright (C) 2012 Stephane Carrez
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
with Upload_Servlet;
with Util.Log.Loggers;

procedure Upload_Server is
   Upload  : aliased Upload_Servlet.Servlet;
   App     : aliased ASF.Servlets.Servlet_Registry;
   WS      : ASF.Server.Web.AWS_Container;
   Log     : Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Upload_Server");
begin
   --  Register the servlets and filters
   App.Add_Servlet (Name => "upload", Server => Upload'Unchecked_Access);

   --  Define servlet mappings
   App.Add_Mapping (Name => "upload", Pattern => "*.html");

   WS.Register_Application ("/upload", App'Unchecked_Access);

   Log.Info ("Connect you browser to: http://localhost:8080/upload/upload.html");

   WS.Start;

   delay 6000.0;

end Upload_Server;
