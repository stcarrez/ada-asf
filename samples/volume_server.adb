-----------------------------------------------------------------------
--  volume_server -- Example of server with a servlet
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
with Volume_Servlet;

procedure Volume_Server is
   Compute : aliased Volume_Servlet.Servlet;
   App     : aliased ASF.Servlets.Servlet_Registry;
   WS      : ASF.Server.Web.AWS_Container;
begin
   --  Register the servlets and filters
   App.Add_Servlet (Name => "compute", Server => Compute'Unchecked_Access);

   --  Define servlet mappings
   App.Add_Mapping (Name => "compute", Pattern => "*.html");

   WS.Register_Application ("/volume", App'Unchecked_Access);

   WS.Start;

   delay 60.0;

end Volume_Server;
