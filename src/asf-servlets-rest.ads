-----------------------------------------------------------------------
--  asf-servlets-rest -- REST servlet
--  Copyright (C) 2016 Stephane Carrez
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
with ASF.Rest;
with ASF.Routes.Servlets.Rest;
with ASF.Applications.Main;

package ASF.Servlets.Rest is

   --  The <b>Servlet</b> represents the component that will handle
   --  an HTTP request received by the server.
   type Rest_Servlet is new Servlet with private;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out Rest_Servlet;
                         Context : in Servlet_Registry'Class);

   --  Receives standard HTTP requests from the public service method and dispatches
   --  them to the Do_XXX methods defined in this class. This method is an HTTP-specific
   --  version of the Servlet.service(Request, Response) method. There's no need
   --  to override this method.
   overriding
   procedure Service (Server   : in Rest_Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   --  Create a route for the REST API.
   function Create_Route (Registry : in ASF.Servlets.Servlet_Registry;
                          Name     : in String)
                          return ASF.Routes.Servlets.Rest.API_Route_Type_Access;

   procedure Dispatch (Server   : in Rest_Servlet;
                       Method   : in ASF.Rest.Method_Type;
                       Request  : in out Requests.Request'Class;
                       Response : in out Responses.Response'Class);

private

   type Rest_Servlet is new Servlet with record
      App    : ASF.Applications.Main.Application_Access;
   end record;

end ASF.Servlets.Rest;
