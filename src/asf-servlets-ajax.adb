-----------------------------------------------------------------------
--  asf.servlets.ajax -- AJAX servlet
--  Copyright (C) 2011, 2022 Stephane Carrez
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

with Util.Strings;

with EL.Expressions;
with EL.Variables;

--
--  URI format
--
--   /<bean-name>/<method-name>[/<param-name>/<value>]
--
--
package body ASF.Servlets.Ajax is

   --  ------------------------------
   --  Ajax Servlet

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   overriding
   procedure Initialize (Server  : in out Ajax_Servlet;
                         Context : in Servlet_Registry'Class) is

      pragma Unreferenced (Context);

      Ctx : constant Servlet_Registry_Access := Server.Get_Servlet_Context;
   begin
      if Ctx.all in ASF.Applications.Main.Application'Class then
         Server.App := ASF.Applications.Main.Application'Class (Ctx.all)'Unchecked_Access;
      end if;
   end Initialize;

   --  ------------------------------
   --  Called by the server (via the service method) to allow a servlet to handle
   --  a GET request.
   --
   --  GET requests are not permitted.
   --
   --  Returns 403 error.
   --  ------------------------------
   overriding
   procedure Do_Get (Server   : in Ajax_Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server, Request);
   begin
      Response.Send_Error (Error   => Responses.SC_FORBIDDEN,
                           Message => "not allowed");
   end Do_Get;

   --  ------------------------------
   --  Called by the server (via the service method) to allow a servlet to handle
   --  a POST request. The HTTP POST method allows the client to send data of unlimited
   --  length to the Web server a single time and is useful when posting information
   --  such as credit card numbers.
   --
   --  1. Split the URI into a bean name, an action method name and a list of parameters.
   --  2. Find the bean object in the application (create it if necessary).
   --  3. For each parameter, invoke the <b>Set_Value</b> bean method.
   --  4. Invoke the bean action method (specified in the second URI component).
   --  5. Navigate to the result view according to the action outcome result.
   --  6. Execute the navigation (render page, redirect, ...
   --
   --  If an error is found, return SC_NOT_FOUND if an object or method cannot be found.
   --  ------------------------------
   overriding
   procedure Do_Post (Server   : in Ajax_Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class) is
      URI  : constant String := Request.Get_Path_Info;
      Pos  : constant Natural := Util.Strings.Index (URI, '/', URI'First + 1);
      Last : Natural;
   begin
      if Pos <= URI'First + 1 or else Pos = URI'Last or else URI (URI'First) /= '/' then
         Response.Send_Error (ASF.Responses.SC_NOT_FOUND, "Invalid URI format");
         return;
      end if;
      Last := Util.Strings.Index (URI, '/', Pos + 1);
      if Last = 0 then
         Last := URI'Last + 1;
      end if;
      Server.App.Dispatch (Name      => URI (URI'First + 1 .. Pos - 1),
                           Operation => URI (Pos + 1 .. Last - 1),
                           Request   => Request,
                           Response  => Response,
                           Prepare  => null);

   exception
      when EL.Expressions.Invalid_Method =>
         Response.Send_Error (ASF.Responses.SC_NOT_FOUND, "Invalid request");
         return;

      when EL.Variables.No_Variable | EL.Expressions.Invalid_Variable =>
         Response.Send_Error (ASF.Responses.SC_NOT_FOUND, "Invalid request");
         return;

         --
--        when others =>
--           Response.Send_Error (ASF.Responses.SC_INTERNAL_SERVER_ERROR, "Server error");
--           return;
   end Do_Post;

end ASF.Servlets.Ajax;
