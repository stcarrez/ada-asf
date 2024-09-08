-----------------------------------------------------------------------
--  asf.servlets.ajax -- AJAX servlet
--  Copyright (C) 2011, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Applications.Main;
with ASF.Requests;
with ASF.Responses;
package ASF.Servlets.Ajax is

   --  ------------------------------
   --  Ajax Servlet
   --  ------------------------------
   --  The <b>Ajax_Servlet</b> defines a servlet used to process Ajax actions
   --  on ASF bean objects.
   --  URI have the following format:
   --
   --   /<bean-name>/<method-name>[/<param-name>/<value>]
   type Ajax_Servlet is new Servlet.Core.Servlet with private;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out Ajax_Servlet;
                         Context : in Servlet_Registry'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a GET request.
   --
   --  GET requests are not permitted.
   --
   --  Returns 403 error.
   overriding
   procedure Do_Get (Server   : in Ajax_Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class);

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
   overriding
   procedure Do_Post (Server   : in Ajax_Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

private

   type Ajax_Servlet is new Servlet.Core.Servlet with record
      App    : ASF.Applications.Main.Application_Access;
   end record;

end ASF.Servlets.Ajax;
