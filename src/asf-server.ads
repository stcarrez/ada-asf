-----------------------------------------------------------------------
--  asf.server -- ASF Server
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with Ada.Finalization;

with ASF.Requests;
with ASF.Responses;
with ASF.Servlets;
package ASF.Server is

   type Container is tagged limited private;

   --  Register the application to serve requests
   procedure Register_Application (Server  : in out Container;
                                   URI     : in String;
                                   Context : in ASF.Servlets.Servlet_Registry_Access);

   --  Receives standard HTTP requests from the public service method and dispatches
   --  them to the Do_XXX methods defined in this class. This method is an HTTP-specific
   --  version of the Servlet.service(Request, Response) method. There's no need
   --  to override this method.
   procedure Service (Server   : in Container;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   --  Get the current registry associated with the current request being processed
   --  by the current thread.  Returns null if there is no current request.
   function Current return ASF.Servlets.Servlet_Registry_Access;

   --  Give access to the current request and response object to the <b>Process</b>
   --  procedure.  If there is no current request for the thread, do nothing.
   procedure Update_Context (Process : not null access
                               procedure (Request  : in out Requests.Request'Class;
                                          Response : in out Responses.Response'Class));

private

   --  Binding to record the ASF applications and bind them to URI prefixes.
   --  It is expected that the number of ASF applications is small (1-10 per server).
   type Binding is record
      Context  : ASF.Servlets.Servlet_Registry_Access;
      Base_URI : access String;
   end record;

   type Binding_Array is array (Natural range <>) of Binding;
   type Binding_Array_Access is access all Binding_Array;

   type Container is new Ada.Finalization.Limited_Controlled with record
      Nb_Bindings  : Natural := 0;
      Applications : Binding_Array_Access := null;
      Default      : ASF.Servlets.Servlet_Registry;
   end record;

   type Request_Context is record
      Application : ASF.Servlets.Servlet_Registry_Access;
      Process     : access
        procedure (Process : not null access
                     procedure (Request  : in out Requests.Request'Class;
                                Response : in out Responses.Response'Class));
   end record;

   --  Set the current registry.  This is called by <b>Service</b> once the
   --  registry is identified from the URI.
   procedure Set_Context (Context : in Request_Context);

end ASF.Server;
