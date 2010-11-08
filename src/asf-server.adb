-----------------------------------------------------------------------
--  asf.server -- ASF Server
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with ASF.Servlets;
with Util.Strings;

package body ASF.Server is

   --  ------------------------------
   --  Register the application to serve requests
   --  ------------------------------
   procedure Register_Application (Server  : in out Container;
                                   URI     : in String;
                                   Context : in ASF.Servlets.Servlet_Registry_Access) is
      Count : constant Natural := Server.Nb_Bindings;
      Apps  : constant Binding_Array_Access := new Binding_Array (1 .. Count + 1);
   begin
      if Server.Applications /= null then
         Apps (1 .. Count) := Server.Applications (1 .. Count);
      end if;
      Server.Nb_Bindings := Count + 1;
      Apps (Apps'Last).Context  := Context;
      Apps (Apps'Last).Base_URI := new String '(URI);
      Server.Applications := Apps;
   end Register_Application;

   --  ------------------------------
   --  Receives standard HTTP requests from the public service method and dispatches
   --  them to the Do_XXX methods defined in this class. This method is an HTTP-specific
   --  version of the Servlet.service(Request, Response) method. There's no need
   --  to override this method.
   --  ------------------------------
   procedure Service (Server   : in Container;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class) is

      use Servlets;
      use Util.Strings;

      URI        : constant String := Request.Get_Request_URI;
      Slash_Pos  : constant Natural := Index (URI, '/', URI'First + 1);
      Apps       : constant Binding_Array_Access := Server.Applications;
      Prefix_End : Natural;
   begin
      --  Find the module and action to invoke
      if Slash_Pos > 1 then
         Prefix_End := Slash_Pos - 1;
      else
         Prefix_End := URI'Last;
      end if;

      for I in Apps.all'Range loop
         if Apps (I).Base_URI.all = URI (URI'First .. Prefix_End) then
            declare
               Context    : constant Servlet_Registry_Access := Apps (I).Context;
               Page       : constant String := URI (Prefix_End + 1 .. URI'Last);
               Dispatcher : constant Request_Dispatcher := Context.Get_Request_Dispatcher (Page);
            begin
               Forward (Dispatcher, Request, Response);
               case Response.Get_Status / 100 is
                  when 2 | 3 =>
                     null;

                  when others =>
                     Context.Send_Error_Page (Request, Response);

               end case;
               return;

            exception
               when E : others =>
                  Context.Error (Request, Response, E);
                  return;
            end;
         end if;
      end loop;

      Response.Set_Status (Responses.SC_NOT_FOUND);
      Server.Default.Send_Error_Page (Request, Response);

   exception
      when E : others =>
         Server.Default.Error (Request, Response, E);
   end Service;

end ASF.Server;
