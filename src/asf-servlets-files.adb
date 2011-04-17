-----------------------------------------------------------------------
--  asf.servlets.files -- Static file servlet
--  Copyright (C) 2010, 2011 Stephane Carrez
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

with Util.Files;
with Util.Strings;
with Util.Streams;
with Util.Streams.Files;
with Ada.Streams;
with Ada.Streams.Stream_IO;

with Ada.Directories;

with ASF.Streams;
package body ASF.Servlets.Files is

   use Ada.Streams;
   use Ada.Streams.Stream_IO;
   use Ada.Directories;

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   procedure Initialize (Server  : in out File_Servlet;
                         Context : in Servlet_Registry'Class) is
      Dir      : constant String := Context.Get_Init_Parameter ("web.dir");
      Def_Type : constant String := Context.Get_Init_Parameter ("content-type.default");
   begin
      Server.Dir := new String '(Dir);
      Server.Default_Content_Type := new String '(Def_Type);
   end Initialize;

   --  ------------------------------
   --  Returns the time the Request object was last modified, in milliseconds since
   --  midnight January 1, 1970 GMT.  If the time is unknown, this method returns
   --  a negative number (the default).
   --
   --  Servlets that support HTTP GET requests and can quickly determine their
   --  last modification time should override this method. This makes browser and
   --  proxy caches work more effectively, reducing the load on server and network
   --  resources.
   --  ------------------------------
   function Get_Last_Modified (Server  : in File_Servlet;
                               Request : in Requests.Request'Class)
                               return Ada.Calendar.Time is
      pragma Unreferenced (Server, Request);
   begin
      return Ada.Calendar.Clock;
   end Get_Last_Modified;

   --  ------------------------------
   --  Set the content type associated with the given file
   --  ------------------------------
   procedure Set_Content_Type (Server   : in File_Servlet;
                               Path     : in String;
                               Response : in out Responses.Response'Class) is
      Pos : constant Natural := Util.Strings.Rindex (Path, '.');
   begin
      if Pos = 0 then
         Response.Set_Content_Type (Server.Default_Content_Type.all);
         return;
      end if;
      if Path (Pos .. Path'Last) = ".css" then
         Response.Set_Content_Type ("text/css");
         return;
      end if;
      if Path (Pos .. Path'Last) = ".js" then
         Response.Set_Content_Type ("text/javascript");
         return;
      end if;
      if Path (Pos .. Path'Last) = ".html" then
         Response.Set_Content_Type ("text/html");
         return;
      end if;
      if Path (Pos .. Path'Last) = ".txt" then
         Response.Set_Content_Type ("text/plain");
         return;
      end if;
      if Path (Pos .. Path'Last) = ".png" then
         Response.Set_Content_Type ("image/png");
         return;
      end if;
      if Path (Pos .. Path'Last) = ".jpg" then
         Response.Set_Content_Type ("image/jpg");
         return;
      end if;
   end Set_Content_Type;

   --  ------------------------------
   --  Called by the server (via the service method) to allow a servlet to handle
   --  a GET request.
   --
   --  Overriding this method to support a GET request also automatically supports
   --  an HTTP HEAD request. A HEAD request is a GET request that returns no body
   --  in the response, only the request header fields.
   --
   --  When overriding this method, read the request data, write the response headers,
   --  get the response's writer or output stream object, and finally, write the
   --  response data. It's best to include content type and encoding.
   --  When using a PrintWriter object to return the response, set the content type
   --  before accessing the PrintWriter object.
   --
   --  The servlet container must write the headers before committing the response,
   --  because in HTTP the headers must be sent before the response body.
   --
   --  Where possible, set the Content-Length header (with the
   --  Response.Set_Content_Length method), to allow the servlet container
   --  to use a persistent connection to return its response to the client,
   --  improving performance. The content length is automatically set if the entire
   --  response fits inside the response buffer.
   --
   --  When using HTTP 1.1 chunked encoding (which means that the response has a
   --  Transfer-Encoding header), do not set the Content-Length header.
   --
   --  The GET method should be safe, that is, without any side effects for which
   --  users are held responsible. For example, most form queries have no side effects.
   --  If a client request is intended to change stored data, the request should use
   --  some other HTTP method.
   --
   --  The GET method should also be idempotent, meaning that it can be safely repeated.
   --  Sometimes making a method safe also makes it idempotent. For example, repeating
   --  queries is both safe and idempotent, but buying a product online or modifying
   --  data is neither safe nor idempotent.
   --
   --  If the request is incorrectly formatted, Do_Get  returns an HTTP "Bad Request"
   --  ------------------------------
   procedure Do_Get (Server   : in File_Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class) is

      use Util.Files;

      URI  : constant String := Request.Get_Path_Info;
      Path : constant String := Find_File_Path (Name => URI, Paths => Server.Dir.all);
   begin
      if not Ada.Directories.Exists (Path)
        or else Ada.Directories.Kind (Path) /= Ada.Directories.Ordinary_File then
         Response.Send_Error (Responses.SC_NOT_FOUND);
         return;
      end if;

      File_Servlet'Class (Server).Set_Content_Type (Path, Response);
      declare
         Output : ASF.Streams.Print_Stream := Response.Get_Output_Stream;
         Input  : Util.Streams.Files.File_Stream;
      begin
         Input.Open (Name => Path, Mode => In_File);
         Util.Streams.Copy (From => Input, Into => Output);
      end;
   end Do_Get;

end ASF.Servlets.Files;
