-----------------------------------------------------------------------
--  upload_servlet -- Servlet example to upload files on the server
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

with ASF.Servlets;
with ASF.Requests;
with ASF.Responses;
with ASF.Streams;

package Upload_Servlet is

   use ASF;

   type File_Type is (IMAGE, PDF, TAR_GZ, TAR, ZIP, UNKNOWN);

   --  Guess a file type depending on a content type or a file name.
   function Get_File_Type (Content_Type : in String;
                           Name         : in String) return File_Type;

   --  Execute a command and write the result to the output stream.
   procedure Execute (Command : in String;
                      Output  : in out ASF.Streams.Print_Stream);

   --  The <b>Servlet</b> represents the component that will handle
   --  an HTTP request received by the server.
   type Servlet is new ASF.Servlets.Servlet with null record;

   --  Called by the servlet container when a GET request is received.
   --  Display the upload form page.
   procedure Do_Get (Server   : in Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class);

   --  Called by the servlet container when a POST request is received.
   --  Receives the uploaded files and identify them using some external command.
   procedure Do_Post (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

private

   --  Write the upload form page with an optional response message.
   procedure Write (Response : in out Responses.Response'Class;
                    Message  : in String);

end Upload_Servlet;
