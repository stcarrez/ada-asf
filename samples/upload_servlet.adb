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

with Ada.Streams;

with Util.Streams.Pipes;
with Util.Strings;

with ASF.Parts;
package body Upload_Servlet is

   --  ------------------------------
   --  Write the upload form page with an optional response message.
   --  ------------------------------
   procedure Write (Response : in out Responses.Response'Class;
                    Message  : in String) is
      Output : ASF.Streams.Print_Stream := Response.Get_Output_Stream;
   begin
      Output.Write ("<html><head><title>Upload servlet example</title></head>"
                    & "<style></style>"
                    & "<body>"
                    & "<h1>Upload files to identify them</h1>");

      --  Display the response or some error.
      if Message /= "" then
         Output.Write ("<h2>" & Message & "</h2>");
      end if;

      --  Render the form.  If we have some existing Radius or Height
      --  use them to set the initial values.
      Output.Write ("<p>Enter the height and radius of the cylinder</p>"
                    & "<form method='post' enctype='multipart/form-data'>"
                    & "<table>"
                    & "<tr><td>File 1</td>"
                    & "<td><input type='file' size='50' name='file1' maxlength='100000'/></td>"
                    & "</tr><tr><td>File 2</td>"
                    & "<td><input type='file' size='50' name='file2' maxlength='100000'/></td>"
                    & "</tr><tr><td>File 3</td>"
                    & "<td><input type='file' size='50' name='file3' maxlength='100000'/></td>"
                    & "</tr>"
                    & "<tr><td></td><td><input type='submit' value='Compute'></input></td></tr>"
                    & "</table></form>"
                    & "</body></html>");
      Response.Set_Status (Responses.SC_OK);
   end Write;

   --  ------------------------------
   --  Called by the servlet container when a GET request is received.
   --  Display the volume form page.
   --  ------------------------------
   procedure Do_Get (Server   : in Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server, Request);
   begin
      Write (Response, "");
   end Do_Get;

   --  ------------------------------
   --  Execute a command and write the result to the output stream.
   --  ------------------------------
   procedure Execute (Command : in String;
                      Output  : in out ASF.Streams.Print_Stream) is
      use type Ada.Streams.Stream_Element_Offset;

      Proc     : Util.Streams.Pipes.Pipe_Stream;
      Content  : Ada.Streams.Stream_Element_Array (0 .. 1024);
      Pos      : Ada.Streams.Stream_Element_Offset;
   begin
      Proc.Open (Command);
      loop
         Proc.Read (Into => Content,
                    Last => Pos);
         exit when Pos < 0;
         Output.Write (Content (0 .. Pos));
      end loop;
      Proc.Close;
      if Proc.Get_Exit_Status /= 0 then
         Output.Write ("Command '" & Command & "' exited with code "
                       & Integer'Image (Proc.Get_Exit_Status));
      end if;
   end Execute;

   --  ------------------------------
   --  Guess a file type depending on a content type or a file name.
   --  ------------------------------
   function Get_File_Type (Content_Type : in String;
                           Name         : in String) return File_Type is
   begin
      if Content_Type = "application/pdf" then
         return PDF;
      elsif Content_Type = "image/png" or Content_Type = "image/jpeg" then
         return IMAGE;
      elsif Content_Type = "application/zip" then
         return ZIP;
      end if;
      declare
         Ext_Pos      : constant Natural := Util.Strings.Rindex (Name, '.');
      begin
         if Ext_Pos > 0 then
            if Name (Ext_Pos .. Name'Last) = ".gz" or Name (Ext_Pos .. Name'Last) = ".tgz" then
               return TAR_GZ;

            elsif Name (Ext_Pos .. Name'Last) = ".tar" then
               return TAR;

            elsif Name (Ext_Pos .. Name'Last) = ".jpg"
              or Name (Ext_Pos .. Name'Last) = ".gif"
              or Name (Ext_Pos .. Name'Last) = ".xbm"
              or Name (Ext_Pos .. Name'Last) = ".png" then
               return IMAGE;

            elsif Name (Ext_Pos .. Name'Last) = ".zip" then
               return ZIP;

            elsif Name (Ext_Pos .. Name'Last) = ".pdf" then
               return PDF;

            end if;
         end if;
         return UNKNOWN;
      end;
   end Get_File_Type;

   --  ------------------------------
   --  Called by the servlet container when a POST request is received.
   --  Receives the uploaded files and identify them using some external command.
   --  ------------------------------
   procedure Do_Post (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server);

      procedure Process_Part (Part : in ASF.Parts.Part'Class);

      Output : ASF.Streams.Print_Stream := Response.Get_Output_Stream;

      procedure Process_Part (Part : in ASF.Parts.Part'Class) is
         Name         : constant String := Part.Get_Name;
         Content_Type : constant String := Part.Get_Content_Type;
         Path         : constant String := Part.Get_Local_Filename;
         Kind         : constant File_Type := Get_File_Type (Content_Type, Name);
      begin
         Output.Write ("<tr><td>Name: " & Name);
         Output.Write ("</td><td>Content_Type: " & Content_Type & "</td>");
         Output.Write ("<td>Path: " & Path & "</td>");
         Output.Write ("<td>Length: " & Natural'Image (Part.Get_Size) & "</td>");
         Output.Write ("</tr><tr><td></td><td colspan='3'><pre>");

         case Kind is
            when TAR_GZ =>
               Execute ("tar tvzf " & Path, Output);

            when TAR =>
               Execute ("tar tvf " & Path, Output);

            when IMAGE =>

               Execute ("identify " & Path, Output);

            when ZIP =>
               Execute ("zipinfo " & Path, Output);

            when PDF =>
               Execute ("pdfinfo " & Path, Output);

            when others =>
               Output.Write ("Unknown file type");

         end case;
         Output.Write ("</pre></td></tr>");
      end Process_Part;

   begin
      Response.Set_Content_Type ("text/html");
      Output.Write ("<html><body>");
      Output.Write ("<table style='width: 100%;'>");
      for I in 1 .. Request.Get_Part_Count loop
         Request.Process_Part (I, Process_Part'Access);
      end loop;
      Output.Write ("</table>");
      Output.Write ("</body></html>");
   end Do_Post;

end Upload_Servlet;
