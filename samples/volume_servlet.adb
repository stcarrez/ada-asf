-----------------------------------------------------------------------
--  volume_servlet -- Servlet example to compute some volumes
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

with Ada.Numerics;
with ASF.Streams;
package body Volume_Servlet is

   --  ------------------------------
   --  Write the volume form page with an optional response message.
   --  ------------------------------
   procedure Write (Response : in out Responses.Response'Class;
                    Message  : in String;
                    Height   : in String;
                    Radius   : in String) is
      Output : ASF.Streams.Print_Stream := Response.Get_Output_Stream;
   begin
      Output.Write ("<html><head><title>Volume servlet example</title></head>"
                    & "<style></style>"
                    & "<body>"
                    & "<h1>Compute the volume of a cylinder</h1>");

      --  Display the response or some error.
      if Message /= "" then
         Output.Write ("<h2>" & Message & "</h2>");
      end if;

      --  Render the form.  If we have some existing Radius or Height
      --  use them to set the initial values.
      Output.Write ("<p>Enter the height and radius of the cylinder</p>"
                    & "<form method='post'>"
                    & "<table>"
                    & "<tr><td>Height</td>"
                    & "<td><input type='text' size='10' name='height'");
      if Height /= "" then
         Output.Write (" value='" & Height & "'");
      end if;
      Output.Write ("></input></td></tr>"
                    & "<tr><td>Radius</td>"
                    & "<td><input type='text' size='10' name='radius'");
      if Radius /= "" then
         Output.Write (" value='" & Radius & "'");
      end if;
      Output.Write ("></input></td></tr>"
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
   begin
      Write (Response, "", "", "");
   end Do_Get;

   --  ------------------------------
   --  Called by the servlet container when a POST request is received.
   --  Computes the cylinder volume and display the result page.
   --  ------------------------------
   procedure Do_Post (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class) is
      Height : constant String := Request.Get_Parameter ("height");
      Radius : constant String := Request.Get_Parameter ("radius");
   begin
      declare
         H : Float := Float'Value (Height);
         R : Float := Float'Value (Radius);
         V : Float := Ada.Numerics.Pi * R * R * H;
      begin
         Write (Response, "The cylinder volume is: " & Float'Image (V), Height, Radius);
      end;

   exception
      when others =>
         Write (Response, "Invalid height or radius.  Please, enter a number",
                Height, Radius);
   end Do_Post;

end Volume_Servlet;
