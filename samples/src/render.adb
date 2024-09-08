-----------------------------------------------------------------------
--  render -- XHTML Rendering example
--  Copyright (C) 2010, 2017, 2020, 2021, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with GNAT.Command_Line;

with ASF.Applications.Main;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Servlets;
with ASF.Servlets.Faces;
with ASF.Server;

with EL.Objects;
--  This example reads an XHTML file and renders the result.
procedure Render is

   use GNAT.Command_Line;
   use Ada.Strings.Fixed;

   use ASF;

   use EL.Objects;

   Factory   : ASF.Applications.Main.Application_Factory;
   Faces     : aliased ASF.Servlets.Faces.Faces_Servlet;
   App       : aliased Applications.Main.Application;
   Conf      : Applications.Config;
   Container : ASF.Server.Container;
begin
   Conf.Set ("view.ignore_white_spaces", "false");
   Conf.Set ("view.escape_unknown_tags", "false");
   Conf.Set ("view.ignore_empty_lines", "true");
   Conf.Set ("view.dir", "./");
   Conf.Set ("view.file_ext", "");
   App.Initialize (Conf, Factory);

   App.Add_Servlet (Name => "file", Server => Faces'Unchecked_Access);
   App.Add_Mapping ("*.xhtml", "file");
   Container.Register_Application ("/render", App'Unchecked_Access);
   loop
      case Getopt ("D:") is
         when 'D' =>
            declare
               Value : constant String := Parameter;
               Pos   : constant Natural := Index (Value, "=");
            begin
               if Pos > 0 then
                  App.Set_Global (Name  => Value (Value'First .. Pos - 1),
                                  Value => To_Object (Value (Pos + 1 .. Value'Last)));
               else
                  App.Set_Global (Name  => Value (Value'First .. Pos - 1),
                                  Value => To_Object (True));
               end if;
            end;

         when others =>
            exit;
      end case;
   end loop;

   declare
      View_Name : constant String := Get_Argument;
      Pos       : constant Natural := Index (View_Name, ".");
      Req       : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
      Content   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if View_Name = "" or else Pos = 0 then
         Ada.Text_IO.Put_Line ("Usage: render [-DNAME=VALUE ] file");
         Ada.Text_IO.Put_Line ("Example: render -DcontextPath=/test samples/web/ajax.xhtml");
         return;
      end if;

      Req.Set_Method ("GET");
      Req.Set_Request_URI ("/render/" & View_Name);
      App.Dispatch (View_Name, Req, Reply);

      Reply.Read_Content (Content);
      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Content));

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line ("Cannot read file '" & View_Name & "'");
   end;
end Render;
