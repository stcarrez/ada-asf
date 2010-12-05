-----------------------------------------------------------------------
--  render -- XHTML Rendering example
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
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with GNAT.Command_Line;

with ASF.Applications.Main;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;

with EL.Objects;
--  This example reads an XHTML file and renders the result.
procedure Render is

   use GNAT.Command_Line;
   use Ada.Strings.Fixed;

   use ASF;

   use EL.Objects;

   Factory  : ASF.Applications.Main.Application_Factory;
   App      : Applications.Main.Application;
   Conf     : Applications.Config;
begin
   Conf.Set ("view.ignore_white_spaces", "false");
   Conf.Set ("view.escape_unknown_tags", "false");
   Conf.Set ("view.ignore_empty_lines", "true");

   App.Initialize (Conf, Factory);
   loop
      case Getopt ("D:") is
         when 'D' =>
            declare
               Value : constant String := Parameter;
               Pos   : constant Natural := Index (Value, "=");
            begin
               if Pos > 0 then
                  App.Set_Global (Name  => Value (1 .. Pos - 1),
                                  Value => To_Object (Value (Pos + 1 .. Value'Last)));
               else
                  App.Set_Global (Name  => Value (1 .. Pos - 1),
                                  Value => To_Object(True));
               end if;
            end;

         when others =>
            exit;
      end case;
   end loop;

   declare
      View_Name : constant String := Get_Argument;
      Req       : ASF.Requests.Mockup.Request;
      Reply     : ASF.Responses.Mockup.Response;
      Content   : Ada.Strings.Unbounded.Unbounded_String;
   begin

      App.Dispatch (Page     => View_Name,
                    Request  => Req,
                    Response => Reply);

      Reply.Read_Content (Content);
      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Content));

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line ("Cannot read file '" & View_Name & "'");
   end;
end Render;
