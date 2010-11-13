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
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with GNAT.Command_Line;

with ASF.Applications.Views;
with ASF.Components.Core;
with ASF.Contexts.Faces;
with ASF.Contexts.Writer.String;

with EL.Objects;
with EL.Contexts;
with EL.Contexts.Default;
with EL.Variables;
with EL.Variables.Default;
with ASF.Streams;
--  This example reads an XHTML file and renders the result.
procedure Render is

   use GNAT.Command_Line;
   use Ada.Strings.Fixed;

   use ASF;
   use ASF.Contexts.Faces;

   use EL.Contexts.Default;
   use EL.Variables;
   use EL.Variables.Default;
   use EL.Contexts;
   use EL.Objects;

   H        : Applications.Views.View_Handler;
   Writer   : aliased Contexts.Writer.String.String_Writer;
   Context  : aliased Faces_Context;
   View     : Components.Core.UIViewRoot;
   ELContext : aliased EL.Contexts.Default.Default_Context;
   Variables : aliased Default_Variable_Mapper;
   Resolver  : aliased Default_ELResolver;
   Conf      : Applications.Config;
   Output    : ASF.Streams.Print_Stream;
begin
   loop
      case Getopt ("D:") is
         when 'D' =>
            declare
               Value : constant String := Parameter;
               Pos   : constant Natural := Index (Value, "=");
            begin
--                 if Pos > 0 then
--                    Variables.Set_Variable (Value (1 .. Pos - 1),
--                                            To_Object (Value (Pos + 1 .. Value'Last)));
--                 else
--                    Variables.Set_Variable (Value, To_Object(True));
--                 end if;
               null;
            end;

         when others =>
            exit;
      end case;
   end loop;

   Conf.Set ("view.ignore_white_spaces", "false");
   Conf.Set ("view.escape_unknown_tags", "false");
   Conf.Set ("view.ignore_empty_lines", "true");
   declare
      View_Name : constant String := Get_Argument;
   begin
      H.Initialize (Conf);

      Context.Set_Response_Writer (Writer'Unchecked_Access);
      Context.Set_ELContext (ELContext'Unchecked_Access);
      ELContext.Set_Variable_Mapper (Variables'Unchecked_Access);
      ELContext.Set_Resolver (Resolver'Unchecked_Access);
      Writer.Initialize ("text/xml", "UTF-8", Output);

      Set_Current (Context'Unchecked_Access);
      H.Restore_View (View_Name, Context, View);

      H.Render_View (Context, View);
      Writer.Flush;

      Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Writer.Get_Response));
      H.Close;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line ("Cannot read file '" & View_Name & "'");
   end;
end Render;
