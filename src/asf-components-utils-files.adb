-----------------------------------------------------------------------
--  components-utils-files -- Include raw files in the output
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
with Ada.Streams.Stream_IO;

with ASF.Requests;
with ASF.Contexts.Writer;

with Util.Beans.Objects;
with Util.Streams.Files;
with Util.Log.Loggers;
package body ASF.Components.Utils.Files is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Components.Utils.Files");

   --  ------------------------------
   --  Get the resource path that must be included.
   --  The resource path is identified by the <b>src</b> attribute.
   --  ------------------------------
   function Get_Resource (UI      : in UIFile;
                          Context : in ASF.Contexts.Faces.Faces_Context'Class) return String is
      Req : constant ASF.Requests.Request_Access := Context.Get_Request;
      Src : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context => Context,
                                                                    Name => "src");
   begin
      if Util.Beans.Objects.Is_Null (Src) then
         UI.Log_Error ("Missing 'src' parameter");
         return "";
      else
         return Req.Get_Resource (Util.Beans.Objects.To_String (Src));
      end if;
   end Get_Resource;

   --  ------------------------------
   --  Copy the stream represented by <b>From</b> in the output stream associated with
   --  the context <b>Context</b>.  When <b>Escape</b> is True, escape any special character
   --  using HTML escape rules.
   --  ------------------------------
   procedure Copy (UI      : in UIFile;
                   From    : in out Util.Streams.Input_Stream'Class;
                   Escape  : in Boolean;
                   Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (UI);
      use Ada.Streams;

      Writer : constant ASF.Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
      Buffer : Stream_Element_Array (0 .. 4_096);
      Last   : Stream_Element_Offset;
   begin
      Writer.Close_Current;
      loop
         From.Read (Buffer, Last);
         if Last > Buffer'First then
            if not Escape then
               Writer.Write (Buffer (Buffer'First .. Last));
            else
               for I in Buffer'First .. Last loop
                  Writer.Write_Char (Char => Character'Val (Buffer (I)));
               end loop;
            end if;
         end if;
         exit when Last < Buffer'Last;
      end loop;
   end Copy;

   --  ------------------------------
   --  Include in the output stream the resource identified by the <b>Get_Resource</b>
   --  function.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIFile;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Path   : constant String := UIFile'Class (UI).Get_Resource (Context);
         Escape : constant Boolean := UI.Get_Attribute ("escape", Context,  True);
         From   : Util.Streams.Files.File_Stream;
      begin
         if Path /= "" then
            Log.Info ("Including file {0} in view", Path);

            From.Open (Name => Path, Mode => Ada.Streams.Stream_IO.In_File);
            UIFile'Class (UI).Copy (From, Escape, Context);
         end if;
      end;
   end Encode_Begin;

end ASF.Components.Utils.Files;
