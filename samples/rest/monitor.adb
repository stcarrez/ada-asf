-----------------------------------------------------------------------
--  monitor - A simple monitor API
--  Copyright (C) 2016 Stephane Carrez
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
with ASF.Responses;
with ASF.Rest.Definition;
package body Monitor is

   type Monitor_Array is array (1 .. MAX_MONITOR) of Monitor_Data;

   Monitors : Monitor_Array;

   --  Get values of the monitor.
   procedure Get_Values (Req    : in out ASF.Rest.Request'Class;
                         Reply  : in out ASF.Rest.Response'Class;
                         Stream : in out ASF.Rest.Output_Stream'Class) is
      Id  : constant String := Req.Get_Path_Parameter (1);
      Pos : Positive;
   begin
      Pos := Positive'Value (Id);
      --  Monitors (Pos).Put (0);

      --  Get the monitor values.
      declare
         Values : constant Value_Array := Monitors (Pos).Get_Values;
      begin

         --  Write the JSON/XML document.
         Stream.Start_Document;
         Stream.Start_Array ("values");
         for V of Values loop
            Stream.Write_Long_Entity ("value", Long_Long_Integer (V));
         end loop;
         Stream.End_Array ("values");
         Stream.End_Document;
      end;

   exception
      when others =>
         Reply.Set_Status (ASF.Responses.SC_NOT_FOUND);
   end Get_Values;

   --  PUT /mon/:id
   procedure Put_Value (Req    : in out ASF.Rest.Request'Class;
                        Reply  : in out ASF.Rest.Response'Class;
                        Stream : in out ASF.Rest.Output_Stream'Class) is
      Id  : constant String := Req.Get_Path_Parameter (1);
      Pos : Positive;
      Val : Natural;
   begin
      Pos := Positive'Value (Id);
      begin
         Val := Natural'Value (Req.Get_Parameter ("value"));
         Monitors (Pos).Put (Val);

      exception
         when others =>
            Reply.Set_Status (ASF.Responses.SC_BAD_REQUEST);
      end;

   exception
      when others =>
         Reply.Set_Status (ASF.Responses.SC_NOT_FOUND);
   end Put_Value;

   protected body Monitor_Data is

      procedure Put (Value : in Natural) is
         use type Ada.Calendar.Time;

         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Dt  : constant Duration := Now - Slot_Start;
         Cnt : Natural := Natural (Dt / Slot_Size);
      begin
         if Cnt > 0 then
            while Cnt > 0 loop
               Cnt := Cnt - 1;
               Pos := Pos + 1;
               if Pos > Values'Last then
                  Pos := Values'First;
                  Value_Count := Values'Length;
               elsif Value_Count < Values'Length then
                  Value_Count := Value_Count + 1;
               end if;
               Slot_Start := Slot_Start + Slot_Size;
            end loop;
         end if;
         Values (Pos) := Value;
      end Put;

      procedure Put (Value : in Natural; Slot : in Natural) is
      begin
         null;
      end Put;

      function Get_Values return Value_Array is
         Result : Value_Array (1 .. Value_Count);
         Cnt    : Natural;
         N      : Natural;
      begin
         if Value_Count = Values'Length then
            Cnt := Values'Last - Pos;
         else
            Cnt := 0;
         end if;
         if Cnt > 0 then
            Result (1 .. Cnt) := Values (Pos + 1 .. Pos + 1 + Cnt - 1);
            N := Cnt + 1;
         else
            N := 1;
         end if;
         if Value_Count = Values'Length then
            Cnt := Pos;
         else
            Cnt := Pos - 1;
         end if;
         if Cnt > 0 then
            Result (N .. N + Cnt - 1) := Values (1 .. Cnt);
         end if;
         return Result;
      end Get_Values;

   end Monitor_Data;

end Monitor;
