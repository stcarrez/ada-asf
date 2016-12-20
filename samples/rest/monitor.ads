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
with Ada.Calendar;
with ASF.Rest.Definition;
package Monitor is

   type Mon is new Natural;

   --  Get values of the monitor.
   procedure Get_Values (D      : in out Mon;
                         Req    : in out ASF.Rest.Request'Class;
                         Reply  : in out ASF.Rest.Response'Class;
                         Stream : in out ASF.Rest.Output_Stream'Class);

   --  PUT /mon/:id
   procedure Put_Value (D      : in out Mon;
                        Req    : in out ASF.Rest.Request'Class;
                        Reply  : in out ASF.Rest.Response'Class;
                        Stream : in out ASF.Rest.Output_Stream'Class);

   package Mon_API is new ASF.Rest.Definition (Object_Type => Mon,
                                               URI         => "/api/monitor");

private

   --  Declare each REST API with a relative URI from Mon_API definition.
   --  GET /api/monitor/:id
   package Mon_Get_Values is new Mon_API.Definition (Handler    => Get_Values'Access,
                                                     Method     => ASF.Rest.GET,
                                                     Pattern    => ":id",
                                                     Permission => 0);

   --  PUT /api/monitor/:id
   package Mon_Put_Value is new Mon_API.Definition (Handler    => Put_Value'Access,
                                                    Method     => ASF.Rest.PUT,
                                                    Pattern    => ":id",
                                                    Permission => 0);

   MAX_VALUES  : constant Natural := 1000;
   MAX_MONITOR : constant Natural := 10;

   type Value_Array is array (Natural range <>) of Natural;

   protected type Monitor_Data is

      procedure Put (Value : in Natural);

      procedure Put (Value : in Natural; Slot : in Natural);

      function Get_Values return Value_Array;

   private
      Values      : Value_Array (1 .. MAX_VALUES) := (others => 0);
      Value_Count : Natural := 0;
      Pos         : Natural := 1;
      Slot_Size   : Duration := 10.0;
      Slot_Start  : Ada.Calendar.Time := Ada.Calendar.Clock;
   end Monitor_Data;

end Monitor;
