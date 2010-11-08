-----------------------------------------------------------------------
--  asf.sessions.factory -- ASF Sessions factory
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

with Util.Encoders.Base64;

--  The <b>ASF.Sessions.Factory</b> package is a factory for creating, searching
--  and deleting sessions.
package body ASF.Sessions.Factory is

   use Ada.Finalization;
   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Create a new session
   --  ------------------------------
   procedure Create_Session (Factory : in out Session_Factory;
                             Result  : out Session) is
      use Ada.Streams;
      use Interfaces;

      Sess    : Session;
      Impl    : constant Session_Record_Access := new Session_Record;
      Rand    : Stream_Element_Array (0 .. 4 * Factory.Id_Size - 1);
      Buffer  : Stream_Element_Array (0 .. 4 * 3 * Factory.Id_Size);
      Encoder : Util.Encoders.Base64.Encoder;
      Last    : Stream_Element_Offset;
      Encoded : Stream_Element_Offset;
   begin
      Impl.Ref_Counter  := Util.Concurrent.Counters.ONE;
      Impl.Create_Time  := Ada.Calendar.Clock;
      Impl.Access_Time  := Impl.Create_Time;
      Impl.Max_Inactive := Factory.Max_Inactive;
      Sess.Impl         := Impl;

      Factory.Lock.Write;

      --  Generate the random sequence.
      for I in 0 .. Factory.Id_Size - 1 loop
         declare
            Value : constant Unsigned_32 := Id_Random.Random (Factory.Random);
         begin
            Rand (4 * I)     := Stream_Element (Value and 16#0FF#);
            Rand (4 * I + 1) := Stream_Element (Shift_Right (Value, 8) and 16#0FF#);
            Rand (4 * I + 2) := Stream_Element (Shift_Right (Value, 16) and 16#0FF#);
            Rand (4 * I + 3) := Stream_Element (Shift_Right (Value, 24) and 16#0FF#);
         end;
      end loop;

      --  Encode the random stream in base64 and save it into the Id string.
      Encoder.Transform (Data => Rand, Into => Buffer,
                         Last => Last, Encoded => Encoded);

      declare
         Id : constant String_Access := new String (1 .. Natural (Encoded + 1));
      begin
         for I in 0 .. Encoded loop
            Id (Natural (I + 1)) := Character'Val (Buffer (I));
         end loop;

         Impl.Id := Id;
         Factory.Sessions.Insert (Id.all'Access, Sess);
      end;
      Factory.Lock.Release_Write;

      Result := Sess;
   end Create_Session;

   --  ------------------------------
   --  Deletes the session.
   --  ------------------------------
   procedure Delete_Session (Factory : in out Session_Factory;
                             Sess    : in out Session) is
   begin
      null;
   end Delete_Session;

   --  ------------------------------
   --  Finds the session knowing the session identifier.
   --  If the session is found, the last access time is updated.
   --  Otherwise, the null session object is returned.
   --  ------------------------------
   procedure Find_Session (Factory : in out Session_Factory;
                           Id      : in String;
                           Result  : out Session) is
   begin
      Result := Null_Session;
      Factory.Lock.Read;
      declare
         Pos : constant Session_Maps.Cursor := Factory.Sessions.Find (Id'Unrestricted_Access);
      begin
         if Session_Maps.Has_Element (Pos) then
            Result := Session_Maps.Element (Pos);
         end if;
      end;
      Factory.Lock.Release_Read;

      if Result.Is_Valid then
         Result.Impl.Access_Time := Ada.Calendar.Clock;
      end if;
   end Find_Session;

   --  ------------------------------
   --  Returns the maximum time interval, in seconds, that the servlet container will
   --  keep this session open between client accesses. After this interval, the servlet
   --  container will invalidate the session. The maximum time interval can be set with
   --  the Set_Max_Inactive_Interval method.
   --  A negative time indicates the session should never timeout.
   --  ------------------------------
   function Get_Max_Inactive_Interval (Factory : in Session_Factory) return Duration is
   begin
      return Factory.Max_Inactive;
   end Get_Max_Inactive_Interval;

   --  ------------------------------
   --  Specifies the time, in seconds, between client requests before the servlet
   --  container will invalidate this session. A negative time indicates the session
   --  should never timeout.
   --  ------------------------------
   procedure Set_Max_Inactive_Interval (Factory  : in out Session_Factory;
                                        Interval : in Duration) is
   begin
      Factory.Max_Inactive := Interval;
   end Set_Max_Inactive_Interval;

   --  ------------------------------
   --  Initialize the session factory.
   --  ------------------------------
   overriding
   procedure Initialize (Factory : in out Session_Factory) is
   begin
      Id_Random.Reset (Factory.Random);
   end Initialize;

end ASF.Sessions.Factory;
