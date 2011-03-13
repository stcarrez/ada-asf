-----------------------------------------------------------------------
--  asf.sessions.factory -- ASF Sessions factory
--  Copyright (C) 2010, 2011 Stephane Carrez
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

with Ada.Finalization;
with Ada.Strings.Unbounded;
private with Util.Strings;
private with Util.Concurrent.Locks;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Numerics.Discrete_Random;
private with Interfaces;
private with Ada.Streams;

--  The <b>ASF.Sessions.Factory</b> package is a factory for creating, searching
--  and deleting sessions.
package ASF.Sessions.Factory is

   type Session_Factory is tagged limited private;

   --  Allocate a unique and random session identifier.  The default implementation
   --  generates a 256 bit random number that it serializes as base64 in the string.
   --  Upon successful completion, the sequence string buffer is allocated and
   --  returned in <b>Id</b>.  The buffer will be freed when the session is removed.
   procedure Allocate_Session_Id (Factory : in out Session_Factory;
                                  Id      : out Ada.Strings.Unbounded.String_Access);

   --  Create a new session
   procedure Create_Session (Factory : in out Session_Factory;
                             Result  : out Session);

   --  Deletes the session.
   procedure Delete_Session (Factory : in out Session_Factory;
                             Sess    : in out Session);

   --  Finds the session knowing the session identifier.
   --  If the session is found, the last access time is updated.
   --  Otherwise, the null session object is returned.
   procedure Find_Session (Factory : in out Session_Factory;
                           Id      : in String;
                           Result  : out Session);

   --  Returns the maximum time interval, in seconds, that the servlet container will
   --  keep this session open between client accesses. After this interval, the servlet
   --  container will invalidate the session. The maximum time interval can be set with
   --  the Set_Max_Inactive_Interval method.
   --  A negative time indicates the session should never timeout.
   function Get_Max_Inactive_Interval (Factory : in Session_Factory) return Duration;

   --  Specifies the time, in seconds, between client requests before the servlet
   --  container will invalidate this session. A negative time indicates the session
   --  should never timeout.
   procedure Set_Max_Inactive_Interval (Factory  : in out Session_Factory;
                                        Interval : in Duration);

private

   use Util.Strings;

   package Session_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => Name_Access,
                                            Element_Type    => Session,
                                            Hash            => Hash,
                                            Equivalent_Keys => Util.Strings.Equivalent_Keys);

   package Id_Random is new Ada.Numerics.Discrete_Random (Interfaces.Unsigned_32);

   type Session_Factory is new Ada.Finalization.Limited_Controlled with record
      Lock         : Util.Concurrent.Locks.RW_Lock;

      --  Id to session map.
      Sessions     : Session_Maps.Map;

      --  Max inactive time in seconds.
      Max_Inactive : Duration := DEFAULT_INACTIVE_TIMEOUT;

      --  Random number generator used for ID generation.
      Random       : Id_Random.Generator;

      --  Number of 32-bit random numbers used for the ID generation.
      Id_Size      : Ada.Streams.Stream_Element_Offset := 8;
   end record;

   --  Initialize the session factory.
   overriding
   procedure Initialize (Factory : in out Session_Factory);

end ASF.Sessions.Factory;
