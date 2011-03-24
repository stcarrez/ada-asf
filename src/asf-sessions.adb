-----------------------------------------------------------------------
--  asf.sessions -- ASF Sessions
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

with Ada.Unchecked_Deallocation;

--  The <b>ASF.Sessions</b> package is an Ada implementation of the
--  Java servlet Specification (See JSR 315 at jcp.org).
package body ASF.Sessions is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Returns true if the session is valid.
   --  ------------------------------
   function Is_Valid (Sess : in Session'Class) return Boolean is
   begin
      return Sess.Impl /= null and then Sess.Impl.Is_Active;
   end Is_Valid;

   --  ------------------------------
   --  Returns a string containing the unique identifier assigned to this session.
   --  The identifier is assigned by the servlet container and is implementation dependent.
   --  ------------------------------
   function Get_Id (Sess : in Session) return String is
   begin
      if Sess.Impl = null or else not Sess.Impl.Is_Active then
         raise No_Session;
      else
         return Sess.Impl.Id.all;
      end if;
   end Get_Id;

   --  ------------------------------
   --  Returns the last time the client sent a request associated with this session,
   --  as the number of milliseconds since midnight January 1, 1970 GMT, and marked
   --  by the time the container recieved the request.
   --
   --  Actions that your application takes, such as getting or setting a value associated
   --  with the session, do not affect the access time.
   --  ------------------------------
   function Get_Last_Accessed_Time (Sess : in Session) return Ada.Calendar.Time is
   begin
      if Sess.Impl = null or else not Sess.Impl.Is_Active then
         raise No_Session;
      else
         return Sess.Impl.Access_Time;
      end if;
   end Get_Last_Accessed_Time;

   --  ------------------------------
   --  Returns the maximum time interval, in seconds, that the servlet container will
   --  keep this session open between client accesses. After this interval, the servlet
   --  container will invalidate the session. The maximum time interval can be set with
   --  the Set_Max_Inactive_Interval method.
   --  A negative time indicates the session should never timeout.
   --  ------------------------------
   function Get_Max_Inactive_Interval (Sess : in Session) return Duration is
   begin
      if Sess.Impl = null or else not Sess.Impl.Is_Active then
         raise No_Session;
      else
         return Sess.Impl.Max_Inactive;
      end if;
   end Get_Max_Inactive_Interval;

   --  ------------------------------
   --  Specifies the time, in seconds, between client requests before the servlet
   --  container will invalidate this session. A negative time indicates the session
   --  should never timeout.
   --  ------------------------------
   procedure Set_Max_Inactive_Interval (Sess     : in Session;
                                        Interval : in Duration) is
   begin
      if Sess.Impl = null or else not Sess.Impl.Is_Active then
         raise No_Session;
      else
         Sess.Impl.Max_Inactive := Interval;
      end if;
   end Set_Max_Inactive_Interval;

   --  ------------------------------
   --  Returns the object bound with the specified name in this session,
   --  or null if no object is bound under the name.
   --  ------------------------------
   function Get_Attribute (Sess : in Session;
                           Name : in String) return EL.Objects.Object is
      Key : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Sess.Impl = null or else not Sess.Impl.Is_Active then
         raise No_Session;
      end if;

      Sess.Impl.Lock.Read;
      declare
         Pos : constant EL.Objects.Maps.Cursor := Sess.Impl.Attributes.Find (Key);
      begin
         if EL.Objects.Maps.Has_Element (Pos) then
            return Value : constant EL.Objects.Object := EL.Objects.Maps.Element (Pos) do
               Sess.Impl.Lock.Release_Read;
            end return;
         end if;

      exception
         when others =>
            Sess.Impl.Lock.Release_Read;
            raise;
      end;
      Sess.Impl.Lock.Release_Read;
      return EL.Objects.Null_Object;
   end Get_Attribute;

   --  ------------------------------
   --  Binds an object to this session, using the name specified.
   --  If an object of the same name is already bound to the session,
   --  the object is replaced.
   --
   --  If the value passed in is null, this has the same effect as calling
   --  removeAttribute().
   --  ------------------------------
   procedure Set_Attribute (Sess  : in out Session;
                            Name  : in String;
                            Value : in EL.Objects.Object) is
   begin
      if Sess.Impl = null or else not Sess.Impl.Is_Active then
         raise No_Session;
      end if;

      declare
         Key : constant Unbounded_String := To_Unbounded_String (Name);
      begin
         Sess.Impl.Lock.Write;
         if EL.Objects.Is_Null (Value) then
            Sess.Impl.Attributes.Delete (Key);
         else
            Sess.Impl.Attributes.Include (Key, Value);
         end if;

      exception
         when others =>
            Sess.Impl.Lock.Release_Write;
            raise;
      end;
      Sess.Impl.Lock.Release_Write;
   end Set_Attribute;

   --  ------------------------------
   --  Removes the object bound with the specified name from this session.
   --  If the session does not have an object bound with the specified name,
   --  this method does nothing.
   --  ------------------------------
   procedure Remove_Attribute (Sess : in out Session;
                               Name : in String) is
   begin
      Set_Attribute (Sess, Name, EL.Objects.Null_Object);
   end Remove_Attribute;

   --  ------------------------------
   --  Gets the principal that authenticated to the session.
   --  Returns null if there is no principal authenticated.
   --  ------------------------------
   function Get_Principal (Sess : in Session) return ASF.Principals.Principal_Access is
   begin
      if Sess.Impl = null or else not Sess.Impl.Is_Active then
         raise No_Session;
      end if;
      return Sess.Impl.Principal;
   end Get_Principal;

   --  ------------------------------
   --  Sets the principal associated with the session.
   --  ------------------------------
   procedure Set_Principal (Sess      : in out Session;
                            Principal : in ASF.Principals.Principal_Access) is
   begin
      if Sess.Impl = null or else not Sess.Impl.Is_Active then
         raise No_Session;
      end if;
      Sess.Impl.Principal := Principal;
   end Set_Principal;

   --  ------------------------------
   --  Invalidates this session then unbinds any objects bound to it.
   --  ------------------------------
   procedure Invalidate (Sess : in out Session) is
   begin
      if Sess.Impl /= null then
         Sess.Impl.Is_Active := False;
         Finalize (Sess);
      end if;
   end Invalidate;

   --  ------------------------------
   --  Adjust (increment) the session record reference counter.
   --  ------------------------------
   overriding
   procedure Adjust (Object : in out Session) is
   begin
      if Object.Impl /= null then
         Util.Concurrent.Counters.Increment (Object.Impl.Ref_Counter);
      end if;
   end Adjust;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Session_Record'Class,
                                     Name   => Session_Record_Access);

   --  ------------------------------
   --  Decrement the session record reference counter and free the session record
   --  if this was the last session reference.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out Session) is
      Release : Boolean;
   begin
      if Object.Impl /= null then
         Util.Concurrent.Counters.Decrement (Object.Impl.Ref_Counter, Release);
         if Release then
            Free (Object.Impl);
         else
            Object.Impl := null;
         end if;
      end if;
   end Finalize;

   overriding
   procedure Finalize (Object : in out Session_Record) is
   begin
      Free (Object.Id);
   end Finalize;

end ASF.Sessions;
