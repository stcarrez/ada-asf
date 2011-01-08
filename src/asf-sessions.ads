-----------------------------------------------------------------------
--  asf.sessions -- ASF Sessions
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
with EL.Objects;
with Ada.Calendar;
with Ada.Finalization;
private with Ada.Strings.Unbounded;
private with Util.Strings;
private with Util.Beans.Objects.Maps;
private with Util.Concurrent.Locks;
private with Util.Concurrent.Counters;

--  The <b>ASF.Sessions</b> package is an Ada implementation of the
--  Java servlet Specification (See JSR 315 at jcp.org).
package ASF.Sessions is

   --  Raised if there is no session.
   No_Session : exception;

   --  The default session inactive timeout.
   DEFAULT_INACTIVE_TIMEOUT : constant Duration := 300.0;

   --  Provides a way to identify a user across more than one page request
   --  or visit to a Web site and to store information about that user.
   --
   --  The servlet container uses this interface to create a session between
   --  an HTTP client and an HTTP server.  The session persists for a specified
   --  time period, across more than one connection or page request from the user.
   --  A session usually corresponds to one user, who may visit a site many times.
   --  The server can maintain a session in many ways such as using cookies
   --  or rewriting URLs.
   --
   --  This interface allows servlets to
   --   * View and manipulate information about a session, such as the session
   --     identifier, creation time, and last accessed time
   --   * Bind objects to sessions, allowing user information to persist across
   --     multiple user connections
   --
   --  A servlet should be able to handle cases in which the client does not
   --  choose to join a session, such as when cookies are intentionally turned off.
   --  Until the client joins the session, isNew returns true. If the client chooses
   --  not to join the session, getSession will return a different session on each
   --  request, and isNew will always return true.
   --
   --  Session information is scoped only to the current web application (ServletContext),
   --  so information stored in one context will not be directly visible in another.
   type Session is tagged private;

   --  Returns true if the session is valid.
   function Is_Valid (Sess : in Session'Class) return Boolean;

   --  Returns a string containing the unique identifier assigned to this session.
   --  The identifier is assigned by the servlet container and is implementation dependent.
   function Get_Id (Sess : in Session) return String;

   --  Returns the last time the client sent a request associated with this session,
   --  as the number of milliseconds since midnight January 1, 1970 GMT, and marked
   --  by the time the container recieved the request.
   --
   --  Actions that your application takes, such as getting or setting a value associated
   --  with the session, do not affect the access time.
   function Get_Last_Accessed_Time (Sess : in Session) return Ada.Calendar.Time;

   --  Returns the maximum time interval, in seconds, that the servlet container will
   --  keep this session open between client accesses. After this interval, the servlet
   --  container will invalidate the session. The maximum time interval can be set with
   --  the Set_Max_Inactive_Interval method.
   --  A negative time indicates the session should never timeout.
   function Get_Max_Inactive_Interval (Sess : in Session) return Duration;

   --  Specifies the time, in seconds, between client requests before the servlet
   --  container will invalidate this session. A negative time indicates the session
   --  should never timeout.
   procedure Set_Max_Inactive_Interval (Sess     : in Session;
                                        Interval : in Duration);

   --  Returns the object bound with the specified name in this session,
   --  or null if no object is bound under the name.
   function Get_Attribute (Sess : in Session;
                           Name : in String) return EL.Objects.Object;

   --  Binds an object to this session, using the name specified.
   --  If an object of the same name is already bound to the session,
   --  the object is replaced.
   --
   --  If the value passed in is null, this has the same effect as calling
   --  removeAttribute().
   procedure Set_Attribute (Sess  : in out Session;
                            Name  : in String;
                            Value : in EL.Objects.Object);

   --  Removes the object bound with the specified name from this session.
   --  If the session does not have an object bound with the specified name,
   --  this method does nothing.
   procedure Remove_Attribute (Sess : in out Session;
                               Name : in String);

   --  Invalidates this session then unbinds any objects bound to it.
   procedure Invalidate (Sess : in out Session);

   Null_Session : constant Session;
private

   type Session_Record;
   type Session_Record_Access is access all Session_Record'Class;

   type Session_Record is new Ada.Finalization.Limited_Controlled with record
      --  Reference counter.
      Ref_Counter  : Util.Concurrent.Counters.Counter;

      --  RW lock to protect access to members.
      Lock         : Util.Concurrent.Locks.RW_Lock;

      --  Attributes bound to this session.
      Attributes   : aliased Util.Beans.Objects.Maps.Map;

      --  Time when the session was created.
      Create_Time  : Ada.Calendar.Time;

      --  Time when the session was last accessed.
      Access_Time  : Ada.Calendar.Time;

      --  Max inactive time in seconds.
      Max_Inactive : Duration := DEFAULT_INACTIVE_TIMEOUT;

      --  Session identifier.
      Id           : Ada.Strings.Unbounded.String_Access;

      --  True if the session is active.
      Is_Active    : Boolean := True;
   end record;

   overriding
   procedure Finalize (Object : in out Session_Record);

   type Session is new Ada.Finalization.Controlled with record
      Impl : Session_Record_Access;
   end record;

   --  Adjust (increment) the session record reference counter.
   overriding
   procedure Adjust (Object : in out Session);

   --  Decrement the session record reference counter and free the session record
   --  if this was the last session reference.
   overriding
   procedure Finalize (Object : in out Session);

   Null_Session : constant Session := Session' (Ada.Finalization.Controlled with Impl => null);

end ASF.Sessions;
