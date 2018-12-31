-----------------------------------------------------------------------
--  asf-contexts-exceptions -- Exception handlers in faces context
--  Copyright (C) 2011, 2018 Stephane Carrez
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
with Ada.Exceptions;

with ASF.Events.Exceptions;
private with Util.Strings.Maps;

--  The <b>ASF.Contexts.Exceptions</b> package holds the exception handler framework which
--  allows an application to handle unexpected exceptions and perform specific actions to
--  somehow recover and notify the user.  This package defines a default exception handler
--  which provides some reasonable behavior to report the error or redirect the user to an
--  error page.  Applications can provide their own mechanisms by inheriting from the
--  base exception handler and overriding the <b>Handle</b procedure.
--
--  See JSR 314 - JavaServer Faces Specification 6.2 ExceptionHandler
package ASF.Contexts.Exceptions is

   --  Message used when an exception without a message is raised.
   EXCEPTION_MESSAGE_BASIC_ID    : constant String := "asf.exceptions.unexpected.basic";

   --  Message used when an exception with a message is raised.
   EXCEPTION_MESSAGE_EXTENDED_ID : constant String := "asf.exceptions.unexpected.extended";

   --  ------------------------------
   --  Exception Handler
   --  ------------------------------
   type Exception_Handler is new Ada.Finalization.Limited_Controlled with private;
   type Exception_Handler_Access is access all Exception_Handler'Class;

   --  Take action to handle the <b>Exception_Event</b> instances that have been queued by
   --  calls to <b>Application.Publish_Event</b>.
   --
   --  This operation is called after each ASF phase by the life cycle manager.
   procedure Handle (Handler : in out Exception_Handler);

   --  Set the message id to be used when a given exception name is raised.
   procedure Set_Message (Handler    : in out Exception_Handler;
                          Name       : in String;
                          Message_Id : in String);

   --  ------------------------------
   --  Exception Queue
   --  ------------------------------
   --  The <b>Exception_Queue</b> contains the pending exception events that have been raised
   --  while processing an ASF phase.  The queue contains two lists of events:
   --  <ul>
   --    <li>A list of exceptions that have not yet been processed,
   --    <li>A list of exceptions that were processed by an exception handler
   --  </ul>
   --  When an exception handler has processed an exception event and indicated it as being
   --  <b>PROCESSED</b>, the exception event is moved to the second list.
   type Exception_Queue is new Ada.Finalization.Limited_Controlled with private;

   --  Queue an exception event to the exception handler.  The exception event will be
   --  processed at the end of the current ASF phase.
   procedure Queue_Exception (Queue : in out Exception_Queue;
                              Ex    : in Ada.Exceptions.Exception_Occurrence);

   --  Clear the exception queue.
   overriding
   procedure Finalize (Queue : in out Exception_Queue);

private

   type Exception_Queue is new Ada.Finalization.Limited_Controlled with record
      Unhandled_Events : ASF.Events.Exceptions.Exception_Event_Vector;
      Handled_Events   : ASF.Events.Exceptions.Exception_Event_Vector;
   end record;

   type Exception_Handler is new Ada.Finalization.Limited_Controlled with record
      Mapping : Util.Strings.Maps.Map;
   end record;

end ASF.Contexts.Exceptions;
