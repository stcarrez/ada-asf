-----------------------------------------------------------------------
--  asf-contexts-exceptions -- Exception handlers in faces context
--  Copyright (C) 2011, 2018, 2019 Stephane Carrez
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

with ASF.Contexts.Faces;
with ASF.Applications.Messages.Factory;

package body ASF.Contexts.Exceptions is

   --  ------------------------------
   --  Take action to handle the <b>Exception_Event</b> instances that have been queued by
   --  calls to <b>Application.Publish_Event</b>.
   --
   --  This operation is called after each ASF phase by the life cycle manager.
   --  ------------------------------
   procedure Handle (Handler : in out Exception_Handler) is
      use ASF.Applications;
      use type ASF.Contexts.Faces.Faces_Context_Access;

      function Get_Message (Event   : in Events.Exceptions.Exception_Event'Class;
                            Context : in ASF.Contexts.Faces.Faces_Context'Class)
                            return ASF.Applications.Messages.Message;

      procedure Process (Event   : in Events.Exceptions.Exception_Event'Class;
                         Remove  : out Boolean;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class);

      --  ------------------------------
      --  Get a localized message for the exception
      --  ------------------------------
      function Get_Message (Event   : in Events.Exceptions.Exception_Event'Class;
                            Context : in ASF.Contexts.Faces.Faces_Context'Class)
                            return ASF.Applications.Messages.Message is
         Name : constant String := Event.Get_Exception_Name;
         Msg  : constant String := Event.Get_Exception_Message;
         Pos  : constant Util.Strings.Maps.Cursor := Handler.Mapping.Find (Name);
      begin
         if Util.Strings.Maps.Has_Element (Pos) then
            return Messages.Factory.Get_Message (Context    => Context,
                                                 Message_Id => Util.Strings.Maps.Element (Pos),
                                                 Param1     => Msg);
         elsif Msg'Length = 0 then
            return Messages.Factory.Get_Message (Context    => Context,
                                                 Message_Id => EXCEPTION_MESSAGE_BASIC_ID,
                                                 Param1     => Name);
         else
            return Messages.Factory.Get_Message (Context    => Context,
                                                 Message_Id => EXCEPTION_MESSAGE_EXTENDED_ID,
                                                 Param1     => Name,
                                                 Param2     => Msg);

         end if;
      end Get_Message;

      --  ------------------------------
      --  Process each exception event and add a message in the faces context.
      --  ------------------------------
      procedure Process (Event   : in Events.Exceptions.Exception_Event'Class;
                         Remove  : out Boolean;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
         Msg : constant Messages.Message := Get_Message (Event, Context);
      begin
         Context.Add_Message (Client_Id => "",
                              Message   => Msg);
         Remove := True;
      end Process;

      Context : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;

   begin
      if Context = null then
         return;
      end if;
      if Context.Get_Response_Completed then
         return;
      end if;
      Context.Iterate_Exception (Process'Access);
   end Handle;

   --  ------------------------------
   --  Set the message id to be used when a given exception name is raised.
   --  ------------------------------
   procedure Set_Message (Handler    : in out Exception_Handler;
                          Name       : in String;
                          Message_Id : in String) is
   begin
      Handler.Mapping.Insert (Name, Message_Id);
   end Set_Message;

   --  ------------------------------
   --  Queue an exception event to the exception handler.  The exception event will be
   --  processed at the end of the current ASF phase.
   --  ------------------------------
   procedure Queue_Exception (Queue : in out Exception_Queue;
                              Ex      : in Ada.Exceptions.Exception_Occurrence) is
   begin
      Queue.Unhandled_Events.Append (ASF.Events.Exceptions.Create_Exception_Event (Ex));
   end Queue_Exception;

   --  ------------------------------
   --  Clear the exception queue.
   --  ------------------------------
   overriding
   procedure Finalize (Queue : in out Exception_Queue) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => ASF.Events.Exceptions.Exception_Event'Class,
                                        Name   => ASF.Events.Exceptions.Exception_Event_Access);

      Len : Natural;
   begin
      loop
         Len := Natural (Queue.Unhandled_Events.Length);
         exit when Len = 0;
         declare
            Event : ASF.Events.Exceptions.Exception_Event_Access
              := Queue.Unhandled_Events.Element (Len);
         begin
            Free (Event);
            Queue.Unhandled_Events.Delete (Len);
         end;
      end loop;
   end Finalize;

end ASF.Contexts.Exceptions;
