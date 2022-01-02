-----------------------------------------------------------------------
--  asf-contexts-exceptions-iterator -- Exception handlers in faces context
--  Copyright (C) 2011, 2021, 2022 Stephane Carrez
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

--  ------------------------------
--  Iterate over the exception events which are in the queue and execute the given procedure.
--  The procedure should return True in <b>Remove</b> to indicate that the exception has been
--  processed.
--
--  Notes: this procedure should not be called directly (use ASF.Contexts.Faces.Iterate).
--  This procedure is separate to avoid circular dependency.
--  ------------------------------
package body ASF.Contexts.Exceptions.Iterator is

   procedure Iterate
     (Queue   : in out Exception_Queue;
      Context : in out ASF.Contexts.Faces.Faces_Context'Class;
      Process : not null
        access procedure (Event   : in ASF.Events.Exceptions.Exception_Event'Class;
                          Remove  : out Boolean;
                          Context : in out Contexts.Faces.Faces_Context'Class)) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => ASF.Events.Exceptions.Exception_Event'Class,
                                        Name   => ASF.Events.Exceptions.Exception_Event_Access);

      Event  : ASF.Events.Exceptions.Exception_Event_Access;
      Remove : Boolean := False;
      Pos    : Natural := 1;
      Len    : Natural := Natural (Queue.Unhandled_Events.Length);
   begin
      while Pos <= Len loop
         Event := Queue.Unhandled_Events.Element (Pos);

         Process (Event.all, Remove, Context);
         if Remove then
            ASF.Events.Exceptions.Event_Vectors.Delete (Queue.Unhandled_Events, Pos);
            Free (Event);
            if Len > 0 then
               Len := Len - 1;
            end if;
         else
            Pos := Pos + 1;
         end if;
      end loop;
   end Iterate;

end ASF.Contexts.Exceptions.Iterator;
