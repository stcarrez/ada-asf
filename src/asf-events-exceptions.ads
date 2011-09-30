-----------------------------------------------------------------------
--  asf-events-exceptions -- Exceptions Events
--  Copyright (C) 2011 Stephane Carrez
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

with Util.Events;

with Ada.Exceptions;
with Ada.Containers.Vectors;

package ASF.Events.Exceptions is

   --  ------------------------------
   --  Exception event
   --  ------------------------------
   --  The <b>Exception_Event</b> represents an exception that is raised while processing
   --  a request.  If is posted by the ASF framework when an unhandled exception is caught.
   --  An application can also publish such event when necessary.
   --
   --  After each lifecycle phase, the exception handler is invoked to process the
   --  <b>Exception_Event</b> that have been queued.
   type Exception_Event is new Util.Events.Event with record
      Ex : Ada.Exceptions.Exception_Occurrence;
   end record;
   type Exception_Event_Access is access all Exception_Event'Class;

   --  Get the exception name.
   function Get_Exception_Name (Event : in Exception_Event) return String;

   --  Get the exception name.
   function Get_Exception_Message (Event : in Exception_Event) return String;

   --  Create an exception event with the given exception.
   function Create_Exception_Event (Ex : in Ada.Exceptions.Exception_Occurrence)
                                    return Exception_Event_Access;

   package Event_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Exception_Event_Access);

   subtype Exception_Event_Vector is Event_Vectors.Vector;
   subtype Exception_Event_Cursor is Event_Vectors.Cursor;

end ASF.Events.Exceptions;
