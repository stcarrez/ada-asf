-----------------------------------------------------------------------
--  asf-events-exceptions -- Exceptions Events
--  Copyright (C) 2011, 2012, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body ASF.Events.Exceptions is

   --  ------------------------------
   --  Get the exception name.
   --  ------------------------------
   function Get_Exception_Name (Event : in Exception_Event) return String is
   begin
      return Ada.Exceptions.Exception_Name (Event.Ex);
   end Get_Exception_Name;

   --  ------------------------------
   --  Get the exception name.
   --  ------------------------------
   function Get_Exception_Message (Event : in Exception_Event) return String is
   begin
      return Ada.Exceptions.Exception_Message (Event.Ex);
   end Get_Exception_Message;

   --  ------------------------------
   --  Create an exception event with the given exception.
   --  ------------------------------
   function Create_Exception_Event (Ex : in Ada.Exceptions.Exception_Occurrence)
                                    return Exception_Event_Access is
      Event : constant Exception_Event_Access := new Exception_Event;
   begin
      Ada.Exceptions.Save_Occurrence (Target => Event.Ex,
                                      Source => Ex);
      return Event;
   end Create_Exception_Event;

end ASF.Events.Exceptions;
