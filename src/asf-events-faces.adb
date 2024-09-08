-----------------------------------------------------------------------
--  asf-events -- ASF Events
--  Copyright (C) 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body ASF.Events.Faces is

   --  ------------------------------
   --  Get the lifecycle phase where the event must be processed.
   --  ------------------------------
   function Get_Phase (Event : in Faces_Event) return ASF.Lifecycles.Phase_Type is
   begin
      return Event.Phase;
   end Get_Phase;

   --  ------------------------------
   --  Set the lifecycle phase when this event must be processed.
   --  ------------------------------
   procedure Set_Phase (Event : in out Faces_Event;
                        Phase : in ASF.Lifecycles.Phase_Type) is
   begin
      Event.Phase := Phase;
   end Set_Phase;

   --  ------------------------------
   --  Get the component onto which the event was posted.
   --  ------------------------------
   function Get_Component (Event : in Faces_Event) return Components.Base.UIComponent_Access is
   begin
      return Event.Component;
   end Get_Component;

end ASF.Events.Faces;
