-----------------------------------------------------------------------
--  asf-events -- ASF Events
--  Copyright (C) 2010, 2012 Stephane Carrez
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
with ASF.Lifecycles;
with ASF.Components.Base;
with ASF.Events.Phases;

with Ada.Containers.Vectors;

--  The <b>ASF.Events</b> package defines the application events that an ASF
--  application can receive.  Events are queued while processing the JSF phases
--  (See UIComponent.Queue_Event).  They are dispatched after each phase
--  (See UIComponent.Broadcast).
--
--  This package is an Ada adaptation for the Java Server Faces Specification
--  JSR 314 - 3.4.2 Application Events.
package ASF.Events.Faces is

   --  ------------------------------
   --  Faces event
   --  ------------------------------
   --  The <b>Faces_Event</b> represents the root type for ASF events.
   --  The event is associated with a component and a lifecycle phase after
   --  which it will be processed.
   type Faces_Event is new Util.Events.Event with private;

   --  Get the lifecycle phase where the event must be processed.
   function Get_Phase (Event : in Faces_Event) return ASF.Lifecycles.Phase_Type;

   --  Set the lifecycle phase when this event must be processed.
   procedure Set_Phase (Event : in out Faces_Event;
                        Phase : in ASF.Lifecycles.Phase_Type);

   --  Get the component onto which the event was posted.
   function Get_Component (Event : in Faces_Event) return Components.Base.UIComponent_Access;

private

   type Faces_Event_Access is access all Faces_Event'Class;

   package Event_Vectors is new Ada.Containers.Vectors (Index_Type   => Natural,
                                                        Element_Type => Faces_Event_Access);

   type Faces_Event is new Util.Events.Event with record
      Phase     : ASF.Events.Phases.Phase_Type := ASF.Events.Phases.RESTORE_VIEW;
      Component : Components.Base.UIComponent_Access := null;
   end record;

end ASF.Events.Faces;
