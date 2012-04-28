-----------------------------------------------------------------------
--  asf-events-phases -- Lifecycle phase event
--  Copyright (C) 2012 Stephane Carrez
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

--  The <b>ASF.Events.Phases</b> package defines the phase event which represents
--  the beginning or ending of processing for a particular phase of the request lifecycle.
--
--  This package is an Ada adaptation for the Java Server Faces Specification
--  JSR 314 - 3.4.2 Application Events.
package ASF.Events.Phases is

   type Phase_Type is (ANY_PHASE,
                       RESTORE_VIEW,
                       APPLY_REQUEST_VALUES,
                       PROCESS_VALIDATION,
                       UPDATE_MODEL_VALUES,
                       INVOKE_APPLICATION,
                       RENDER_RESPONSE);

   --  ------------------------------
   --  Phase event
   --  ------------------------------
   --  The <b>Phase_Event</b> represents the phase event notifying application of the
   --  current lifecycly processing.
   type Phase_Event (Phase : Phase_Type) is new Util.Events.Event with null record;

   --  ------------------------------
   --  Phase listener
   --  ------------------------------
   --  The <b>Phase_Listener</b> is an interface which allows application to be called
   --  and receive the <b>Phase_Event</b> during the ASF lifecycle processing.
   type Phase_Listener is limited interface and Util.Events.Event_Listener;
   type Phase_Listener_Access is access all Phase_Listener'Class;

   --  Notifies that the lifecycle phase described by the event is about to begin.
   procedure Before_Phase (Listener : in Phase_Listener;
                           Event    : in Phase_Event'Class) is null;

   --  Notifies that the lifecycle phase described by the event has finished.
   procedure After_Phase (Listener : in Phase_Listener;
                          Event    : in Phase_Event'Class) is null;

   --  Return the phase that this listener is interested in processing the <b>Phase_Event</b>
   --  events.  If the listener is interested by several events, it should return <b>ANY_PHASE</b>.
   function Get_Phase (Listener : in Phase_Listener) return Phase_Type is abstract;

end ASF.Events.Phases;
