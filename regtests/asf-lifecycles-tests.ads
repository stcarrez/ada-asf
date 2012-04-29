-----------------------------------------------------------------------
--  asf-lifecycles-tests -  Tests for ASF lifecycles
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

with Util.Tests;

with ASF.Events.Phases;
package ASF.Lifecycles.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Initialize the test application
   overriding
   procedure Set_Up (T : in out Test);

   --  Test a GET request and the lifecycles that this implies.
   procedure Test_Get_Lifecycle (T : in out Test);

   --  Test a GET+POST request with submitted values and an action method called on the bean.
   procedure Test_Post_Lifecycle (T : in out Test);

   type Phase_Counters is array (ASF.Events.Phases.Phase_Type) of Natural;
   type Phase_Counters_Array is access all Phase_Counters;

   --  A phase listener used to count the number of calls made to Before/After phase
   --  in various configurations.  The phase listener is a readonly instance because it is
   --  shared by multiple concurrent requests.  For the test, we have to use indirect
   --  access to update the counters.
   type Test_Phase_Listener is new Ada.Finalization.Limited_Controlled
     and ASF.Events.Phases.Phase_Listener with record
      Before_Count : Phase_Counters_Array := null;
      After_Count  : Phase_Counters_Array := null;
      Phase        : ASF.Events.Phases.Phase_Type := ASF.Events.Phases.ANY_PHASE;
   end record;

   --  Check that the RESTORE_VIEW and RENDER_RESPONSE counters have the given value.
   procedure Check_Get_Counters (Listener : in Test_Phase_Listener;
                                 T        : in out Test'Class;
                                 Value    : in Natural);

   --  Check that the APPLY_REQUESTS .. INVOKE_APPLICATION counters have the given value.
   procedure Check_Post_Counters (Listener : in Test_Phase_Listener;
                                 T        : in out Test'Class;
                                 Value    : in Natural);

   --  Notifies that the lifecycle phase described by the event is about to begin.
   overriding
   procedure Before_Phase (Listener : in Test_Phase_Listener;
                           Event    : in ASF.Events.Phases.Phase_Event'Class);

   --  Notifies that the lifecycle phase described by the event has finished.
   overriding
   procedure After_Phase (Listener : in Test_Phase_Listener;
                          Event    : in ASF.Events.Phases.Phase_Event'Class);

   --  Return the phase that this listener is interested in processing the <b>Phase_Event</b>
   --  events.  If the listener is interested by several events, it should return <b>ANY_PHASE</b>.
   overriding
   function Get_Phase (Listener : in Test_Phase_Listener) return ASF.Events.Phases.Phase_Type;

   overriding
   procedure Initialize (Listener : in out Test_Phase_Listener);

   overriding
   procedure Finalize (Listener : in out Test_Phase_Listener);

end ASF.Lifecycles.Tests;
