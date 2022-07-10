-----------------------------------------------------------------------
--  asf-lifecycles-tests -  Tests for ASF lifecycles
--  Copyright (C) 2012, 2019, 2022 Stephane Carrez
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

with Util.Test_Caller;
with Util.Beans.Objects;

with ASF.Tests;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Applications.Main;
with ASF.Applications.Tests;
package body ASF.Lifecycles.Tests is

   use Util.Beans.Objects;
   use ASF.Tests;

   package Caller is new Util.Test_Caller (Test, "Lifecycles");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test lifecycle (get)",
                       Test_Get_Lifecycle'Access);
      Caller.Add_Test (Suite, "Test lifecycle (postback)",
                       Test_Post_Lifecycle'Access);
   end Add_Tests;

   --  ------------------------------
   --  Initialize the test application
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);

      use type ASF.Applications.Main.Application_Access;
      Fact     : ASF.Applications.Main.Application_Factory;
   begin
      if ASF.Tests.Get_Application = null then
         ASF.Tests.Initialize (Util.Tests.Get_Properties, Factory => Fact);
      end if;
   end Set_Up;

   --  ------------------------------
   --  Test a GET request and the lifecycles that this implies.
   --  ------------------------------
   procedure Test_Get_Lifecycle (T : in out Test) is
      Request  : ASF.Requests.Mockup.Request;
      Reply    : ASF.Responses.Mockup.Response;
      Form     : aliased ASF.Applications.Tests.Form_Bean;
      Listener : aliased Test_Phase_Listener;
      Handler  : constant Lifecycle_Access := ASF.Tests.Get_Application.Get_Lifecycle_Handler;
   begin
      Handler.Add_Phase_Listener (Listener'Unchecked_Access);
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Get (Request, Reply, "/tests/form-text.html", "form-text.txt");

      Listener.Check_Get_Counters (T, 1);
      Listener.Check_Post_Counters (T, 0);

      Handler.Remove_Phase_Listener (Listener'Unchecked_Access);

   exception
      when others =>
         Handler.Remove_Phase_Listener (Listener'Unchecked_Access);
         raise;
   end Test_Get_Lifecycle;

   --  ------------------------------
   --  Test a GET+POST request with submitted values and an action method called on the bean.
   --  ------------------------------
   procedure Test_Post_Lifecycle (T : in out Test) is

      Request  : ASF.Requests.Mockup.Request;
      Reply    : ASF.Responses.Mockup.Response;
      Form     : aliased ASF.Applications.Tests.Form_Bean;
      Listener : aliased Test_Phase_Listener;
      Handler  : constant Lifecycle_Access := ASF.Tests.Get_Application.Get_Lifecycle_Handler;
   begin
      Handler.Add_Phase_Listener (Listener'Unchecked_Access);
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Get (Request, Reply, "/tests/form-text.html", "form-text.txt");

      Request.Set_Parameter ("formText", "1");
      Request.Set_Parameter ("name", "John");
      Request.Set_Parameter ("password", "12345");
      Request.Set_Parameter ("email", "john@gmail.com");
      Request.Set_Parameter ("ok", "1");
      Do_Post (Request, Reply, "/tests/form-text.html", "form-text-post.txt");

      Listener.Check_Post_Counters (T, 1);
      Listener.Check_Get_Counters (T, 2);

      Handler.Remove_Phase_Listener (Listener'Unchecked_Access);

   exception
      when others =>
         Handler.Remove_Phase_Listener (Listener'Unchecked_Access);
         raise;
   end Test_Post_Lifecycle;

   --  ------------------------------
   --  Check that the RESTORE_VIEW and RENDER_RESPONSE counters have the given value.
   --  ------------------------------
   procedure Check_Get_Counters (Listener : in Test_Phase_Listener;
                                 T        : in out Test'Class;
                                 Value    : in Natural) is
      use ASF.Events.Phases;
   begin
      Util.Tests.Assert_Equals (T, Value, Listener.Before_Count (RESTORE_VIEW),
                                "Invalid before RESTORE_VIEW count");
      Util.Tests.Assert_Equals (T, Value, Listener.After_Count (RESTORE_VIEW),
                                "Invalid after RESTORE_VIEW count");
      Util.Tests.Assert_Equals (T, Value, Listener.Before_Count (RENDER_RESPONSE),
                                "Invalid before RENDER_RESPONSE count");
      Util.Tests.Assert_Equals (T, Value, Listener.After_Count (RENDER_RESPONSE),
                                "Invalid after RENDER_RESPONSE count");
   end Check_Get_Counters;

   --  ------------------------------
   --  Check that the APPLY_REQUESTS .. INVOKE_APPLICATION counters have the given value.
   --  ------------------------------
   procedure Check_Post_Counters (Listener : in Test_Phase_Listener;
                                  T        : in out Test'Class;
                                  Value    : in Natural) is
      use ASF.Events.Phases;
   begin
      Util.Tests.Assert_Equals (T, Value, Listener.Before_Count (APPLY_REQUEST_VALUES),
                                "Invalid before APPLY_REQUEST_VALUES count");
      Util.Tests.Assert_Equals (T, Value, Listener.After_Count (APPLY_REQUEST_VALUES),
                                "Invalid after APPLY_REQUEST_VALUES count");
      Util.Tests.Assert_Equals (T, Value, Listener.Before_Count (PROCESS_VALIDATION),
                                "Invalid before PROCESS_VALIDATION count");
      Util.Tests.Assert_Equals (T, Value, Listener.After_Count (PROCESS_VALIDATION),
                                "Invalid after PROCESS_VALIDATION count");
      Util.Tests.Assert_Equals (T, Value, Listener.Before_Count (UPDATE_MODEL_VALUES),
                                "Invalid before UPDATE_MODEL_VALUES count");
      Util.Tests.Assert_Equals (T, Value, Listener.After_Count (UPDATE_MODEL_VALUES),
                                "Invalid after UPDATE_MODEL_VALUES count");
      Util.Tests.Assert_Equals (T, Value, Listener.Before_Count (INVOKE_APPLICATION),
                                "Invalid before INVOKE_APPLICATION count");
      Util.Tests.Assert_Equals (T, Value, Listener.After_Count (INVOKE_APPLICATION),
                                "Invalid after INVOKE_APPLICATION count");
   end Check_Post_Counters;

   --  ------------------------------
   --  Notifies that the lifecycle phase described by the event is about to begin.
   --  ------------------------------
   overriding
   procedure Before_Phase (Listener : in Test_Phase_Listener;
                           Event    : in ASF.Events.Phases.Phase_Event'Class) is
   begin
      Listener.Before_Count (Event.Phase) := Listener.Before_Count (Event.Phase) + 1;
   end Before_Phase;

   --  ------------------------------
   --  Notifies that the lifecycle phase described by the event has finished.
   --  ------------------------------
   overriding
   procedure After_Phase (Listener : in Test_Phase_Listener;
                          Event    : in ASF.Events.Phases.Phase_Event'Class) is
   begin
      Listener.After_Count (Event.Phase) := Listener.After_Count (Event.Phase) + 1;
   end After_Phase;

   --  ------------------------------
   --  Return the phase that this listener is interested in processing the <b>Phase_Event</b>
   --  events.  If the listener is interested by several events, it should return <b>ANY_PHASE</b>.
   --  ------------------------------
   overriding
   function Get_Phase (Listener : in Test_Phase_Listener) return ASF.Events.Phases.Phase_Type is
   begin
      return Listener.Phase;
   end Get_Phase;

   overriding
   procedure Initialize (Listener : in out Test_Phase_Listener) is
   begin
      Listener.After_Count := new Phase_Counters '(others => 0);
      Listener.Before_Count := new Phase_Counters '(others => 0);
   end Initialize;

   overriding
   procedure Finalize (Listener : in out Test_Phase_Listener) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Phase_Counters,
                                         Name   => Phase_Counters_Array);
   begin
      Free (Listener.After_Count);
      Free (Listener.Before_Count);
   end Finalize;

end ASF.Lifecycles.Tests;
