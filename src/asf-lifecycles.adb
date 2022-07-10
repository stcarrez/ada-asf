-----------------------------------------------------------------------
--  asf-lifecycles -- Lifecycle
--  Copyright (C) 2010, 2011, 2012, 2018, 2022 Stephane Carrez
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

with ASF.Contexts.Exceptions;
with ASF.Contexts.Flash;
package body ASF.Lifecycles is

   --  ------------------------------
   --  Initialize the the lifecycle handler.
   --  ------------------------------
   procedure Initialize (Controller : in out Lifecycle;
                         Views      : ASF.Applications.Views.View_Handler_Access) is
   begin
      --  Create the phase controllers.
      Lifecycle'Class (Controller).Create_Phase_Controllers;

      --  Initialize the phase controllers.
      for Phase in Controller.Controllers'Range loop
         Controller.Controllers (Phase).Initialize (Views);
      end loop;
   end Initialize;

   --  ------------------------------
   --  Finalize the lifecycle handler, freeing the allocated storage.
   --  ------------------------------
   overriding
   procedure Finalize (Controller : in out Lifecycle) is
      procedure Free is new Ada.Unchecked_Deallocation (Phase_Controller'Class,
                                                        Phase_Controller_Access);
   begin
      --  Free the phase controllers.
      for Phase in Controller.Controllers'Range loop
         Free (Controller.Controllers (Phase));
      end loop;
   end Finalize;

   --  ------------------------------
   --  Set the controller to be used for the given phase.
   --  ------------------------------
   procedure Set_Controller (Controller : in out Lifecycle;
                             Phase      : in Phase_Type;
                             Instance   : in Phase_Controller_Access) is
   begin
      Controller.Controllers (Phase) := Instance;
   end Set_Controller;

   --  ------------------------------
   --  Execute the lifecycle controller associated with the phase defined in <b>Phase</b>.
   --  Before processing, setup the faces context to update the current phase, then invoke
   --  the <b>Before_Phase</b> actions of the phase listeners.  After execution of the controller
   --  invoke the <b>After_Phase</b> actions of the phase listeners.
   --  If an exception is raised, catch it and save it in the faces context.
   --  ------------------------------
   procedure Execute (Controller : in Lifecycle;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class;
                      Listeners  : in Listener_Vectors.Ref;
                      Phase      : in Phase_Type) is
      use type ASF.Events.Phases.Phase_Type;
      use type ASF.Contexts.Exceptions.Exception_Handler_Access;

      --  Execute the before phase listener action.
      procedure Before_Phase (Listener : in ASF.Events.Phases.Phase_Listener_Access);

      --  Execute the after phase listener action.
      procedure After_Phase (Listener : in ASF.Events.Phases.Phase_Listener_Access);

      Event : ASF.Events.Phases.Phase_Event (Phase);
      Flash : constant ASF.Contexts.Faces.Flash_Context_Access := Context.Get_Flash;

      --  ------------------------------
      --  Execute the before phase listener action.
      --  ------------------------------
      procedure Before_Phase (Listener : in ASF.Events.Phases.Phase_Listener_Access) is
         P : constant ASF.Events.Phases.Phase_Type := Listener.Get_Phase;
      begin
         if P = Event.Phase or else P = ASF.Events.Phases.ANY_PHASE then
            Listener.Before_Phase (Event);
         end if;

      exception
         when E : others =>
            Context.Queue_Exception (E);
      end Before_Phase;

      --  ------------------------------
      --  Execute the after phase listener action.
      --  ------------------------------
      procedure After_Phase (Listener : in ASF.Events.Phases.Phase_Listener_Access) is
         P : constant ASF.Events.Phases.Phase_Type := Listener.Get_Phase;
      begin
         if P = Event.Phase or else P = ASF.Events.Phases.ANY_PHASE then
            Listener.After_Phase (Event);
         end if;

      exception
         when E : others =>
            Context.Queue_Exception (E);
      end After_Phase;

   begin
      Context.Set_Current_Phase (Phase);

      Flash.Do_Pre_Phase_Actions (Phase, Context);

      --  Call the before phase listeners if there are some.
      Listeners.Iterate (Before_Phase'Access);

      begin
         Controller.Controllers (Phase).Execute (Context);
      exception
         when E : others =>
            Context.Queue_Exception (E);
      end;

      Flash.Do_Post_Phase_Actions (Phase, Context);

      Listeners.Iterate (After_Phase'Access);

      --  If exceptions have been raised and queued during the current phase, process them.
      --  An exception handler could use them to redirect the current request to another
      --  page or navigate to a specific view.
      declare
         Ex : constant ASF.Contexts.Exceptions.Exception_Handler_Access
           := Context.Get_Exception_Handler;
      begin
         if Ex /= null then
            Ex.Handle;
         end if;
      end;
   end Execute;

   --  ------------------------------
   --  Register a bundle and bind it to a facelet variable.
   --  ------------------------------
   procedure Execute (Controller : in Lifecycle;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use ASF.Events.Phases;

      Listeners : constant Listener_Vectors.Ref := Controller.Listeners.Get;
   begin
      for Phase in RESTORE_VIEW .. INVOKE_APPLICATION loop
         if Context.Get_Render_Response or else Context.Get_Response_Completed then
            return;
         end if;
         Controller.Execute (Context, Listeners, Phase);
      end loop;
   end Execute;

   --  ------------------------------
   --  Render the response by executing the response phase.
   --  ------------------------------
   procedure Render (Controller : in Lifecycle;
                     Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Listeners : constant Listener_Vectors.Ref := Controller.Listeners.Get;
   begin
      Controller.Execute (Context, Listeners, ASF.Events.Phases.RENDER_RESPONSE);
   end Render;

   --  ------------------------------
   --  Add a phase listener in the lifecycle controller.
   --  ------------------------------
   procedure Add_Phase_Listener (Controller : in out Lifecycle;
                                 Listener   : in ASF.Events.Phases.Phase_Listener_Access) is
   begin
      Controller.Listeners.Append (Listener);
   end Add_Phase_Listener;

   --  ------------------------------
   --  Remove a phase listener that was registered in the lifecycle controller.
   --  ------------------------------
   procedure Remove_Phase_Listener (Controller : in out Lifecycle;
                                    Listener   : in ASF.Events.Phases.Phase_Listener_Access) is
   begin
      Controller.Listeners.Remove (Listener);
   end Remove_Phase_Listener;

end ASF.Lifecycles;
