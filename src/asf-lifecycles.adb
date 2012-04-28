-----------------------------------------------------------------------
--  asf-lifecycles -- Lifecycle
--  Copyright (C) 2010, 2011 Stephane Carrez
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
package body ASF.Lifecycles is

   --  ------------------------------
   --  Initialize the the lifecycle handler.
   --  ------------------------------
   procedure Initialize (Controller : in out Lifecycle;
                         App        : access ASF.Applications.Main.Application'Class) is
   begin
      --  Create the phase controllers.
      Lifecycle'Class (Controller).Create_Phase_Controllers;

      --  Initialize the phase controllers.
      for Phase in Controller.Controllers'Range loop
         Controller.Controllers (Phase).Initialize (App);
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
   --  Register a bundle and bind it to a facelet variable.
   --  ------------------------------
   procedure Execute (Controller : in Lifecycle;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use type ASF.Contexts.Exceptions.Exception_Handler_Access;
      use ASF.Events.Phases;
   begin
      for Phase in RESTORE_VIEW .. INVOKE_APPLICATION loop
         if Context.Get_Render_Response or Context.Get_Response_Completed then
            return;
         end if;
         begin
            Controller.Controllers (Phase).Execute (Context);
         exception
            when E : others =>
               Context.Queue_Exception (E);
         end;

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
      end loop;
   end Execute;

   --  ------------------------------
   --  Render the response by executing the response phase.
   --  ------------------------------
   procedure Render (Controller : in Lifecycle;
                     Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      Controller.Controllers (ASF.Events.Phases.RENDER_RESPONSE).Execute (Context);
   end Render;

end ASF.Lifecycles;
