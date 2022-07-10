-----------------------------------------------------------------------
--  asf-lifecycles-default -- Default Lifecycle handler
--  Copyright (C) 2010, 2018, 2022 Stephane Carrez
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

with ASF.Lifecycles.Restore;
with ASF.Lifecycles.Apply;
with ASF.Lifecycles.Validation;
with ASF.Lifecycles.Update;
with ASF.Lifecycles.Invoke;
with ASF.Lifecycles.Response;
package body ASF.Lifecycles.Default is

   --  ------------------------------
   --  Creates the phase controllers by invoking the <b>Set_Controller</b>
   --  procedure for each phase.  This is called by <b>Initialize</b> to build
   --  the lifecycle handler.
   --  ------------------------------
   overriding
   procedure Create_Phase_Controllers (Controller : in out Lifecycle) is
      use ASF.Events.Phases;
   begin
      Controller.Set_Controller (Phase    => RESTORE_VIEW,
                                 Instance => new Lifecycles.Restore.Restore_Controller);

      Controller.Set_Controller (Phase    => APPLY_REQUEST_VALUES,
                                 Instance => new Lifecycles.Apply.Apply_Controller);

      Controller.Set_Controller (Phase    => PROCESS_VALIDATION,
                                 Instance => new Lifecycles.Validation.Validation_Controller);

      Controller.Set_Controller (Phase    => UPDATE_MODEL_VALUES,
                                 Instance => new Lifecycles.Update.Update_Controller);

      Controller.Set_Controller (Phase    => INVOKE_APPLICATION,
                                 Instance => new Lifecycles.Invoke.Invoke_Controller);

      Controller.Set_Controller (Phase    => RENDER_RESPONSE,
                                 Instance => new Lifecycles.Response.Response_Controller);
   end Create_Phase_Controllers;

   --  ------------------------------
   --  Initialize the the lifecycle handler.
   --  ------------------------------
   overriding
   procedure Initialize (Controller : in out Lifecycle;
                         Views      : ASF.Applications.Views.View_Handler_Access) is
   begin
      Lifecycle'Class (Controller).Create_Phase_Controllers;
      for Phase in Controller.Controllers'Range loop
         Controller.Controllers (Phase).Initialize (Views);
      end loop;
   end Initialize;

end ASF.Lifecycles.Default;
