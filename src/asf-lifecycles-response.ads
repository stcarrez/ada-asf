-----------------------------------------------------------------------
--  asf-lifecycles-response -- Response phase
--  Copyright (C) 2010, 2018 Stephane Carrez
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

--  The <b>ASF.Lifecycles.Response</b> package defines the behavior
--  of the response phase.
with ASF.Applications.Views;
package ASF.Lifecycles.Response is

   --  ------------------------------
   --  Response controller
   --  ------------------------------
   type Response_Controller is new Phase_Controller with private;

   --  Initialize the phase controller.
   overriding
   procedure Initialize (Controller : in out Response_Controller;
                         Views      : ASF.Applications.Views.View_Handler_Access);

   --  Execute the restore view phase.
   overriding
   procedure Execute (Controller : in Response_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class);

private

   type Response_Controller is new Phase_Controller with record
      View_Handler : ASF.Applications.Views.View_Handler_Access;
   end record;

end ASF.Lifecycles.Response;
