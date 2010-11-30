-----------------------------------------------------------------------
--  asf-lifecycles-response -- Response phase
--  Copyright (C) 2010 Stephane Carrez
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
with Ada.Exceptions;
with ASF.Applications.Main;
with ASF.Applications.Views;
with ASF.Components.Root;
with Util.Log.Loggers;

--  The <b>ASF.Lifecycles.Response</b> package defines the behavior
--  of the response phase.
package body ASF.Lifecycles.Response is

   use Ada.Exceptions;
   use Util.Log;
   use ASF.Applications;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Lifecycles.Response");

   --  ------------------------------
   --  Initialize the phase controller.
   --  ------------------------------
   overriding
   procedure Initialize (Controller : in out Response_Controller;
                         App        : access ASF.Applications.Main.Application'Class) is
   begin
      Controller.View_Handler := App.Get_View_Handler;
   end Initialize;

   --  ------------------------------
   --  Execute the restore view phase.
   --  ------------------------------
   overriding
   procedure Execute (Controller : in Response_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (Controller);

      View    : constant Components.Root.UIViewRoot := Context.Get_View_Root;
   begin
      Controller.View_Handler.Render_View (Context, View);

   exception
      when E: others =>
         Log.Error ("Error when displaying view {0}: {1}: {2}", "?",
                    Exception_Name (E), Exception_Message (E));
         raise;
   end Execute;

end ASF.Lifecycles.Response;
