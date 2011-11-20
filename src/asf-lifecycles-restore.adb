-----------------------------------------------------------------------
--  asf-lifecycles-restore -- Restore view phase
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
with Ada.Exceptions;
with ASF.Applications.Main;
with ASF.Components.Root;
with ASF.Requests;
with Util.Log.Loggers;
package body ASF.Lifecycles.Restore is

   use Ada.Exceptions;
   use Util.Log;
   use ASF.Applications;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Lifecycles.Restore");

   --  ------------------------------
   --  Initialize the phase controller.
   --  ------------------------------
   overriding
   procedure Initialize (Controller : in out Restore_Controller;
                         App        : access ASF.Applications.Main.Application'Class) is
   begin
      Controller.View_Handler := App.Get_View_Handler;
   end Initialize;

   --  ------------------------------
   --  Execute the restore view phase.
   --  ------------------------------
   overriding
   procedure Execute (Controller : in Restore_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is

      Req     : constant ASF.Requests.Request_Access := Context.Get_Request;
      Page    : constant String := Req.Get_Path_Info;
      View    : Components.Root.UIViewRoot;
   begin
      Controller.View_Handler.Restore_View (Page, Context, View);

      Context.Set_View_Root (View);

      --  If this is not a postback, render the response immediately.
      if Req.Get_Method = "GET" then
         Context.Render_Response;
      end if;

   exception
      when E : others =>
         Log.Error ("Error when restoring view {0}: {1}: {2}", Page,
                    Exception_Name (E), Exception_Message (E));
         raise;
   end Execute;

end ASF.Lifecycles.Restore;
