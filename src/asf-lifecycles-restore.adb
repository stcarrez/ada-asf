-----------------------------------------------------------------------
--  asf-lifecycles-restore -- Restore view phase
--  Copyright (C) 2010, 2011, 2015, 2018, 2019 Stephane Carrez
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
with ASF.Components.Root;
with ASF.Requests;
with Util.Log.Loggers;
package body ASF.Lifecycles.Restore is

   use Ada.Exceptions;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Lifecycles.Restore");

   --  ------------------------------
   --  Initialize the phase controller.
   --  ------------------------------
   overriding
   procedure Initialize (Controller : in out Restore_Controller;
                         Views      : ASF.Applications.Views.View_Handler_Access) is
   begin
      Controller.View_Handler := Views;
   end Initialize;

   --  ------------------------------
   --  Execute the restore view phase.
   --  ------------------------------
   overriding
   procedure Execute (Controller : in Restore_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Req     : constant ASF.Requests.Request_Access := Context.Get_Request;
      Page    : constant String := Context.Get_View_Name;
      View    : Components.Root.UIViewRoot;
   begin
      Req.Inject_Parameters (Context.Get_ELContext.all);
      Controller.View_Handler.Restore_View (Page, Context, View);

      Context.Set_View_Root (View);

      --  If the view was not found, render the response immediately.
      --  The missing page will be handled by the lifecycle response handler.
      if Components.Root.Get_Root (View) = null then
         Context.Render_Response;

         --  If this is not a postback, check for view parameters.
      elsif Req.Get_Method = "GET" then
         --  We need to process the ASF lifecycle towards the meta data component tree.
         --  This allows some Ada beans to be initialized from the request parameters
         --  and have some component actions called on the http GET (See <f:viewActions)).
         Components.Root.Set_Meta (View);

         --  If the view has no meta data, render the response immediately.
         if not Components.Root.Has_Meta (View) then
            Context.Render_Response;
         end if;
      end if;

   exception
      when E : others =>
         Log.Error ("Error when restoring view {0}: {1}: {2}", Page,
                    Exception_Name (E), Exception_Message (E));
         raise;
   end Execute;

end ASF.Lifecycles.Restore;
