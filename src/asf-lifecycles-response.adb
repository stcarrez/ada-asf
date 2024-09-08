-----------------------------------------------------------------------
--  asf-lifecycles-response -- Response phase
--  Copyright (C) 2010, 2011, 2012, 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Exceptions;
with ASF.Components.Root;
with ASF.Responses;
with Util.Log.Loggers;

--  The <b>ASF.Lifecycles.Response</b> package defines the behavior
--  of the response phase.
package body ASF.Lifecycles.Response is

   use Ada.Exceptions;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Lifecycles.Response");

   --  ------------------------------
   --  Initialize the phase controller.
   --  ------------------------------
   overriding
   procedure Initialize (Controller : in out Response_Controller;
                         Views      : ASF.Applications.Views.View_Handler_Access) is
   begin
      Controller.View_Handler := Views;
   end Initialize;

   --  ------------------------------
   --  Execute the restore view phase.
   --  ------------------------------
   overriding
   procedure Execute (Controller : in Response_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      View    : constant Components.Root.UIViewRoot := Context.Get_View_Root;
   begin
      if Components.Root.Get_Root (View) = null then
         Log.Warn ("Page not found: {0}", Context.Get_View_Name);
         Context.Get_Response.Send_Error (ASF.Responses.SC_NOT_FOUND);
         Context.Response_Completed;
      else
         Controller.View_Handler.Render_View (Context, View);
      end if;

   exception
      when E : others =>
         Log.Error ("Error when displaying view {0}: {1}: {2}", "?",
                    Exception_Name (E), Exception_Message (E));
         raise;
   end Execute;

end ASF.Lifecycles.Response;
