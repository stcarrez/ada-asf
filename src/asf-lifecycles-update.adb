-----------------------------------------------------------------------
--  asf-lifecycles-update -- Update model phase
--  Copyright (C) 2010, 2011, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Exceptions;
with ASF.Components.Root;
with ASF.Components.Base;
with Util.Log.Loggers;

--  The <b>ASF.Lifecycles.Update</b> package defines the behavior
--  of the update model phase.
package body ASF.Lifecycles.Update is

   use Ada.Exceptions;
   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Lifecycles.Update");

   --  ------------------------------
   --  Execute the update model phase.
   --  ------------------------------
   overriding
   procedure Execute (Controller : in Update_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (Controller);

      View : constant Components.Root.UIViewRoot := Context.Get_View_Root;
      Root : constant access Components.Base.UIComponent'Class := Components.Root.Get_Root (View);
   begin
      Root.Process_Updates (Context);

   exception
      when E : others =>
         Log.Error ("Error when running the update model phase {0}: {1}: {2}", "?",
                    Exception_Name (E), Exception_Message (E));
         raise;
   end Execute;

end ASF.Lifecycles.Update;
