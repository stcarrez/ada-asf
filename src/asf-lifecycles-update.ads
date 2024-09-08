-----------------------------------------------------------------------
--  asf-lifecycles-update -- Update model phase
--  Copyright (C) 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  The <b>ASF.Lifecycles.Update</b> package defines the behavior
--  of the update model phase.
package ASF.Lifecycles.Update is

   --  ------------------------------
   --  Update model controller
   --  ------------------------------
   type Update_Controller is new Phase_Controller with null record;

   --  Execute the update model phase.
   overriding
   procedure Execute (Controller : in Update_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Lifecycles.Update;
