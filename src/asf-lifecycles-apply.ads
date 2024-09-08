-----------------------------------------------------------------------
--  asf-lifecycles-apply -- Apply values phase
--  Copyright (C) 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  The <b>ASF.Lifecycles.Apply</b> package defines the behavior
--  of the apply values phase.
package ASF.Lifecycles.Apply is

   --  ------------------------------
   --  Apply values controller
   --  ------------------------------
   type Apply_Controller is new Phase_Controller with null record;

   --  Execute the apply values phase.
   overriding
   procedure Execute (Controller : in Apply_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Lifecycles.Apply;
