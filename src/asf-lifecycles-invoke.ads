-----------------------------------------------------------------------
--  asf-lifecycles-invoke -- Invoke application phase
--  Copyright (C) 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  The <b>ASF.Lifecycles.Invoke</b> package defines the behavior
--  of the invoke application phase.
package ASF.Lifecycles.Invoke is

   --  ------------------------------
   --  Invoke application controller
   --  ------------------------------
   type Invoke_Controller is new Phase_Controller with null record;

   --  Execute the invoke application phase.
   overriding
   procedure Execute (Controller : in Invoke_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Lifecycles.Invoke;
