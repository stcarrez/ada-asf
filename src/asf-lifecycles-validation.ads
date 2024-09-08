-----------------------------------------------------------------------
--  asf-lifecycles-validation -- Validation phase
--  Copyright (C) 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  The <b>ASF.Lifecycles.Validation</b> package defines the behavior
--  of the validation phase.
package ASF.Lifecycles.Validation is

   --  ------------------------------
   --  Validation controller
   --  ------------------------------
   type Validation_Controller is new Phase_Controller with null record;

   --  Execute the validation phase.
   overriding
   procedure Execute (Controller : in Validation_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Lifecycles.Validation;
