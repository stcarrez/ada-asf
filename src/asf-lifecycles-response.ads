-----------------------------------------------------------------------
--  asf-lifecycles-response -- Response phase
--  Copyright (C) 2010, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
