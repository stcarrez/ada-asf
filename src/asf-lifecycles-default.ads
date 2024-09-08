-----------------------------------------------------------------------
--  asf-lifecycles-default -- Default Lifecycle handler
--  Copyright (C) 2010, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package ASF.Lifecycles.Default is

   type Lifecycle is new ASF.Lifecycles.Lifecycle with null record;

   --  Creates the phase controllers by invoking the <b>Set_Controller</b>
   --  procedure for each phase.  This is called by <b>Initialize</b> to build
   --  the lifecycle handler.
   overriding
   procedure Create_Phase_Controllers (Controller : in out Lifecycle);

   --  Initialize the the lifecycle handler.
   overriding
   procedure Initialize (Controller : in out Lifecycle;
                         Views      : ASF.Applications.Views.View_Handler_Access);

end ASF.Lifecycles.Default;
