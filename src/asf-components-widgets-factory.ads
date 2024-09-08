-----------------------------------------------------------------------
--  widgets-factory -- Factory for widget Components
--  Copyright (C) 2013, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Factory;

package ASF.Components.Widgets.Factory is

   --  Register the widget component factory.
   procedure Register (Factory : in out ASF.Factory.Component_Factory);

end ASF.Components.Widgets.Factory;
