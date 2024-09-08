-----------------------------------------------------------------------
--  core-factory -- Factory for UI Core Components
--  Copyright (C) 2009, 2010, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Factory;
package ASF.Components.Core.Factory is

   --  Register the Core component factory.
   procedure Register (Factory : in out ASF.Factory.Component_Factory);

end ASF.Components.Core.Factory;
