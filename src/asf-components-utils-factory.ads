-----------------------------------------------------------------------
--  util-factory -- Factory for UI Util Components
--  Copyright (C) 2009, 2010, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with EL.Functions;
with ASF.Factory;

package ASF.Components.Utils.Factory is

   --  Register the Util component factory.
   procedure Register (Factory : in out ASF.Factory.Component_Factory);

   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class);

end ASF.Components.Utils.Factory;
