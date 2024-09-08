-----------------------------------------------------------------------
--  components-ajax-factory -- Factory for AJAX Components
--  Copyright (C) 2011, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Factory;
package ASF.Components.Ajax.Factory is

   --  Register the Ajax component factory.
   procedure Register (Factory : in out ASF.Factory.Component_Factory);

end ASF.Components.Ajax.Factory;
