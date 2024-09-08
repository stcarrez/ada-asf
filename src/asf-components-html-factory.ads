-----------------------------------------------------------------------
--  html-factory -- Factory for HTML UI Components
--  Copyright (C) 2009, 2010, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Factory;
package ASF.Components.Html.Factory is

   --  Register the HTML component factory.
   procedure Register (Factory : in out ASF.Factory.Component_Factory);

end ASF.Components.Html.Factory;
