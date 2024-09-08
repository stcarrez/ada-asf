-----------------------------------------------------------------------
--  asf-principals -- Component and tag factory
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Security;
package ASF.Principals is

   --  ------------------------------
   --  Principal
   --  ------------------------------
   subtype Principal is Security.Principal;
   subtype Principal_Access is Security.Principal_Access;

end ASF.Principals;
