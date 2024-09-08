-----------------------------------------------------------------------
--  countries - A simple bean example
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Beans.Basic;

package Countries is

   --  Get a select item list which contains a list of countries.
   function Create_Country_List return Util.Beans.Basic.Readonly_Bean_Access;

end Countries;
