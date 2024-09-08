-----------------------------------------------------------------------
--  security-filters -- Security filter
--  Copyright (C) 2011, 2012, 2015, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Servlet.Security.Filters;

--  The <b>Security.Filters</b> package defines a servlet filter that can be activated
--  on requests to authenticate users and verify they have the permission to view
--  a page.
package ASF.Security.Filters is

   SID_COOKIE : constant String := "SID";

   AID_COOKIE : constant String := "AID";

   subtype Auth_Filter is Servlet.Security.Filters.Auth_Filter;

end ASF.Security.Filters;
