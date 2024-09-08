-----------------------------------------------------------------------
--  security-filters-oauth -- OAuth Security filter
--  Copyright (C) 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Servlet.Security.Filters.OAuth;

--  The <b>ASF.Security.Filters.OAuth</b> package provides a servlet filter that
--  implements the RFC 6749 "Accessing Protected Resources" part: it extracts the OAuth
--  access token, verifies the grant and the permission.  The servlet filter implements
--  the RFC 6750 "OAuth 2.0 Bearer Token Usage".
--
package ASF.Security.Filters.OAuth renames Servlet.Security.Filters.OAuth;
