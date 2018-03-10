-----------------------------------------------------------------------
--  security-filters-oauth -- OAuth Security filter
--  Copyright (C) 2017, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Servlet.Security.Filters.OAuth;

--  The <b>ASF.Security.Filters.OAuth</b> package provides a servlet filter that
--  implements the RFC 6749 "Accessing Protected Resources" part: it extracts the OAuth
--  access token, verifies the grant and the permission.  The servlet filter implements
--  the RFC 6750 "OAuth 2.0 Bearer Token Usage".
--
package ASF.Security.Filters.OAuth renames Servlet.Security.Filters.OAuth;
