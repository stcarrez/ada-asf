-----------------------------------------------------------------------
--  security-filters -- Security filter
--  Copyright (C) 2011, 2012, 2015 Stephane Carrez
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

with ASF.Filters;
with ASF.Requests;
with ASF.Responses;
with ASF.Servlets;
with ASF.Sessions;
with ASF.Principals;
with Security.Policies;  use Security;
with Servlet.Security.Filters;

--  The <b>Security.Filters</b> package defines a servlet filter that can be activated
--  on requests to authenticate users and verify they have the permission to view
--  a page.
package ASF.Security.Filters is

   SID_COOKIE : constant String := "SID";

   AID_COOKIE : constant String := "AID";

   subtype Auth_Filter is Servlet.Security.Filters.Auth_Filter;

end ASF.Security.Filters;
