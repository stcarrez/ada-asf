-----------------------------------------------------------------------
--  security-filters -- Security filter
--  Copyright (C) 2011, 2012 Stephane Carrez
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

--  The <b>Security.Filters</b> package defines a servlet filter that can be activated
--  on requests to authenticate users and verify they have the permission to view
--  a page.
package ASF.Security.Filters is

   SID_COOKIE : constant String := "SID";

   AID_COOKIE : constant String := "AID";

   type Auth_Filter is new ASF.Filters.Filter with private;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   overriding
   procedure Initialize (Server  : in out Auth_Filter;
                         Context : in ASF.Servlets.Servlet_Registry'Class);

   --  Set the permission manager that must be used to verify the permission.
   procedure Set_Permission_Manager (Filter  : in out Auth_Filter;
                                     Manager : in Policies.Policy_Manager_Access);

   --  Filter the request to make sure the user is authenticated.
   --  Invokes the <b>Do_Login</b> procedure if there is no user.
   --  If a permission manager is defined, check that the user has the permission
   --  to view the page.  Invokes the <b>Do_Deny</b> procedure if the permission
   --  is denied.
   procedure Do_Filter (F        : in Auth_Filter;
                        Request  : in out ASF.Requests.Request'Class;
                        Response : in out ASF.Responses.Response'Class;
                        Chain    : in out ASF.Servlets.Filter_Chain);

   --  Display or redirects the user to the login page.  This procedure is called when
   --  the user is not authenticated.
   procedure Do_Login (F        : in Auth_Filter;
                       Request  : in out ASF.Requests.Request'Class;
                       Response : in out ASF.Responses.Response'Class);

   --  Display the forbidden access page.  This procedure is called when the user is not
   --  authorized to see the page.  The default implementation returns the SC_FORBIDDEN error.
   procedure Do_Deny (F        : in Auth_Filter;
                      Request  : in out ASF.Requests.Request'Class;
                      Response : in out ASF.Responses.Response'Class);

   --  Authenticate a user by using the auto-login cookie.  This procedure is called if the
   --  current session does not have any principal.  Based on the request and the optional
   --  auto-login cookie passed in <b>Auth_Id</b>, it should identify the user and return
   --  a principal object.  The principal object will be freed when the session is closed.
   --  If the user cannot be authenticated, the returned principal should be null.
   --
   --  The default implementation returns a null principal.
   procedure Authenticate (F        : in Auth_Filter;
                           Request  : in out ASF.Requests.Request'Class;
                           Response : in out ASF.Responses.Response'Class;
                           Session  : in ASF.Sessions.Session;
                           Auth_Id  : in String;
                           Principal : out ASF.Principals.Principal_Access);

private

   type Auth_Filter is new ASF.Filters.Filter with record
      Manager : Policies.Policy_Manager_Access := null;
   end record;

end ASF.Security.Filters;
