-----------------------------------------------------------------------
--  security-filters -- Security filter
--  Copyright (C) 2011 Stephane Carrez
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

with Ada.Strings.Unbounded;

with Util.Log.Loggers;

with ASF.Cookies;
with ASF.Applications.Main;

with Security.Contexts;
package body Security.Filters is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("Security.Filters");

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   procedure Initialize (Server  : in out Auth_Filter;
                         Context : in ASF.Servlets.Servlet_Registry'Class) is
      use ASF.Applications.Main;
   begin
      if Context in Application'Class then
         Server.Set_Permission_Manager (Application'Class (Context).Get_Permission_Manager);
      end if;
   end Initialize;

   --  ------------------------------
   --  Set the permission manager that must be used to verify the permission.
   --  ------------------------------
   procedure Set_Permission_Manager (Filter  : in out Auth_Filter;
                                     Manager : in Permissions.Permission_Manager_Access) is
   begin
      Filter.Manager := Manager;
   end Set_Permission_Manager;

   --  ------------------------------
   --  Filter the request to make sure the user is authenticated.
   --  Invokes the <b>Do_Login</b> procedure if there is no user.
   --  If a permission manager is defined, check that the user has the permission
   --  to view the page.  Invokes the <b>Do_Deny</b> procedure if the permission
   --  is denied.
   --  ------------------------------
   procedure Do_Filter (F        : in Auth_Filter;
                        Request  : in out ASF.Requests.Request'Class;
                        Response : in out ASF.Responses.Response'Class;
                        Chain    : in out ASF.Servlets.Filter_Chain) is
      use Ada.Strings.Unbounded;
      use Security.Permissions;
      use type ASF.Principals.Principal_Access;

      Session : ASF.Sessions.Session;
      SID     : Unbounded_String;
      AID     : Unbounded_String;
      Auth    : ASF.Principals.Principal_Access;

      procedure Fetch_Cookie (Cookie : in ASF.Cookies.Cookie);

      --  ------------------------------
      --  Collect the AID and SID cookies.
      --  ------------------------------
      procedure Fetch_Cookie (Cookie : in ASF.Cookies.Cookie) is
         Name : constant String := ASF.Cookies.Get_Name (Cookie);
      begin
         if Name = SID_COOKIE then
            SID := To_Unbounded_String (ASF.Cookies.Get_Value (Cookie));
         elsif Name = AID_COOKIE then
            AID := To_Unbounded_String (ASF.Cookies.Get_Value (Cookie));
         end if;
      end Fetch_Cookie;

      Context : aliased Security.Contexts.Security_Context;
   begin
      Request.Iterate_Cookies (Fetch_Cookie'Access);

      Session := Request.Get_Session (Create => True);

      --  If the session does not have a principal, try to authenticate the user with
      --  the auto-login cookie.
      Auth := Session.Get_Principal;
      if Auth = null then
         Auth_Filter'Class (F).Authenticate (Request, Response, Session, To_String (AID), Auth);
         if Auth /= null then
            Session.Set_Principal (Auth);
         end if;
      end if;

      --  No principal, redirect to the login page.
      if Auth = null then
         Auth_Filter'Class (F).Do_Login (Request, Response);
         return;
      end if;

      --  A permission manager is installed, check that the user can display the page.
      if F.Manager /= null then
         Context.Set_Context (F.Manager, Auth.all'Access);
         declare
            URI  : constant String := Request.Get_Path_Info;
            Perm : constant URI_Permission (URI'Length)
              := URI_Permission '(Len => URI'Length, URI => URI);
         begin
            if not F.Manager.Has_Permission (Context'Unchecked_Access, Perm) then
               Log.Info ("Deny access on {0}", URI);
               Auth_Filter'Class (F).Do_Deny (Request, Response);
               return;
            end if;
         end;
      end if;

      --  Request is authorized, proceed to the next filter.
      ASF.Servlets.Do_Filter (Chain    => Chain,
                              Request  => Request,
                              Response => Response);
   end Do_Filter;

   --  ------------------------------
   --  Display or redirects the user to the login page.  This procedure is called when
   --  the user is not authenticated.
   --  ------------------------------
   procedure Do_Login (F        : in Auth_Filter;
                       Request  : in out ASF.Requests.Request'Class;
                       Response : in out ASF.Responses.Response'Class) is
   begin
      null;
   end Do_Login;

   --  ------------------------------
   --  Display the forbidden access page.  This procedure is called when the user is not
   --  authorized to see the page.  The default implementation returns the SC_FORBIDDEN error.
   --  ------------------------------
   procedure Do_Deny (F        : in Auth_Filter;
                      Request  : in out ASF.Requests.Request'Class;
                      Response : in out ASF.Responses.Response'Class) is
      pragma Unreferenced (F, Request);
   begin
      Response.Set_Status (ASF.Responses.SC_FORBIDDEN);
   end Do_Deny;

   --  ------------------------------
   --  Authenticate a user by using the auto-login cookie.  This procedure is called if the
   --  current session does not have any principal.  Based on the request and the optional
   --  auto-login cookie passed in <b>Auth_Id</b>, it should identify the user and return
   --  a principal object.  The principal object will be freed when the session is closed.
   --  If the user cannot be authenticated, the returned principal should be null.
   --
   --  The default implementation returns a null principal.
   --  ------------------------------
   procedure Authenticate (F        : in Auth_Filter;
                           Request  : in out ASF.Requests.Request'Class;
                           Response : in out ASF.Responses.Response'Class;
                           Session  : in ASF.Sessions.Session;
                           Auth_Id  : in String;
                           Principal : out ASF.Principals.Principal_Access) is
      pragma Unreferenced (F, Request, Response, Session, Auth_Id);
   begin
      Principal := null;
   end Authenticate;

end Security.Filters;
