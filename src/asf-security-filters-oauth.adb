-----------------------------------------------------------------------
--  security-filters-oauth -- OAuth Security filter
--  Copyright (C) 2017 Stephane Carrez
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

with Util.Log.Loggers;

with ASF.Applications.Main;

with Security.Contexts;
with Security.Policies.URLs;
package body ASF.Security.Filters.OAuth is

   use Ada.Strings.Unbounded;
   use Servers;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Security.Filters.OAuth");

   function Get_Access_Token (Request : in ASF.Requests.Request'Class) return String;

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   procedure Initialize (Server  : in out Auth_Filter;
                         Config  : in ASF.Servlets.Filter_Config) is
      use ASF.Applications;

      Context : constant Servlets.Servlet_Registry_Access := Servlets.Get_Servlet_Context (Config);
   begin
      if Context.all in Main.Application'Class then
         Server.Set_Permission_Manager (Main.Application'Class (Context.all).Get_Security_Manager);
      end if;
   end Initialize;

   --  ------------------------------
   --  Set the permission manager that must be used to verify the permission.
   --  ------------------------------
   procedure Set_Permission_Manager (Filter  : in out Auth_Filter;
                                     Manager : in Policies.Policy_Manager_Access) is
   begin
      Filter.Manager := Manager;
   end Set_Permission_Manager;

   function Get_Access_Token (Request : in ASF.Requests.Request'Class) return String is
      Header : constant String := Request.Get_Header (AUTHORIZATION_HEADER_NAME);
   begin
      if Header'Length > 0 then
         return Header;
      end if;
      return Header;
   end Get_Access_Token;

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
      use Policies.URLs;
      use type Policies.Policy_Manager_Access;

      Servlet : constant String := Request.Get_Servlet_Path;
      URL     : constant String := Servlet & Request.Get_Path_Info;
   begin
      if F.Realm = null then
         Log.Error ("Deny access on {0} due to missing realm", URL);
         Auth_Filter'Class (F).Do_Deny (Request, Response);
         return;
      end if;
      declare
         Bearer  : constant String := Get_Access_Token (Request);
         Auth    : Principal_Access;
         Grant   : Servers.Grant_Type;
         Context : aliased Contexts.Security_Context;
      begin
         if Bearer'Length = 0 then
            Log.Info ("Ask authentication on {0} due to missing access token", URL);
            Auth_Filter'Class (F).Do_Login (Request, Response);
            return;
         end if;
         F.Realm.Authenticate (Bearer, Grant);
         if Grant.Status /= Valid_Grant then
            Log.Info ("Ask authentication on {0} due to missing access token", URL);
            Auth_Filter'Class (F).Do_Deny (Request, Response);
            return;
         end if;

         --  OAuth_Permission
         --  if not F.Manager.Has_Permission (Context, Perm) then
         --     -- deny
         --  Request.Set_Attribute ("application", Grant.Application);
         --  Request is authorized, proceed to the next filter.
         ASF.Servlets.Do_Filter (Chain    => Chain,
                                 Request  => Request,
                                 Response => Response);
      end;
   end Do_Filter;

   --  ------------------------------
   --  Display or redirects the user to the login page.  This procedure is called when
   --  the user is not authenticated.
   --  ------------------------------
   procedure Do_Login (F        : in Auth_Filter;
                       Request  : in out ASF.Requests.Request'Class;
                       Response : in out ASF.Responses.Response'Class) is
      pragma Unreferenced (F, Request);
   begin
      Response.Send_Error (ASF.Responses.SC_UNAUTHORIZED);
   end Do_Login;

   --  ------------------------------
   --  Display the forbidden access page.  This procedure is called when the user is not
   --  authorized to see the page.  The default implementation returns the SC_FORBIDDEN error.
   --  ------------------------------
   procedure Do_Deny (F        : in Auth_Filter;
                      Request  : in out ASF.Requests.Request'Class;
                      Response : in out ASF.Responses.Response'Class) is
      pragma Unreferenced (Request);
   begin
      Response.Add_Header (WWW_AUTHENTICATE_HEADER_NAME,
                           "Bearer realm=""" & To_String (F.Realm_URL)
                           & """, error=""invalid_token""");
      Response.Set_Status (ASF.Responses.SC_FORBIDDEN);
   end Do_Deny;

end ASF.Security.Filters.OAuth;
