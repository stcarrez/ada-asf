-----------------------------------------------------------------------
--  security-openid-servlets - Servlets for OpenID 2.0 Authentication
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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

with ASF.Sessions;

with Util.Beans.Objects;
with Util.Beans.Objects.Records;
with Util.Log.Loggers;
package body ASF.Security.Servlets is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("Security.Openid.Servlets");

   --  Make a package to store the Association in the session.
   package Association_Bean is new Util.Beans.Objects.Records (Openid.Association);

   subtype Association_Access is Association_Bean.Element_Type_Access;

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   procedure Initialize (Server  : in out Openid_Servlet;
                         Context : in ASF.Servlets.Servlet_Registry'Class) is
   begin
      null;
   end Initialize;

   --  Property name that specifies the OpenID callback URL.
   OPENID_VERIFY_URL      : constant String := "openid.callback_url";

   --  Property name that specifies the realm.
   OPENID_REALM           : constant String := "openid.realm";

   --  Name of the session attribute which holds information about the active authentication.
   OPENID_ASSOC_ATTRIBUTE : constant String := "openid-assoc";

   procedure Initialize (Server  : in Openid_Servlet;
                         Manager : in out OpenID.Manager) is
      Ctx          : constant ASF.Servlets.Servlet_Registry_Access := Server.Get_Servlet_Context;
      Callback_URI : constant String := Ctx.Get_Init_Parameter (OPENID_VERIFY_URL);
      Realm        : constant String := Ctx.Get_Init_Parameter (OPENID_REALM);
   begin
      Manager.Initialize (Return_To => Callback_URI,
                          Name      => Realm);
   end Initialize;

   function Get_Provider_URL (Server   : in Request_Auth_Servlet;
                              Request  : in ASF.Requests.Request'Class) return String is
      Ctx  : constant ASF.Servlets.Servlet_Registry_Access := Server.Get_Servlet_Context;
      URI  : constant String := Request.Get_Path_Info;
   begin
      if URI'Length = 0 then
         return "";
      end if;
      Log.Info ("OpenID authentication with {0}", URI);
      return Ctx.Get_Init_Parameter ("openid.provider." & URI (URI'First + 1 .. URI'Last));
   end Get_Provider_URL;

   --  ------------------------------
   --  Proceed to the OpenID authentication with an OpenID provider.
   --  Find the OpenID provider URL and starts the discovery, association phases
   --  during which a private key is obtained from the OpenID provider.
   --  After OpenID discovery and association, the user will be redirected to
   --  the OpenID provider.
   --  ------------------------------
   procedure Do_Get (Server   : in Request_Auth_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class) is

      Provider : constant String := Server.Get_Provider_URL (Request);
   begin
      Log.Info ("Request OpenId authentication to {0}", Provider);

      if Provider'Length = 0 then
         Response.Set_Status (ASF.Responses.SC_NOT_FOUND);
         return;
      end if;

      declare
         Mgr   : OpenID.Manager;
         OP    : OpenID.End_Point;
         Bean  : constant Util.Beans.Objects.Object := Association_Bean.Create;
         Assoc : constant Association_Access := Association_Bean.To_Element_Access (Bean);
      begin
         Server.Initialize (Mgr);

         --  Yadis discovery (get the XRDS file).
         Mgr.Discover (Provider, OP);

         --  Associate to the OpenID provider and get an end-point with a key.
         Mgr.Associate (OP, Assoc.all);

         --  Save the association in the HTTP session and
         --  redirect the user to the OpenID provider.
         declare
            Auth_URL : constant String := Mgr.Get_Authentication_URL (OP, Assoc.all);
            Session  : ASF.Sessions.Session := Request.Get_Session (Create => True);
         begin
            Log.Info ("Redirect to auth URL: {0}", Auth_URL);

            Response.Send_Redirect (Location => Auth_URL);
            Session.Set_Attribute (Name  => OPENID_ASSOC_ATTRIBUTE,
                                   Value => Bean);
         end;
      end;
   end Do_Get;

   --  ------------------------------
   --  Verify the authentication result that was returned by the OpenID provider.
   --  If the authentication succeeded and the signature was correct, sets a
   --  user principals on the session.
   --  ------------------------------
   procedure Do_Get (Server   : in Verify_Auth_Servlet;
                     Request  : in out ASF.Requests.Request'Class;
                     Response : in out ASF.Responses.Response'Class) is
      use type OpenID.Auth_Result;

      type Auth_Params is new OpenID.Parameters with null record;

      overriding
      function Get_Parameter (Params : in Auth_Params;
                              Name   : in String) return String;

      overriding
      function Get_Parameter (Params : in Auth_Params;
                              Name   : in String) return String is
         pragma Unreferenced (Params);
      begin
         return Request.Get_Parameter (Name);
      end Get_Parameter;

      Session : ASF.Sessions.Session := Request.Get_Session (Create => False);
      Bean    : Util.Beans.Objects.Object;
      Mgr     : OpenID.Manager;
      Assoc   : Association_Access;
      Auth    : OpenID.Authentication;
      Params  : Auth_Params;
      Ctx     : constant ASF.Servlets.Servlet_Registry_Access := Server.Get_Servlet_Context;
   begin
      Log.Info ("Verify openid authentication");

      if not Session.Is_Valid then
         Log.Warn ("Session has expired during OpenID authentication process");
         Response.Set_Status (ASF.Responses.SC_FORBIDDEN);
         return;
      end if;

      Bean := Session.Get_Attribute (OPENID_ASSOC_ATTRIBUTE);

      --  Cleanup the session and drop the association end point.
      Session.Remove_Attribute (OPENID_ASSOC_ATTRIBUTE);
      if Util.Beans.Objects.Is_Null (Bean) then
         Log.Warn ("Verify openid request without active session");
         Response.Set_Status (ASF.Responses.SC_FORBIDDEN);
         return;
      end if;

      Assoc := Association_Bean.To_Element_Access (Bean);
      Server.Initialize (Mgr);

      --  Verify that what we receive through the callback matches the association key.
      Mgr.Verify (Assoc.all, Params, Auth);
      if OpenID.Get_Status (Auth) /= OpenID.AUTHENTICATED then
         Log.Info ("Authentication has failed");
         Response.Set_Status (ASF.Responses.SC_FORBIDDEN);
         return;
      end if;

      Log.Info ("Authentication succeeded for {0}", OpenID.Get_Email (Auth));

      --  Get a user principal and set it on the session.
      declare
         User : ASF.Principals.Principal_Access;
         URL  : constant String := Ctx.Get_Init_Parameter ("openid.success_url");
      begin
         Verify_Auth_Servlet'Class (Server).Create_Principal (Auth, User);
         Session.Set_Principal (User);

         Log.Info ("Redirect user to success URL: {0}", URL);
         Response.Send_Redirect (Location => URL);
      end;
   end Do_Get;

   --  ------------------------------
   --  Create a principal object that correspond to the authenticated user identified
   --  by the <b>Auth</b> information.  The principal will be attached to the session
   --  and will be destroyed when the session is closed.
   --  ------------------------------
   procedure Create_Principal (Server : in Verify_Auth_Servlet;
                               Auth   : in OpenID.Authentication;
                               Result : out ASF.Principals.Principal_Access) is
      pragma Unreferenced (Server);

      P : constant OpenID.Principal_Access := OpenID.Create_Principal (Auth);
   begin
      Result := P.all'Access;
   end Create_Principal;

end ASF.Security.Servlets;
