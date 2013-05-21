-----------------------------------------------------------------------
--  security-openid-servlets - Servlets for OpenID 2.0 Authentication
--  Copyright (C) 2010, 2011, 2012, 2013 Stephane Carrez
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

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Security.Servlets");

   --  Make a package to store the Association in the session.
   package Association_Bean is new Util.Beans.Objects.Records (Auth.Association);

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

   --  Name of the session attribute which holds information about the active authentication.
   OPENID_ASSOC_ATTRIBUTE : constant String := "openid-assoc";

   procedure Initialize (Server   : in Openid_Servlet;
                         Provider : in String;
                         Manager  : in out Auth.Manager) is
   begin
      Manager.Initialize (Params  => Server,
                          Name    => Provider);
   end Initialize;

   --  Get a configuration parameter from the servlet context for the security Auth provider.
   overriding
   function Get_Parameter (Server : in Openid_Servlet;
                           Name   : in String) return String is
      Ctx          : constant ASF.Servlets.Servlet_Registry_Access := Server.Get_Servlet_Context;
   begin
      return Ctx.Get_Init_Parameter (Name);
   end Get_Parameter;

   function Get_Provider_URL (Server   : in Request_Auth_Servlet;
                              Request  : in ASF.Requests.Request'Class) return String is
      pragma Unreferenced (Server);

      URI  : constant String := Request.Get_Path_Info;
   begin
      if URI'Length = 0 then
         return "";
      end if;
      Log.Info ("OpenID authentication with {0}", URI);
      return URI (URI'First + 1 .. URI'Last);
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

      Ctx      : constant ASF.Servlets.Servlet_Registry_Access := Server.Get_Servlet_Context;
      Name     : constant String := Server.Get_Provider_URL (Request);
      URL      : constant String := Ctx.Get_Init_Parameter ("auth.url." & Name);
   begin
      Log.Info ("Request OpenId authentication to {0} - {1}", Name, URL);

      if Name'Length = 0 or URL'Length = 0 then
         Response.Set_Status (ASF.Responses.SC_NOT_FOUND);
         return;
      end if;

      declare
         Mgr   : Auth.Manager;
         OP    : Auth.End_Point;
         Bean  : constant Util.Beans.Objects.Object := Association_Bean.Create;
         Assoc : constant Association_Access := Association_Bean.To_Element_Access (Bean);
      begin
         Server.Initialize (Name, Mgr);

         --  Yadis discovery (get the XRDS file).  This step does nothing for OAuth.
         Mgr.Discover (URL, OP);

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
      use type Auth.Auth_Result;

      type Auth_Params is new Auth.Parameters with null record;

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

      Session    : ASF.Sessions.Session := Request.Get_Session (Create => False);
      Bean       : Util.Beans.Objects.Object;
      Mgr        : Auth.Manager;
      Assoc      : Association_Access;
      Credential : Auth.Authentication;
      Params     : Auth_Params;
      Ctx        : constant ASF.Servlets.Servlet_Registry_Access := Server.Get_Servlet_Context;
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
      Server.Initialize (Auth.Get_Provider (Assoc.all), Mgr);

      --  Verify that what we receive through the callback matches the association key.
      Mgr.Verify (Assoc.all, Params, Credential);
      if Auth.Get_Status (Credential) /= Auth.AUTHENTICATED then
         Log.Info ("Authentication has failed");
         Response.Set_Status (ASF.Responses.SC_FORBIDDEN);
         return;
      end if;

      Log.Info ("Authentication succeeded for {0}", Auth.Get_Email (Credential));

      --  Get a user principal and set it on the session.
      declare
         User : ASF.Principals.Principal_Access;
         URL  : constant String := Ctx.Get_Init_Parameter ("openid.success_url");
      begin
         Verify_Auth_Servlet'Class (Server).Create_Principal (Credential, User);
         Session.Set_Principal (User);

         Log.Info ("Redirect user to success URL: {0}", URL);
         Response.Send_Redirect (Location => URL);
      end;
   end Do_Get;

   --  ------------------------------
   --  Create a principal object that correspond to the authenticated user identified
   --  by the <b>Credential</b> information.  The principal will be attached to the session
   --  and will be destroyed when the session is closed.
   --  ------------------------------
   procedure Create_Principal (Server     : in Verify_Auth_Servlet;
                               Credential : in Auth.Authentication;
                               Result     : out ASF.Principals.Principal_Access) is
      pragma Unreferenced (Server);

      P : constant Auth.Principal_Access := Auth.Create_Principal (Credential);
   begin
      Result := P.all'Access;
   end Create_Principal;

end ASF.Security.Servlets;
