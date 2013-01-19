-----------------------------------------------------------------------
--  asf-security-tests - Unit tests for ASF.Security
--  Copyright (C) 2013 Stephane Carrez
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

with Util.Test_Caller;

with Security.Policies; use Security;
with Security.Policies.URLs;

with ASF.Servlets;
with ASF.Servlets.Tests;
with ASF.Security.Filters;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
package body ASF.Security.Tests is

   use Util.Tests;

   --  ------------------------------
   --  Check that the given URI reports the HTTP status.
   --  ------------------------------
   procedure Check_Security (T      : in out Test;
                             URI    : in String;
                             Result : in Natural) is
      Ctx : ASF.Servlets.Servlet_Registry;

      S1  : aliased ASF.Servlets.Tests.Test_Servlet1;
      F1  : aliased ASF.Security.Filters.Auth_Filter;
      Sec : aliased Policies.Policy_Manager (Max_Policies => 10);
   begin
      F1.Set_Permission_Manager (Sec'Unchecked_Access);
      Ctx.Add_Servlet ("Faces", S1'Unchecked_Access);
      Ctx.Add_Filter ("Security", F1'Unchecked_Access);

      Sec.Add_Policy (new Policies.URLs.URL_Policy);
      Sec.Read_Policy (Util.Tests.Get_Path ("regtests/files/permissions/simple-policy.xml"));
      Ctx.Add_Mapping (Pattern => "*.jsf", Name => "Faces");
      Ctx.Add_Filter_Mapping (Pattern => "*.jsf", Name => "Security");

      declare
         Dispatcher : constant ASF.Servlets.Request_Dispatcher
           := Ctx.Get_Request_Dispatcher (Path => URI & ".jsf");
         Req        : ASF.Requests.Mockup.Request;
         Resp       : ASF.Responses.Mockup.Response;
      begin
         Req.Set_Request_URI ("/admin/test");
         Req.Set_Method ("GET");
         ASF.Servlets.Forward (Dispatcher, Req, Resp);

         Assert_Equals (T, Result, Resp.Get_Status, "Invalid status");
      end;
   end Check_Security;

   --  ------------------------------
   --  Test the security filter granting permission for a given URI.
   --  ------------------------------
   procedure Test_Security_Filter (T : in out Test) is
   begin
      T.Check_Security ("/admin/test", ASF.Responses.SC_UNAUTHORIZED);
   end Test_Security_Filter;

   --  ------------------------------
   --  Test the security filter grants access to anonymous allowed pages.
   --  ------------------------------
   procedure Test_Anonymous_Access (T : in out Test) is
   begin
      T.Check_Security ("/view", ASF.Responses.SC_OK);
   end Test_Anonymous_Access;

   package Caller is new Util.Test_Caller (Test, "Security");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ASF.Security.Filters.Auth_Filter (deny)",
                       Test_Security_Filter'Access);
      Caller.Add_Test (Suite, "Test ASF.Security.Filters.Auth_Filter (grant)",
                       Test_Anonymous_Access'Access);
   end Add_Tests;

end ASF.Security.Tests;
