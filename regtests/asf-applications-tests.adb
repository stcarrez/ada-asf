-----------------------------------------------------------------------
--  asf-applications-tests -  ASF Application tests using ASFUnit
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

with Util.Tests;
with Util.Test_Caller;

with ASF.Tests;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Applications.Main;
package body ASF.Applications.Tests is

   use ASF.Tests;
   use Util.Tests;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test service HTTP invalid method",
                       Test_Invalid_Request'Access);
      Caller.Add_Test (Suite, "Test service HTTP GET (File_Servlet)",
                       Test_Get_File'Access);
      Caller.Add_Test (Suite, "Test service HTTP GET 404",
                       Test_Get_404'Access);

      Caller.Add_Test (Suite, "Test service HTTP GET (Measure_Servlet)",
                       Test_Get_Measures'Access);
   end Add_Tests;

   --  ------------------------------
   --  Initialize the test application
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out Test) is
      use type ASF.Applications.Main.Application_Access;
   begin
      if ASF.Tests.Get_Application = null then
         ASF.Tests.Initialize (Util.Tests.Get_Properties);
      end if;
   end Set_Up;

   --  ------------------------------
   --  Test an invalid HTTP request.
   --  ------------------------------
   procedure Test_Invalid_Request (T : in out Test) is
      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
   begin
      --  Unknown method
      Request.Set_Method ("BLAB");
      Request.Set_Request_URI ("/asfunit/views/set.xhtml");
      Do_Req (Request, Reply);
      Assert_Equals (T, ASF.Responses.SC_NOT_IMPLEMENTED, Reply.Get_Status, "Invalid response");

      --  PUT on a file is not allowed
      Request.Set_Method ("PUT");
      Request.Set_Request_URI ("/asfunit/views/set.xhtml");
      Do_Req (Request, Reply);
      Assert_Equals (T, ASF.Responses.SC_METHOD_NOT_ALLOWED, Reply.Get_Status, "Invalid response");

      --  POST on a file is not allowed
      Request.Set_Method ("POST");
      Request.Set_Request_URI ("/asfunit/views/set.xhtml");
      Do_Req (Request, Reply);
      Assert_Equals (T, ASF.Responses.SC_METHOD_NOT_ALLOWED, Reply.Get_Status, "Invalid response");

      --  DELETE on a file is not allowed
      Request.Set_Method ("DELETE");
      Request.Set_Request_URI ("/asfunit/views/set.xhtml");
      Do_Req (Request, Reply);
      Assert_Equals (T, ASF.Responses.SC_METHOD_NOT_ALLOWED, Reply.Get_Status, "Invalid response");
   end Test_Invalid_Request;

   --  ------------------------------
   --  Test a GET request on a static file served by the File_Servlet.
   --  ------------------------------
   procedure Test_Get_404 (T : in out Test) is
      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
   begin
      Do_Get (Request, Reply, "/file-does-not-exist.txt", "test-404.html");
      Assert_Equals (T, ASF.Responses.SC_NOT_FOUND, Reply.Get_Status, "Invalid response");
   end Test_Get_404;

   --  ------------------------------
   --  Test a GET request on a static file served by the File_Servlet.
   --  ------------------------------
   procedure Test_Get_File (T : in out Test) is
      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
   begin
      Do_Get (Request, Reply, "/views/set.xhtml", "get-file-set.txt");
      Assert_Contains (T, "<c:set var=""user"" value=""John Smith""/>", Reply, "Wrong content");

      Do_Get (Request, Reply, "/views/set.html", "get-file-set.html");
      Assert_Matches (T, "^\s*John Smith\s?$", Reply, "Wrong content");
   end Test_Get_File;

   --  ------------------------------
   --  Test a GET request on the measure servlet
   --  ------------------------------
   procedure Test_Get_Measures (T : in out Test) is
      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
   begin
      Do_Get (Request, Reply, "/stats.xml", "stats.xml");

      --  We must get at least one measure value (assuming the Test_Get_File test
      --  was executed).
      Assert_Matches (T, "<time count=""\d+"" time=""\d+.\d+[um]s"" title="".*""/>",
                      Reply, "Wrong content");
   end Test_Get_Measures;

end ASF.Applications.Tests;
