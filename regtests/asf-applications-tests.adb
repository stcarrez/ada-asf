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
with ASF.Events.Actions;
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

      Caller.Add_Test (Suite, "Test service HTTP POST+INVOKE_APPLICATION",
                       Test_Form_Post'Access);

      Caller.Add_Test (Suite, "Test service HTTP POST+PROCESS_VALIDATION",
                       Test_Form_Post_Validation_Error'Access);
   end Add_Tests;

   package Save_Binding is
     new ASF.Events.Actions.Action_Method.Bind (Bean   => Form_Bean,
                                                Method => Save,
                                                Name   => "save");

   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Save_Binding.Proxy'Access,
         Save_Binding.Proxy'Access);

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Form_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "email" then
         return Util.Beans.Objects.To_Object (From.Email);
      elsif Name = "password" then
         return Util.Beans.Objects.To_Object (From.Password);
      elsif Name = "name" then
         return Util.Beans.Objects.To_Object (From.Name);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Form_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "email" then
         From.Email := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "password" then
         From.Password := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "name" then
         From.Name := Util.Beans.Objects.To_Unbounded_String (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Form_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Binding_Array'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Initialize the test application
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);

      use type ASF.Applications.Main.Application_Access;
   begin
      if ASF.Tests.Get_Application = null then
         ASF.Tests.Initialize (Util.Tests.Get_Properties);
      end if;
   end Set_Up;

   --  ------------------------------
   --  Action to authenticate a user (password authentication).
   --  ------------------------------
   procedure Save (Data    : in out Form_Bean;
                   Outcome : in out Unbounded_String) is
   begin
      Outcome := To_Unbounded_String ("success");


   end Save;

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

   --  ------------------------------
   --  Test a GET+POST request with submitted values and an action method called on the bean.
   --  ------------------------------
   procedure Test_Form_Post (T : in out Test) is
      use Util.Beans.Objects;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Form    : aliased Form_Bean;
   begin
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Get (Request, Reply, "/tests/form-text.html", "form-text.txt");

      Assert_Matches (T, ".*<label for=.name.>Name</label>.*", Reply, "Wrong form content");
      Assert_Matches (T, ".*<input type=.text. name=.name. value=.. id=.name.*",
                      Reply, "Wrong form content");

      Request.Set_Parameter ("formText", "1");
      Request.Set_Parameter ("name", "John");
      Request.Set_Parameter ("password", "12345");
      Request.Set_Parameter ("email", "john@gmail.com");
      Do_Post (Request, Reply, "/tests/form-text.html", "form-text-post.txt");

      Assert_Matches (T, ".*<input type=.text. name=.name. value=.John. id=.name.*",
                      Reply, "Wrong form content");

      Assert_Equals (T, "John", Form.Name, "Form name not saved in the bean");
      Assert_Equals (T, "12345", Form.Password, "Form password not saved in the bean");
      Assert_Equals (T, "john@gmail.com", Form.Email, "Form email not saved in the bean");

   end Test_Form_Post;

   --  ------------------------------
   --  Test a POST request with an invalid submitted value
   --  ------------------------------
   procedure Test_Form_Post_Validation_Error (T : in out Test) is
      use Util.Beans.Objects;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Form    : aliased Form_Bean;
   begin
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));

      --  Post with password too short and empty email
      Request.Set_Parameter ("formText", "1");
      Request.Set_Parameter ("name", "John");
      Request.Set_Parameter ("password", "12");
      Request.Set_Parameter ("email", "");
      Do_Post (Request, Reply, "/tests/form-text.html", "form-text-post-1.txt");

      Assert_Matches (T, ".*<input type=.text. name=.name. value=.John. id=.name.*",
                      Reply, "Wrong form content");

      Assert_Matches (T, ".*Password: Validation Error: Length is less than "
                      & "allowable minimum of '4'.*",
                      Reply, "Missing error message");

      --  No field should be modified.
      Assert_Equals (T, "", Form.Name, "Form name was saved in the bean");
      Assert_Equals (T, "", Form.Password, "Form password was saved in the bean");
      Assert_Equals (T, "", Form.Email, "Form email was saved in the bean");

      Request.Set_Parameter ("email", "1");
      Request.Set_Parameter ("password", "12333");
      Request.Set_Parameter ("name", "122222222222222222222222222222222222222222");
      Do_Post (Request, Reply, "/tests/form-text.html", "form-text-post-2.txt");

   end Test_Form_Post_Validation_Error;

end ASF.Applications.Tests;
