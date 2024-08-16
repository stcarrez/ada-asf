-----------------------------------------------------------------------
--  asf-applications-tests -  ASF Application tests using ASFUnit
--  Copyright (C) 2011, 2012, 2015, 2017, 2021, 2022 Stephane Carrez
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

with Util.Log;
with Util.Test_Caller;

with ASF.Tests;
with ASF.Events.Faces.Actions;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Applications.Main;
with ASF.Applications.Main.Configs;
with ASF.Models.Selects;
with ASF.Contexts.Faces;

pragma Warnings (Off);  --  gcc complains that the package is not referenced but it is!
with ASF.Contexts.Flash;
pragma Warnings (On);

package body ASF.Applications.Tests is

   use ASF.Tests;
   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Applications");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test service HTTP invalid method",
                       Test_Invalid_Request'Access);
      Caller.Add_Test (Suite, "Test service HTTP GET (File_Servlet)",
                       Test_Get_File'Access);
      Caller.Add_Test (Suite, "Test service HTTP GET 404",
                       Test_Get_404'Access);

      Caller.Add_Test (Suite, "Test service HTTP GET (Measure_Servlet)",
                       Test_Get_Measures'Access);

      Caller.Add_Test (Suite, "Test service HTTP POST+INVOKE_APPLICATION (inputText)",
                       Test_Form_Post'Access);

      Caller.Add_Test (Suite, "Test service HTTP POST+PROCESS_VALIDATION (inputText)",
                       Test_Form_Post_Validation_Error'Access);

      Caller.Add_Test (Suite, "Test service HTTP POST+CSRF errors",
                       Test_Form_Post_CSRF'Access);

      Caller.Add_Test (Suite, "Test service HTTP POST+PROCESS_VALIDATION (selectOneMenu)",
                       Test_Form_Post_Select'Access);

      Caller.Add_Test (Suite, "Test AJAX bean method invocation",
                       Test_Ajax_Action'Access);

      Caller.Add_Test (Suite, "Test AJAX invalid requests",
                       Test_Ajax_Action_Error'Access);

      Caller.Add_Test (Suite, "Test POST/REDIRECT/GET with flash object",
                       Test_Flash_Object'Access);

      Caller.Add_Test (Suite, "Test GET+POST request with the default navigation",
                       Test_Default_Navigation_Rule'Access);

      Caller.Add_Test (Suite, "Test GET with view parameters",
                       Test_View_Params'Access);

      Caller.Add_Test (Suite, "Test GET with view action",
                       Test_View_Action'Access);

      Caller.Add_Test (Suite, "Test GET with request parameter injection from URI",
                       Test_View_Inject_Parameter'Access);
   end Add_Tests;

   package Save_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Form_Bean,
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
      elsif Name = "gender" then
         return Util.Beans.Objects.To_Object (From.Gender);
      elsif Name = "genderList" then
         declare
            List : ASF.Models.Selects.Select_Item_List;
         begin
            ASF.Models.Selects.Append (List, ASF.Models.Selects.Create_Select_Item ("None", "0"));
            ASF.Models.Selects.Append (List, ASF.Models.Selects.Create_Select_Item ("Mrs", "1"));
            ASF.Models.Selects.Append (List, ASF.Models.Selects.Create_Select_Item ("Mr", "2"));
            ASF.Models.Selects.Append (List, ASF.Models.Selects.Create_Select_Item ("Mss", "3"));
            return ASF.Models.Selects.To_Object (List);
         end;
      elsif Name = "called" then
         return Util.Beans.Objects.To_Object (From.Called);
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
      --  Simulate a Set_Value that raises some exception during the Update_Values phase.
      if Util.Beans.Objects.To_String (Value) = "error" then
         From.Perm_Error := True;
         raise Constraint_Error;
      end if;
      if Name = "email" then
         From.Email := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "password" then
         From.Password := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "name" then
         From.Name := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "gender" then
         From.Gender := Util.Beans.Objects.To_Unbounded_String (Value);
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
   --  Create a form bean.
   --  ------------------------------
   function Create_Form_Bean return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Form_Bean_Access := new Form_Bean;
   begin
      return Result.all'Access;
   end Create_Form_Bean;

   --  ------------------------------
   --  Create a list of forms.
   --  ------------------------------
   function Create_Form_List return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Form_Lists.List_Bean_Access := new Form_Lists.List_Bean;
      Form   : Form_Bean;
   begin
      for I in 1 .. 100 loop
         Form.Name := To_Unbounded_String ("Name" & Integer'Image (I));
         Form.Email := To_Unbounded_String ("Joe"  & Integer'Image (I) & "@gmail.com");
         Result.List.Append (Form);
      end loop;
      return Result.all'Access;
   end Create_Form_List;

   --  ------------------------------
   --  Initialize the ASF application for the unit test.
   --  ------------------------------
   procedure Initialize_Test_Application is
      use type ASF.Applications.Main.Application_Access;
      Fact     : ASF.Applications.Main.Application_Factory;
      Config   : constant String := Util.Tests.Get_Path ("regtests/config/test-config.xml");
   begin
      if ASF.Tests.Get_Application = null then
         ASF.Tests.Initialize (Util.Tests.Get_Properties, Factory => Fact);
      end if;
      ASF.Applications.Main.Configs.Read_Configuration (App  => ASF.Tests.Get_Application.all,
                                                        File => Config);
      ASF.Tests.Get_Application.Start;
   end Initialize_Test_Application;

   --  ------------------------------
   --  Initialize the test application
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Initialize_Test_Application;
   end Set_Up;

   --  ------------------------------
   --  Action to authenticate a user (password authentication).
   --  ------------------------------
   procedure Save (Data    : in out Form_Bean;
                   Outcome : in out Unbounded_String) is
      use Util.Beans.Objects;
   begin
      if Data.Def_Nav then
         Outcome := To_Unbounded_String ("form-default-result");
      else
         Outcome := To_Unbounded_String ("success");
      end if;
      Data.Called := Data.Called + 1;
      if Data.Use_Flash then
         declare
            Ctx : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
         begin
            Ctx.Get_Flash.Set_Attribute ("name", To_Object (Data.Name));
            Ctx.Get_Flash.Set_Attribute ("email", To_Object (Data.Email));
            Ctx.Get_Flash.Set_Keep_Messages (True);
            Ctx.Add_Message (Client_Id => "",
                             Message   => "Message saved in the flash context");
         end;
      end if;
      if Data.Perm_Error then
         raise Test_Exception;
      end if;
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
      Assert_Matches (T, ".*This is a 404 error page.*", Reply, "Invalid 404 page returned",
                      Status => ASF.Responses.SC_NOT_FOUND);

      Do_Get (Request, Reply, "/file-does-not-exist.js", "test-404.html");
      Assert_Equals (T, ASF.Responses.SC_NOT_FOUND, Reply.Get_Status, "Invalid response");
      Assert_Matches (T, ".*This is a 404 error page.*", Reply, "Invalid 404 page returned",
                      Status => ASF.Responses.SC_NOT_FOUND);
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
      Assert_Header (T, "Content-Type", "text/plain", Reply, "Content-Type");

      Do_Get (Request, Reply, "/views/set.html", "get-file-set.html");
      Assert_Matches (T, "^\s*John Smith\s?$", Reply, "Wrong content");
      Assert_Header (T, "Content-Type", "text/html", Reply, "Content-Type");

      Do_Get (Request, Reply, "/js/asf.js", "get-file-asf.js");
      Assert_Matches (T, "^\s*var ASF = {};\s?$", Reply, "Wrong content");
      Assert_Header (T, "Content-Type", "text/javascript", Reply, "Content-Type");

      Do_Get (Request, Reply, "/css/asf.css", "get-file-asf.css");
      Assert_Matches (T, "^.*asf.css.*$", Reply, "Wrong content");
      Assert_Header (T, "Content-Type", "text/css", Reply, "Content-Type");

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
      Assert_Matches (T, "<time count=""\d+"" time=""\d+.\d+ [um]s"" title="".*""/>",
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

      Request.Set_Parameter ("formText", ASF.Tests.Extract ("formText", "form-text.txt"));
      Request.Set_Parameter ("name", "John");
      Request.Set_Parameter ("password", "12345");
      Request.Set_Parameter ("email", "john@gmail.com");
      Request.Set_Parameter ("ok", "1");
      Do_Post (Request, Reply, "/tests/form-text.html", "form-text-post-1.txt");

      Assert_Matches (T, ".*success exact.*",
                      Reply, "Wrong form content");

      Assert_Equals (T, "John", Form.Name, "Form name not saved in the bean");
      Assert_Equals (T, "12345", Form.Password, "Form password not saved in the bean");
      Assert_Equals (T, "john@gmail.com", Form.Email, "Form email not saved in the bean");
      Assert_Equals (T, 1, Form.Called, "save action was not called");
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
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Get (Request, Reply, "/tests/form-text.html", "form-text.txt");

      --  Post with password too short and empty email
      Request.Set_Parameter ("ok", "1");
      Request.Set_Parameter ("formText", ASF.Tests.Extract ("formText", "form-text.txt"));
      Request.Set_Parameter ("name", "John");
      Request.Set_Parameter ("password", "12");
      Request.Set_Parameter ("email", "");
      Do_Post (Request, Reply, "/tests/form-text.html", "form-text-post-1.txt");

      Assert_Matches (T, ".*<input type=.text. name=.name. value=.John. id=.name.*",
                      Reply, "Wrong form content");

      Assert_Matches (T, ".*Validation Error: Length is less than "
                      & "allowable minimum of '4'.*",
                      Reply, "Missing error message");

      --  No field should be modified.
      Assert_Equals (T, "", Form.Name, "Form name was saved in the bean");
      Assert_Equals (T, "", Form.Password, "Form password was saved in the bean");
      Assert_Equals (T, "", Form.Email, "Form email was saved in the bean");

      Request.Set_Parameter ("ok", "1");
      Request.Set_Parameter ("email", "1");
      Request.Set_Parameter ("password", "12333");
      Request.Set_Parameter ("name", "122222222222222222222222222222222222222222");
      Do_Post (Request, Reply, "/tests/form-text.html", "form-text-post-2.txt");

      Assert_Matches (T, ".*<span class=.error.>Invalid email address</span>.*",
                      Reply, "Invalid error message for email");

      Assert_Matches (T, ".*<span class=.error.>Invalid name</span>.*",
                      Reply, "Invalid error message for name");

      Request.Set_Parameter ("ok", "1");
      Request.Set_Parameter ("email", "1dddddd");
      Request.Set_Parameter ("password", "12333ddddddddddddddd");
      Request.Set_Parameter ("name", "1222222222");
      Do_Post (Request, Reply, "/tests/form-text.html", "form-text-post-3.txt");

      Assert_Matches (T, ".*Validation Error: Length is greater than "
                      & "allowable maximum of '10'.*",
                      Reply, "Invalid error message for password");

      Request.Set_Parameter ("ok", "1");
      Request.Set_Parameter ("email", "error");
      Request.Set_Parameter ("password", "12333");
      Request.Set_Parameter ("name", "1222222222");
      Do_Post (Request, Reply, "/tests/form-text.html", "form-text-post-4.txt");

      Assert_Matches (T, ".*<span class=.error.>Validation Error: Value is not correct.*",
                      Reply, "Invalid error message for name");

      Assert_Equals (T, 0, Form.Called, "Save method should not be called");
   end Test_Form_Post_Validation_Error;

   --  ------------------------------
   --  Test a POST request with an invalid CSRF token.
   --  ------------------------------
   procedure Test_Form_Post_CSRF (T : in out Test) is
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

      declare
         Token : constant String := ASF.Tests.Extract ("formText", "form-text.txt");
      begin
         Request.Set_Parameter ("formText", Token & "x");
         Request.Set_Parameter ("name", "John");
         Request.Set_Parameter ("password", "12345");
         Request.Set_Parameter ("email", "john@gmail.com");
         Request.Set_Parameter ("ok", "1");
         Do_Post (Request, Reply, "/tests/form-text.html", "form-text-post-csrf-1.txt");

         Assert_Matches (T, ".*The form has expired.*",
                        Reply, "Wrong form content");

         Request.Set_Parameter ("formText", "2" & Token);
         Request.Set_Parameter ("name", "John");
         Request.Set_Parameter ("password", "12345");
         Request.Set_Parameter ("email", "john@gmail.com");
         Request.Set_Parameter ("ok", "1");
         Do_Post (Request, Reply, "/tests/form-text.html", "form-text-post-csrf-2.txt");

         Assert_Matches (T, ".*The form has expired.*",
                        Reply, "Wrong form content");
      end;
      Assert_Equals (T, 0, Form.Called, "form action must not be called");
   end Test_Form_Post_CSRF;

   --  ------------------------------
   --  Test a GET+POST request with form having <h:selectOneMenu> element.
   --  ------------------------------
   procedure Test_Form_Post_Select (T : in out Test) is
      use Util.Beans.Objects;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Form    : aliased Form_Bean;
   begin
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Get (Request, Reply, "/tests/form-select.html", "form-select.txt");

      Assert_Matches (T, ".*<label for=.gender.>Gender</label>.*", Reply, "Wrong form content");
      Assert_Matches (T, ".*<select name=.gender.*",
                      Reply, "Wrong form content");

      Request.Set_Parameter ("formSelect", ASF.Tests.Extract ("formSelect", "form-select.txt"));
      Request.Set_Parameter ("gender", "2");
      Do_Post (Request, Reply, "/tests/form-select.html", "form-select-post.txt");

      Assert_Matches (T, ".*<option value=.2. selected=.selected.*",
                      Reply, "Wrong form content");

      Request.Set_Parameter ("formSelect", ASF.Tests.Extract ("formSelect", "form-select.txt"));
      Request.Set_Parameter ("gender", "3");
      Do_Post (Request, Reply, "/tests/form-select.html", "form-select-post2.txt");

      Assert_Matches (T, ".*<option value=.3. selected=.selected.*",
                      Reply, "Wrong form content");

   end Test_Form_Post_Select;

   --  ------------------------------
   --  Test a POST request to invoke a bean method.
   --  ------------------------------
   procedure Test_Ajax_Action (T : in out Test) is
      use Util.Beans.Objects;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Form    : aliased Form_Bean;
   begin
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Post (Request, Reply, "/ajax/form/save", "ajax-action-1.txt");

      Assert_Equals (T, 1, Form.Called, "Save method was not called");
   end Test_Ajax_Action;

   --  ------------------------------
   --  Test a POST request to invoke a bean method.
   --  Verify that invalid requests raise an error.
   --  ------------------------------
   procedure Test_Ajax_Action_Error (T : in out Test) is
      use Util.Beans.Objects;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Form    : aliased Form_Bean;
   begin
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));

      Do_Post (Request, Reply, "/ajax/", "ajax-error-1.txt");
      Assert_Equals (T, ASF.Responses.SC_NOT_FOUND, Reply.Get_Status, "Invalid error response 1");

      Do_Post (Request, Reply, "/ajax/a", "ajax-error-2.txt");
      Assert_Equals (T, ASF.Responses.SC_NOT_FOUND, Reply.Get_Status, "Invalid error response 2");

      Do_Post (Request, Reply, "/ajax/a/", "ajax-error-3.txt");
      Assert_Equals (T, ASF.Responses.SC_NOT_FOUND, Reply.Get_Status, "Invalid error response 3");

      Do_Post (Request, Reply, "/ajax//c", "ajax-error-4.txt");
      Assert_Equals (T, ASF.Responses.SC_NOT_FOUND, Reply.Get_Status, "Invalid error response 4");

      Do_Post (Request, Reply, "/ajax/form/savex", "ajax-error-5.txt");
      Assert_Equals (T, ASF.Responses.SC_NOT_FOUND, Reply.Get_Status, "Invalid error response 5");

      Do_Post (Request, Reply, "/ajax/formx/save", "ajax-error-6.txt");
      Assert_Equals (T, ASF.Responses.SC_NOT_FOUND, Reply.Get_Status, "Invalid error response 6");

   end Test_Ajax_Action_Error;

   --  ------------------------------
   --  Test a POST/REDIRECT/GET request with a flash information passed in between.
   --  ------------------------------
   procedure Test_Flash_Object (T : in out Test) is
      use Util.Beans.Objects;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Form    : aliased Form_Bean;
      Path    : constant String := Util.Tests.Get_Path ("regtests/config/test-config.xml");
      App     : constant ASF.Applications.Main.Application_Access := ASF.Tests.Get_Application;
   begin
      ASF.Applications.Main.Configs.Read_Configuration (App.all, Path);

      Form.Use_Flash := True;
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Get (Request, Reply, "/tests/form-text-redirect.html", "form-text-flash.txt");

      Assert_Matches (T, ".*<label for=.name.>Name</label>.*", Reply, "Wrong form content");
      Assert_Matches (T, ".*<input type=.text. name=.name. value=.. id=.name.*",
                      Reply, "Wrong form content");

      Request.Set_Parameter ("formText", ASF.Tests.Extract ("formText", "form-text-flash.txt"));
      Request.Set_Parameter ("name", "John");
      Request.Set_Parameter ("password", "12345");
      Request.Set_Parameter ("email", "john@gmail.com");
      Request.Set_Parameter ("ok", "1");
      Do_Post (Request, Reply, "/tests/form-text-redirect.html", "form-text-post-flash.txt");

      --  The navigation rule should redirect to the GET page.
      Assert_Redirect (T, "/asfunit/tests/flash-data.html", Reply, "Invalid response");

      Request.Set_Cookie (Reply);
      Do_Get (Request, Reply, "/tests/flash-data.html", "flash-data.txt");

      Assert_Matches (T, ".*Name: John.*",
                      Reply, "Wrong form content");
      Assert_Matches (T, ".*Email: john@gmail.com",
                      Reply, "Wrong form content");
      Assert_Matches (T, ".*Message saved in the flash context.*",
                      Reply, "Message was not restored");

      Request.Set_Cookie (Reply);
      Do_Get (Request, Reply, "/tests/flash-data.html", "flash-data-2.txt");
      Assert_Matches (T, ".*Name:  Email:",
                      Reply, "Wrong form content");

   end Test_Flash_Object;

   --  ------------------------------
   --  Test a GET+POST request with the default navigation rule based on the outcome.
   --  ------------------------------
   procedure Test_Default_Navigation_Rule (T : in out Test) is
      use Util.Beans.Objects;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Form    : aliased Form_Bean;
   begin
      Form.Def_Nav := True;
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Get (Request, Reply, "/tests/form-text-default.html", "form-text-default.txt");

      Assert_Matches (T, ".*<label for=.name.>Name</label>.*", Reply, "Wrong form content");
      Assert_Matches (T, ".*<input type=.text. name=.name. value=.. id=.name.*",
                      Reply, "Wrong form content");

      Request.Set_Parameter ("formText", ASF.Tests.Extract ("formText", "form-text-default.txt"));
      Request.Set_Parameter ("name", "John");
      Request.Set_Parameter ("password", "12345");
      Request.Set_Parameter ("email", "john@gmail.com");
      Request.Set_Parameter ("ok", "1");
      Do_Post (Request, Reply, "/tests/form-text-default.html", "form-text-post-default.txt");

      Assert_Matches (T, ".*Email: john@gmail.com Name: John.*",
                      Reply, "Wrong form content");
   end Test_Default_Navigation_Rule;

   --  ------------------------------
   --  Test a GET request with meta data and view parameters.
   --  ------------------------------
   procedure Test_View_Params (T : in out Test) is
      use Util.Beans.Objects;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Form    : aliased Form_Bean;
      Path    : constant String := Util.Tests.Get_Path ("regtests/config/test-config.xml");
      App     : constant ASF.Applications.Main.Application_Access := ASF.Tests.Get_Application;
   begin
      ASF.Applications.Main.Configs.Read_Configuration (App.all, Path);

      Form.Use_Flash := True;
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Get (Request, Reply, "/tests/view-params.html?name=John&email=john@gmail.com&is_a=male",
              "view-params.txt");

      Assert_Equals (T, "John", Form.Name, "View parameter for name was not set");
      Assert_Equals (T, "john@gmail.com", Form.Email, "View parameter for email was not set");
      Assert_Equals (T, "male", Form.Gender, "View parameter for gender was not set");
      Assert_Matches (T, ".*Name: John Email: john@gmail.com Gender: male",
                      Reply, "Wrong generated content");
   end Test_View_Params;

   --  ------------------------------
   --  Test a GET request with meta data and view action.
   --  ------------------------------
   procedure Test_View_Action (T : in out Test) is
      use Util.Beans.Objects;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Form    : aliased Form_Bean;
      Path    : constant String := Util.Tests.Get_Path ("regtests/config/test-config.xml");
      App     : constant ASF.Applications.Main.Application_Access := ASF.Tests.Get_Application;
   begin
      ASF.Applications.Main.Configs.Read_Configuration (App.all, Path);

      Form.Use_Flash := True;
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Get (Request, Reply, "/tests/view-action.html?name=John&email=john@gmail.com&is_a=male",
              "view-action.txt");

      Assert_Equals (T, 1, Form.Called, "View action was not called");
      Assert_Equals (T, "John", Form.Name, "View parameter for name was not set");
      Assert_Equals (T, "john@gmail.com", Form.Email, "View parameter for email was not set");
      Assert_Equals (T, "male", Form.Gender, "View parameter for gender was not set");
      Assert_Matches (T, ".*Name: John Email: john@gmail.com Gender: male",
                      Reply, "Wrong generated content");
   end Test_View_Action;

   --  ------------------------------
   --  Test a GET request with pretty URL and request parameter injection.
   --  ------------------------------
   procedure Test_View_Inject_Parameter (T : in out Test) is
      use Util.Beans.Objects;

      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Form    : aliased Form_Bean;
      Path    : constant String := Util.Tests.Get_Path ("regtests/config/test-inject.xml");
      App     : constant ASF.Applications.Main.Application_Access := ASF.Tests.Get_Application;
   begin
      ASF.Applications.Main.Configs.Read_Configuration (App.all, Path);

      App.Dump_Routes (Util.Log.INFO_LEVEL);
      Form.Use_Flash := True;
      Request.Set_Attribute ("user", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Get (Request, Reply, "/users/john/view", "view-user.txt");

      Assert_Equals (T, 0, Form.Called, "View action must not be called");
      Assert_Equals (T, "john", Form.Name, "View parameter for name was not set");
      Assert_Matches (T, ".*Name: john Email:  Gender: ",
                      Reply, "Wrong generated content");

      Do_Get (Request, Reply, "/users/harry/potter/view", "view-user2.txt");

      Assert_Equals (T, "harry", Form.Name, "View parameter for name was not set");
      Assert_Equals (T, "potter", Form.Email, "View parameter for email was not set");
      Assert_Matches (T, ".*Name: harry Email: potter Gender: ",
                      Reply, "Wrong generated content");

      Do_Get (Request, Reply, "/users/Gandalf/Mithrandir/view/wizard", "view-user3.txt");

      Assert_Equals (T, "Gandalf", Form.Name, "View parameter for name was not set");
      Assert_Equals (T, "Mithrandir", Form.Email, "View parameter for email was not set");
      Assert_Equals (T, "wizard", Form.Gender, "View parameter for gender was not set");
      Assert_Matches (T, ".*Name: Gandalf Email: Mithrandir Gender: wizard",
                      Reply, "Wrong generated content");

   end Test_View_Inject_Parameter;

end ASF.Applications.Tests;
