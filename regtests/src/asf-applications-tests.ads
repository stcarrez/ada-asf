-----------------------------------------------------------------------
--  asf-applications-tests -  ASF Application tests using ASFUnit
--  Copyright (C) 2011, 2015, 2023 Stephane Carrez
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
with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Methods;
with Util.Beans.Basic.Lists;
with Ada.Strings.Unbounded;

package ASF.Applications.Tests is

   use Ada.Strings.Unbounded;

   Test_Exception : exception;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Initialize the test application
   overriding
   procedure Set_Up (T : in out Test);

   --  Test a GET request on a static file served by the File_Servlet.
   procedure Test_Get_File (T : in out Test);

   --  Test a GET 404 error on missing static file served by the File_Servlet.
   procedure Test_Get_404 (T : in out Test);

   --  Test a GET request on the measure servlet
   procedure Test_Get_Measures (T : in out Test);

   --  Test an invalid HTTP request.
   procedure Test_Invalid_Request (T : in out Test);

   --  Test a GET+POST request with submitted values and an action method called on the bean.
   procedure Test_Form_Post (T : in out Test);

   --  Test a POST request with an invalid submitted value
   procedure Test_Form_Post_Validation_Error (T : in out Test);

   --  Test a POST request with an invalid CSRF token.
   procedure Test_Form_Post_CSRF (T : in out Test);

   --  Test a GET+POST request with form having <h:selectOneMenu> element.
   procedure Test_Form_Post_Select (T : in out Test);

   --  Test a POST request to invoke a bean method.
   procedure Test_Ajax_Action (T : in out Test);

   --  Test a POST request to invoke a bean method.
   --  Verify that invalid requests raise an error.
   procedure Test_Ajax_Action_Error (T : in out Test);

   --  Test a POST/REDIRECT/GET request with a flash information passed in between.
   procedure Test_Flash_Object (T : in out Test);

   --  Test a GET+POST request with the default navigation rule based on the outcome.
   procedure Test_Default_Navigation_Rule (T : in out Test);

   --  Test a GET request with meta data and view parameters.
   procedure Test_View_Params (T : in out Test);

   --  Test a GET request with meta data and view action.
   procedure Test_View_Action (T : in out Test);

   --  Test a GET request with pretty URL and request parameter injection.
   procedure Test_View_Inject_Parameter (T : in out Test);

   type Form_Bean is new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with record
      Name       : Unbounded_String;
      Password   : Unbounded_String;
      Email      : Unbounded_String;
      Called     : Natural := 0;
      Gender     : Unbounded_String;
      Use_Flash  : Boolean := False;
      Def_Nav    : Boolean := False;
      Perm_Error : Boolean := False;
   end record;
   type Form_Bean_Access is access all Form_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Form_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Form_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Form_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Action to save the form
   procedure Save (Data    : in out Form_Bean;
                   Outcome : in out Unbounded_String);

   --  Create a form bean.
   function Create_Form_Bean return Util.Beans.Basic.Readonly_Bean_Access;

   --  Create a list of forms.
   package Form_Lists is
     new Util.Beans.Basic.Lists (Form_Bean);

   --  Create a list of forms.
   function Create_Form_List return Util.Beans.Basic.Readonly_Bean_Access;

   --  Initialize the ASF application for the unit test.
   procedure Initialize_Test_Application;

end ASF.Applications.Tests;
