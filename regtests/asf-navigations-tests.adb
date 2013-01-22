-----------------------------------------------------------------------
--  asf-navigations-tests -  Tests for ASF navigation
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
with Util.Beans.Objects;

with ASF.Tests;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Applications.Main;
with ASF.Applications.Main.Configs;
with ASF.Applications.Tests;
package body ASF.Navigations.Tests is

   use Util.Beans.Objects;
   use ASF.Tests;

   package Caller is new Util.Test_Caller (Test, "Navigations");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test navigation exact match (view, outcome, action)",
                       Test_Exact_Navigation'Access);

      Caller.Add_Test (Suite, "Test navigation partial match (view, outcome)",
                       Test_Partial_Navigation'Access);

      Caller.Add_Test (Suite, "Test navigation exception match (view, outcome)",
                       Test_Exception_Navigation'Access);

   end Add_Tests;

   --  ------------------------------
   --  Initialize the test application
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);

      use type ASF.Applications.Main.Application_Access;
      Fact     : ASF.Applications.Main.Application_Factory;
   begin
      if ASF.Tests.Get_Application = null then
         ASF.Tests.Initialize (Util.Tests.Get_Properties, Factory => Fact);
      end if;
   end Set_Up;

   --  ------------------------------
   --  Check the navigation for an URI and expect the result to match the regular expression.
   --  ------------------------------
   procedure Check_Navigation (T     : in out Test;
                               Name  : in String;
                               Match : in String;
                               Raise_Flag : in Boolean := False) is
      Request  : ASF.Requests.Mockup.Request;
      Reply    : ASF.Responses.Mockup.Response;
      Form     : aliased ASF.Applications.Tests.Form_Bean;
      File     : constant String := Util.Tests.Get_Path ("regtests/config/test-navigations.xml");
   begin
      Form.Perm_Error := Raise_Flag;
      ASF.Applications.Main.Configs.Read_Configuration (ASF.Tests.Get_Application.all, File);
      Request.Set_Attribute ("form", To_Object (Value   => Form'Unchecked_Access,
                                                Storage => STATIC));
      Do_Get (Request, Reply, "/tests/" & Name & ".html", Name & ".txt");

      Request.Set_Parameter ("formText", "1");
      Request.Set_Parameter ("name", "John");
      Request.Set_Parameter ("password", "12345");
      Request.Set_Parameter ("email", "john@gmail.com");
      Request.Set_Parameter ("ok", "1");
      Do_Post (Request, Reply, "/tests/" & Name & ".html", Name & "form-navigation-exact.txt");
      Assert_Matches (T, Match,
                      Reply, "Wrong generated content");
   end Check_Navigation;

   --  ------------------------------
   --  Test a form navigation with an exact match (view, outcome, action).
   --  ------------------------------
   procedure Test_Exact_Navigation (T : in out Test) is
   begin
      T.Check_Navigation ("form-nav", ".*success.*");
   end Test_Exact_Navigation;

   --  ------------------------------
   --  Test a form navigation with a partial match (view, outcome).
   --  ------------------------------
   procedure Test_Partial_Navigation (T : in out Test) is
   begin
      T.Check_Navigation ("form-nav-partial", ".*success partial.*");
   end Test_Partial_Navigation;

   --  ------------------------------
   --  Test a form navigation with a exception match (view, outcome).
   --  ------------------------------
   procedure Test_Exception_Navigation (T : in out Test) is
   begin
      T.Check_Navigation ("form-nav-exception", ".*success exception.*", True);
   end Test_Exception_Navigation;

end ASF.Navigations.Tests;
