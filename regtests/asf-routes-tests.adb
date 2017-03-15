-----------------------------------------------------------------------
--  asf-routes-tests - Unit tests for ASF.Routes
--  Copyright (C) 2015, 2016, 2017 Stephane Carrez
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

with Util.Measures;
with Util.Log.Loggers;
with Util.Test_Caller;

package body ASF.Routes.Tests is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Routes.Tests");

   package Caller is new Util.Test_Caller (Test, "Routes");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ASF.Routes.Add_Route (fixed path)",
                       Test_Add_Route_With_Path'Access);
      Caller.Add_Test (Suite, "Test ASF.Routes.Add_Route (:param path)",
                       Test_Add_Route_With_Param'Access);
      Caller.Add_Test (Suite, "Test ASF.Routes.Add_Route (*.ext path)",
                       Test_Add_Route_With_Ext'Access);
      Caller.Add_Test (Suite, "Test ASF.Routes.Add_Route (#{} path)",
                       Test_Add_Route_With_EL'Access);
      Caller.Add_Test (Suite, "Test ASF.Routes.Iterate",
                       Test_Iterate'Access);
   end Add_Tests;

   overriding
   function Get_Value (Bean : in Test_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "id" then
         return Util.Beans.Objects.To_Object (Bean.Id);
      elsif Name = "name" then
         return Util.Beans.Objects.To_Object (Bean.Name);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   overriding
   procedure Set_Value (Bean  : in out Test_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" then
         Bean.Id := Util.Beans.Objects.To_Unbounded_String (Value);
      elsif Name = "name" then
         Bean.Name := Util.Beans.Objects.To_Unbounded_String (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Setup the test instance.
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out Test) is
   begin
      T.Bean := new Test_Bean;
      ASF.Tests.EL_Test (T).Set_Up;
      T.Root_Resolver.Register (Ada.Strings.Unbounded.To_Unbounded_String ("user"),
                                Util.Beans.Objects.To_Object (T.Bean.all'Access));
      for I in T.Routes'Range loop
         T.Routes (I) := Route_Type_Refs.Create
            (new Test_Route_Type '(Util.Refs.Ref_Entity with Index => I));
      end loop;
   end Set_Up;

   --  ------------------------------
   --  Verify that the path matches the given route.
   --  ------------------------------
   procedure Verify_Route (T      : in out Test;
                           Router : in out Router_Type;
                           Path   : in String;
                           Index  : in Positive;
                           Bean   : in out Test_Bean'Class) is
      Route   : constant Route_Type_Access := T.Routes (Index).Value;
      R       : Route_Context_Type;
   begin
      Router.Find_Route (Path, R);
      declare
         P : constant String := Get_Path (R);
      begin
         T.Assert_Equals (Path, P, "Add_Route + Find_Route with " & Path);
         T.Assert (Get_Route (R) /= null, "Get_Route returns a null route for " & Path);
         T.Assert (Get_Route (R) = Route, "Get_Route returns a wrong route for " & Path);

         --  Inject the path parameters in the bean instance.
         Inject_Parameters (R, Bean, T.ELContext.all);
      end;
   end Verify_Route;

   --  ------------------------------
   --  Add the route associated with the path pattern.
   --  ------------------------------
   procedure Add_Route (T      : in out Test;
                        Router : in out Router_Type;
                        Path   : in String;
                        Index  : in Positive;
                        Bean   : in out Test_Bean'Class) is
      procedure Insert (Route : in out ASF.Routes.Route_Type_Ref);

      procedure Insert (Route : in out ASF.Routes.Route_Type_Ref) is
      begin
         Route := T.Routes (Index);
      end Insert;
   begin
      Router.Add_Route (Path, T.ELContext.all, Insert'Access);
      Verify_Route (T, Router, Path, Index, Bean);
   end Add_Route;

   --  ------------------------------
   --  Test the Add_Route with simple fixed path components.
   --  Example: /list/index.html
   --  ------------------------------
   procedure Test_Add_Route_With_Path (T : in out Test) is
      Router  : Router_Type;
      Bean    : Test_Bean;
   begin
      Add_Route (T, Router, "/page.html", 1, Bean);
      Add_Route (T, Router, "/list/page.html", 2, Bean);
      Add_Route (T, Router, "/list/index.html", 3, Bean);
      Add_Route (T, Router, "/view/page/content/index.html", 4, Bean);
      Add_Route (T, Router, "/list//page/view.html", 5, Bean);
      Add_Route (T, Router, "/list////page/content.html", 6, Bean);

      Verify_Route (T, Router, "/list/index.html", 3, Bean);
   end Test_Add_Route_With_Path;

   --  ------------------------------
   --  Test the Add_Route with extension mapping.
   --  Example: /list/*.html
   --  ------------------------------
   procedure Test_Add_Route_With_Ext (T : in out Test) is
      Router  : Router_Type;
      Bean    : Test_Bean;
   begin
      Add_Route (T, Router, "/page.html", 1, Bean);
      Add_Route (T, Router, "/list/*.html", 2, Bean);
      Add_Route (T, Router, "/list/index.html", 3, Bean);
      Add_Route (T, Router, "/view/page/content/page1.html", 4, Bean);
      Add_Route (T, Router, "/view/page/content/page2.html", 5, Bean);
      Add_Route (T, Router, "/view/page/content/*.html", 6, Bean);
      Add_Route (T, Router, "/ajax/*", 7, Bean);
      Add_Route (T, Router, "*.html", 8, Bean);

      --  Verify precedence and wildcard matching.
      Verify_Route (T, Router, "/list/index.html", 3, Bean);
      Verify_Route (T, Router, "/list/admin.html", 2, Bean);
      Verify_Route (T, Router, "/list/1/2/3/admin.html", 2, Bean);
      Verify_Route (T, Router, "/view/page/content/t.html", 6, Bean);
      Verify_Route (T, Router, "/view/page/content/1/t.html", 6, Bean);
      Verify_Route (T, Router, "/view/page/content/1/2/t.html", 6, Bean);
      Verify_Route (T, Router, "/ajax/form/save", 7, Bean);
      Verify_Route (T, Router, "/view/index.html", 8, Bean);

      Add_Route (T, Router, "/ajax/timeKeeper/*", 9, Bean);
      Verify_Route (T, Router, "/ajax/form/save", 7, Bean);
      Verify_Route (T, Router, "/ajax/timeKeeper/save", 9, Bean);
   end Test_Add_Route_With_Ext;

   --  ------------------------------
   --  Test the Add_Route with fixed path components and path parameters.
   --  Example: /users/:id/view.html
   --  ------------------------------
   procedure Test_Add_Route_With_Param (T : in out Test) is
      use Ada.Strings.Unbounded;

      Router  : Router_Type;
      Bean    : Test_Bean;
   begin
      Add_Route (T, Router, "/users/:id/view.html", 1, Bean);
      T.Assert_Equals (":id", To_String (Bean.Id), "Bean injection failed for :id");

      Bean.Id := To_Unbounded_String ("");
      Add_Route (T, Router, "/users/:id/list.html", 2, Bean);
      T.Assert_Equals (":id", To_String (Bean.Id), "Bean injection failed for :id");

      Bean.Id := To_Unbounded_String ("");
      Add_Route (T, Router, "/users/:id/index.html", 3, Bean);
      T.Assert_Equals (":id", To_String (Bean.Id), "Bean injection failed for :id");

      Bean.Id := To_Unbounded_String ("");
      Add_Route (T, Router, "/view/page/content/index.html", 4, Bean);
      Add_Route (T, Router, "/list//page/view.html", 5, Bean);
      Add_Route (T, Router, "/list////page/content.html", 6, Bean);
      T.Assert_Equals ("", To_String (Bean.Id), "Bean injection failed for fixed path");

      Add_Route (T, Router, "/users/:id/:name/index.html", 7, Bean);
      T.Assert_Equals (":id", To_String (Bean.Id), "Bean injection failed for :id");
      T.Assert_Equals (":name", To_String (Bean.Name), "Bean injection failed for :name");

      Add_Route (T, Router, "/users/list/index.html", 8, Bean);

      Verify_Route (T, Router, "/users/1234/view.html", 1, Bean);
      T.Assert_Equals ("1234", To_String (Bean.Id), "Bean injection failed for :id");

      Verify_Route (T, Router, "/users/234/567/index.html", 7, Bean);
      T.Assert_Equals ("234", To_String (Bean.Id), "Bean injection failed for :id");
      T.Assert_Equals ("567", To_String (Bean.Name), "Bean injection failed for :name");

   end Test_Add_Route_With_Param;

   --  ------------------------------
   --  Test the Add_Route with fixed path components and EL path injection.
   --  Example: /users/#{user.id}/view.html
   --  ------------------------------
   procedure Test_Add_Route_With_EL (T : in out Test) is
            use Ada.Strings.Unbounded;

      Router  : Router_Type;
      Bean    : aliased Test_Bean;
   begin
      Add_Route (T, Router, "/users/#{user.id}", 1, Bean);
      Add_Route (T, Router, "/users/view.html", 2, Bean);
      Add_Route (T, Router, "/users/#{user.id}/#{user.name}/index.html", 3, Bean);
      Add_Route (T, Router, "/users/admin/#{user.id}/pages/#{user.name}", 4, Bean);

      --  Verify that the path parameters are injected in the 'user' bean (= T.Bean).
      Verify_Route (T, Router, "/users/234/567/index.html", 3, Bean);
      T.Assert_Equals ("234", To_String (T.Bean.Id), "Bean injection failed for :id");
      T.Assert_Equals ("567", To_String (T.Bean.Name), "Bean injection failed for :name");

   end Test_Add_Route_With_EL;

   P_1 : aliased constant String := "/list/index.html";
   P_2 : aliased constant String := "/users/:id";
   P_3 : aliased constant String := "/users/:id/view";
   P_4 : aliased constant String := "/users/index.html";
   P_5 : aliased constant String := "/users/:id/:name";
   P_6 : aliased constant String := "/users/:id/:name/view.html";
   P_7 : aliased constant String := "/users/:id/list";
   P_8 : aliased constant String := "/users/test.html";
   P_9 : aliased constant String := "/admin/1/2/3/4/5/list.html";
   P_10 : aliased constant String := "/admin/*.html";
   P_11 : aliased constant String := "/admin/*.png";
   P_12 : aliased constant String := "/admin/*.jpg";
   P_13 : aliased constant String := "/users/:id/page/*.xml";
   P_14 : aliased constant String := "/*.jsf";

   type Const_String_Access is access constant String;

   type String_Array is array (Positive range <>) of Const_String_Access;

   Path_Array : constant String_Array :=
     (P_1'Access, P_2'Access, P_3'Access, P_4'Access,
      P_5'Access, P_6'Access, P_7'Access, P_8'Access,
      P_9'Access, P_10'Access, P_11'Access, P_12'Access,
      P_13'Access, P_14'Access);

   --  ------------------------------
   --  Test the Iterate over several paths.
   --  ------------------------------
   procedure Test_Iterate (T : in out Test) is
      procedure Process (Pattern : in String;
                         Route   : in Route_Type_Access);

      Router  : Router_Type;
      Bean    : Test_Bean;

      procedure Process (Pattern : in String;
                         Route   : in Route_Type_Access) is
      begin
         T.Assert (Route /= null, "The route is null for " & Pattern);
         T.Assert (Route.all in Test_Route_Type'Class, "Invalid route for " & Pattern);

         Log.Info ("Route {0} to {1}", Pattern, Natural'Image (Test_Route_Type (Route.all).Index));
         T.Assert_Equals (Pattern, Path_Array (Test_Route_Type (Route.all).Index).all,
                          "Invalid route for " & Pattern);
      end Process;

   begin
      for I in Path_Array'Range loop
         Add_Route (T, Router, Path_Array (I).all, I, Bean);
      end loop;
      Router.Iterate (Process'Access);

      declare
         St : Util.Measures.Stamp;
      begin
         for I in 1 .. 1000 loop
            declare
               R : Route_Context_Type;
            begin
               Router.Find_Route ("/admin/1/2/3/4/5/list.html", R);
            end;
         end loop;
         Util.Measures.Report (St, "Find 1000 routes (fixed path)");
      end;

      declare
         St : Util.Measures.Stamp;
      begin
         for I in 1 .. 1000 loop
            declare
               R : Route_Context_Type;
            begin
               Router.Find_Route ("/admin/1/2/3/4/5/list.jsf", R);
            end;
         end loop;
         Util.Measures.Report (St, "Find 1000 routes (extension)");
      end;
   end Test_Iterate;

end ASF.Routes.Tests;
