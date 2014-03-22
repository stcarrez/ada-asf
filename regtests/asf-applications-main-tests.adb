-----------------------------------------------------------------------
--  asf-applications-main-tests - Unit tests for Applications
--  Copyright (C) 2011, 2012, 2014 Stephane Carrez
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

with Ada.Unchecked_Deallocation;

with EL.Contexts.Default;

with ASF.Applications.Tests;
with ASF.Applications.Main.Configs;
with ASF.Requests.Mockup;
package body ASF.Applications.Main.Tests is

   use Util.Tests;

   function Create_Form_Bean return Util.Beans.Basic.Readonly_Bean_Access;

   package Caller is new Util.Test_Caller (Test, "Applications.Main");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ASF.Applications.Main.Read_Configuration",
                       Test_Read_Configuration'Access);
      Caller.Add_Test (Suite, "Test ASF.Applications.Main.Create",
                       Test_Create_Bean'Access);
      Caller.Add_Test (Suite, "Test ASF.Applications.Main.Load_Bundle",
                       Test_Load_Bundle'Access);
      Caller.Add_Test (Suite, "Test ASF.Applications.Main.Register,Load_Bundle",
                       Test_Bundle_Configuration'Access);
      Caller.Add_Test (Suite, "Test ASF.Applications.Main.Get_Supported_Locales",
                       Test_Locales'Access);
   end Add_Tests;

   --  ------------------------------
   --  Initialize the test application
   --  ------------------------------
   procedure Set_Up (T : in out Test) is
      Fact : ASF.Applications.Main.Application_Factory;
      C    : ASF.Applications.Config;
   begin
      T.App := new ASF.Applications.Main.Application;
      C.Copy (Util.Tests.Get_Properties);
      T.App.Initialize (C, Fact);
      T.App.Register ("layoutMsg", "layout");
   end Set_Up;

   --  ------------------------------
   --  Deletes the application object
   --  ------------------------------
   overriding
   procedure Tear_Down (T : in out Test) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => ASF.Applications.Main.Application'Class,
                                        Name   => ASF.Applications.Main.Application_Access);
   begin
      Free (T.App);
   end Tear_Down;

   function Create_Form_Bean return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Applications.Tests.Form_Bean_Access := new Applications.Tests.Form_Bean;
   begin
      return Result.all'Access;
   end Create_Form_Bean;

   --  ------------------------------
   --  Test creation of module
   --  ------------------------------
   procedure Test_Read_Configuration (T : in out Test) is
      Path : constant String := Util.Tests.Get_Test_Path ("config/empty.xml");
   begin
      ASF.Applications.Main.Configs.Read_Configuration (T.App.all, Path);

   end Test_Read_Configuration;

   --  ------------------------------
   --  Test creation of a module and registration in an application.
   --  ------------------------------
   procedure Test_Create_Bean (T : in out Test) is
      use type Util.Beans.Basic.Readonly_Bean_Access;

      procedure Check (Name : in String;
                       Kind : in ASF.Beans.Scope_Type);

      procedure Check (Name : in String;
                       Kind : in ASF.Beans.Scope_Type) is
         Value : Util.Beans.Objects.Object;
         Bean  : Util.Beans.Basic.Readonly_Bean_Access;
         Scope : ASF.Beans.Scope_Type;
         Context : EL.Contexts.Default.Default_Context;
      begin
         T.App.Create (Name    => Ada.Strings.Unbounded.To_Unbounded_String (Name),
                       Context => Context,
                       Result  => Bean,
                       Scope   => Scope);
         T.Assert (Kind = Scope, "Invalid scope for " & Name);
         T.Assert (Bean /= null, "Invalid bean object");
         Value := Util.Beans.Objects.To_Object (Bean);
         T.Assert (not Util.Beans.Objects.Is_Null (Value), "Invalid bean");

         --  Special test for the sessionForm bean which is initialized by configuration properties
         if Name = "sessionForm" then
            T.Assert_Equals ("John.Rambo@gmail.com",
                             Util.Beans.Objects.To_String (Bean.Get_Value ("email")),
                             "Session form not initialized");
         end if;
      end Check;

      Path : constant String := Util.Tests.Get_Test_Path ("regtests/config/test-module.xml");

   begin
      T.App.Register_Class ("ASF.Applications.Tests.Form_Bean", Create_Form_Bean'Access);
      ASF.Applications.Main.Configs.Read_Configuration (T.App.all, Path);

      --  Check the 'regtests/config/test-module.xml' managed bean configuration.
      Check ("applicationForm", ASF.Beans.APPLICATION_SCOPE);
      Check ("sessionForm", ASF.Beans.SESSION_SCOPE);
      Check ("requestForm", ASF.Beans.REQUEST_SCOPE);
   end Test_Create_Bean;

   --  ------------------------------
   --  Test loading a resource bundle through the application.
   --  ------------------------------
   procedure Test_Load_Bundle (T : in out Test) is
      use type Util.Beans.Basic.Readonly_Bean_Access;

      Path   : constant String := Util.Tests.Get_Test_Path ("regtests/config/test-bundle.xml");
      Bundle : ASF.Locales.Bundle;
   begin
      ASF.Applications.Main.Configs.Read_Configuration (T.App.all, Path);

      T.App.Load_Bundle (Name   => "samples",
                         Locale => "en",
                         Bundle => Bundle);
      Util.Tests.Assert_Equals (T, "Help", String '(Bundle.Get ("layout_help_label")),
                                "Invalid bundle value");

      T.App.Load_Bundle (Name   => "asf",
                         Locale => "en",
                         Bundle => Bundle);
      Util.Tests.Assert_Matches (T, ".*greater than.*",
                                 String '(Bundle.Get ("validators.length.maximum")),
                                "Invalid bundle value");
   end Test_Load_Bundle;

   --  ------------------------------
   --  Test application configuration and registration of resource bundles.
   --  ------------------------------
   procedure Test_Bundle_Configuration (T : in out Test) is
      use type Util.Beans.Basic.Readonly_Bean_Access;

      Path    : constant String := Util.Tests.Get_Test_Path ("regtests/config/test-bundle.xml");
      Result  : Util.Beans.Basic.Readonly_Bean_Access;
      Context : aliased ASF.Contexts.Faces.Faces_Context;
      Ctx     : aliased EL.Contexts.Default.Default_Context;
      Scope   : Scope_Type;
   begin
      Context.Set_ELContext (Ctx'Unchecked_Access);
      ASF.Applications.Main.Configs.Read_Configuration (T.App.all, Path);

      T.App.Create (Name    => Ada.Strings.Unbounded.To_Unbounded_String ("samplesMsg"),
                    Context => Ctx,
                    Result  => Result,
                    Scope   => Scope);
      T.Assert (Result /= null, "The samplesMsg bundle was not created");

      T.App.Create (Name    => Ada.Strings.Unbounded.To_Unbounded_String ("defaultMsg"),
                    Context => Ctx,
                    Result  => Result,
                    Scope   => Scope);
      T.Assert (Result /= null, "The defaultMsg bundle was not created");
   end Test_Bundle_Configuration;

   --  ------------------------------
   --  Test locales.
   --  ------------------------------
   procedure Test_Locales (T : in out Test) is
      use Util.Locales;

      Path      : constant String := Util.Tests.Get_Test_Path ("regtests/config/test-locales.xml");
      Req       : aliased ASF.Requests.Mockup.Request;
      View      : constant access Applications.Views.View_Handler'Class := T.App.Get_View_Handler;
      Context   : aliased ASF.Contexts.Faces.Faces_Context;
      ELContext : aliased EL.Contexts.Default.Default_Context;
      Locale    : Util.Locales.Locale;
   begin
      ASF.Applications.Main.Configs.Read_Configuration (T.App.all, Path);

      Util.Tests.Assert_Equals (T, To_String (Util.Locales.FRENCH),
                                To_String (T.App.Get_Default_Locale),
                                "Invalid default locale");

      Context.Set_ELContext (ELContext'Unchecked_Access);
      Context.Set_Request (Req'Unchecked_Access);
      Req.Set_Header ("Accept-Language", "da, en-gb;q=0.3, fr;q=0.7");
      T.App.Set_Context (Context'Unchecked_Access);
      Locale := View.Calculate_Locale (Context);

      Util.Tests.Assert_Equals (T, To_String (Util.Locales.FRENCH),
                                To_String (Locale),
                                "Invalid calculated locale");

      Req.Set_Header ("Accept-Language", "da, en-gb, en;q=0.8, fr;q=0.7");
      T.App.Set_Context (Context'Unchecked_Access);
      Locale := View.Calculate_Locale (Context);

      Util.Tests.Assert_Equals (T, "en_GB",
                                To_String (Locale),
                                "Invalid calculated locale");

      Req.Set_Header ("Accept-Language", "da, fr;q=0.7, fr-fr;q=0.8");
      T.App.Set_Context (Context'Unchecked_Access);
      Locale := View.Calculate_Locale (Context);

      Util.Tests.Assert_Equals (T, "fr_FR",
                                To_String (Locale),
                                "Invalid calculated locale");

      Req.Set_Header ("Accept-Language", "da, ru, it;q=0.8, de;q=0.7");
      T.App.Set_Context (Context'Unchecked_Access);
      Locale := View.Calculate_Locale (Context);

      Util.Tests.Assert_Equals (T, To_String (Util.Locales.FRENCH),
                                To_String (Locale),
                                "Invalid calculated locale");
   end Test_Locales;

end ASF.Applications.Main.Tests;
