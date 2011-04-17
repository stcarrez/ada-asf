-----------------------------------------------------------------------
--  EL testsuite - EL Testsuite
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with AUnit.Test_Caller;
with AUnit.Assertions;
with Util.Files;
with Util.Tests;
with Ada.Text_IO;
package body Security.Openid.Tests is

   overriding
   procedure Get_Request (Realm  : in Manager;
                          URI    : in String;
                          Accept_Format : in String;
                          Result : out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Util.Files.Read_File (Path     => To_String (Realm.File),
                            Into     => Result,
                            Max_Size => 100000);
   end Get_Request;

   overriding
   procedure Post_Request (Realm  : in Manager;
                           URI    : in String;
                           Params : in String;
                           Result : out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Post_Request;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Suite.Add_Test (Caller.Create ("Test Security.OpenID.Discover",
        Test_Discovery'Access));

   end Add_Tests;

   procedure Check_Discovery (T    : in out Test;
                              Name : in String;
                              URI  : in String) is
      M : Manager;
      Dir         : constant String := "regtests/files/discover/";
      Path        : constant String := Util.Tests.Get_Path (Dir);
      Result      : End_Point;
   begin
      M.File := To_Unbounded_String (Path & Name & ".xrds");
      M.Discover (Name   => Name,
                  Result => Result);
      Ada.Text_IO.Put_Line ("Result: " & To_String (Result));
   end Check_Discovery;

   procedure Test_Discovery (T : in out Test) is
   begin
      Check_Discovery (T, "google", "https://www.google.com/accounts/o8/ud");
      Check_Discovery (T, "yahoo", "https://open.login.yahooapis.com/openid/op/auth");
      Check_Discovery (T, "claimid", "");
      Check_Discovery (T, "livejournal", "");
      Check_Discovery (T, "myopenid", "");
      Check_Discovery (T, "myspace", "");
      Check_Discovery (T, "orange", "");
      Check_Discovery (T, "verisign", "");
      Check_Discovery (T, "steamcommunity", "");
   end Test_Discovery;

end Security.Openid.Tests;
