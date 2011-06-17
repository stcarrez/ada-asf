-----------------------------------------------------------------------
--  Sessions Tests - Unit tests for ASF.Sessions
--  Copyright (C) 2010, 2011 Stephane Carrez
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

with AUnit.Test_Suites; use AUnit.Test_Suites;
with Util.Tests;

package ASF.Sessions.Tests is

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite);

   type Test is new Util.Tests.Test with record
      Writer    : Integer;
   end record;

   --  Test creation of session
   procedure Test_Create_Session (T : in out Test);
   procedure Test_Empty_Session (T : in out Test);
   procedure Test_Session_Attributes (T : in out Test);

end ASF.Sessions.Tests;
