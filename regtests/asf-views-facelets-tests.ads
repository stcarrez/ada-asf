-----------------------------------------------------------------------
--  Facelet Tests - Unit tests for ASF.Views.Facelets
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

package ASF.Views.Facelets.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with record
      Writer    : Integer;
   end record;

   overriding
   procedure Set_Up (T : in out Test);
   --  Set up performed before each test case

   overriding
   procedure Tear_Down (T : in out Test);
   --  Tear down performed after each test case

   --  Test loading of facelet file
   procedure Test_Load_Facelet (T : in out Test);

   --  Test loading of an unknown file
   procedure Test_Load_Unknown_Facelet (T : in out Test);

end ASF.Views.Facelets.Tests;
