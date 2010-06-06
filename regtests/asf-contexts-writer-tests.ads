-----------------------------------------------------------------------
--  Writer Tests - Unit tests for ASF.Contexts.Writer
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

with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package ASF.Contexts.Writer.Tests is

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite);

   type Test_Writer is new ResponseWriter with record
      Response : Unbounded_String;
   end record;
   type Test_Writer_Access is access all Test_Writer'Class;

   overriding
   procedure Write (Stream : in out Test_Writer;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   type Test is new AUnit.Test_Fixtures.Test_Fixture with record
      Writer : Test_Writer_Access;
   end record;

   overriding
   procedure Set_Up (T : in out Test);
   --  Set up performed before each test case

   overriding
   procedure Tear_Down (T : in out Test);
   --  Tear down performed after each test case

   procedure Test_Write_Element (T : in out Test);

end ASF.Contexts.Writer.Tests;
