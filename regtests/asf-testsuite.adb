-----------------------------------------------------------------------
--  ASF testsuite - Ada Server Faces Test suite
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
with AUnit.Assertions;
with ASF.Contexts.Writer.Tests;
with ASF.Views.Facelets.Tests;
with ASF.Applications.Views.Tests;

with AUnit.Reporter.Text;
with AUnit.Run;
package body ASF.Testsuite is

   use AUnit.Assertions;
   procedure Run is
      Ret : aliased Test_Suite;

      function Get_Suite return Access_Test_Suite;

      function Get_Suite return Access_Test_Suite is
      begin
         return Ret'Unchecked_Access;
      end Get_Suite;

      procedure Runner is new AUnit.Run.Test_Runner (Get_Suite);
      Reporter : AUnit.Reporter.Text.Text_Reporter;
   begin
      ASF.Contexts.Writer.Tests.Add_Tests (Ret'Unchecked_Access);
      ASF.Views.Facelets.Tests.Add_Tests (Ret'Unchecked_Access);
      ASF.Applications.Views.Tests.Add_Tests (Ret'Unchecked_Access);

      Runner (Reporter);
   end Run;

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      ASF.Contexts.Writer.Tests.Add_Tests (Ret);
      ASF.Views.Facelets.Tests.Add_Tests (Ret);
      ASF.Applications.Views.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end ASF.Testsuite;
