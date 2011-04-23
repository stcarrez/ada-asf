-----------------------------------------------------------------------
--  ASF testsuite - Ada Server Faces Test suite
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
with ASF.Contexts.Writer.Tests;
with ASF.Views.Facelets.Tests;
with ASF.Applications.Views.Tests;
with ASF.Sessions.Tests;
with ASF.Servlets.Tests;
with ASF.Contexts.Faces.Tests;
with ASF.Cookies.Tests;
with ASF.Applications.Tests;
with Security.Openid.Tests;
with Security.Permissions.Tests;
package body ASF.Testsuite is

   Tests : aliased Test_Suite;

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := Tests'Access;
   begin
      ASF.Cookies.Tests.Add_Tests (Ret);
      ASF.Contexts.Writer.Tests.Add_Tests (Ret);
      ASF.Contexts.Faces.Tests.Add_Tests (Ret);
      ASF.Views.Facelets.Tests.Add_Tests (Ret);
      ASF.Applications.Views.Tests.Add_Tests (Ret);
      ASF.Sessions.Tests.Add_Tests (Ret);
      ASF.Servlets.Tests.Add_Tests (Ret);

      Security.Openid.Tests.Add_Tests (Ret);
      Security.Permissions.Tests.Add_Tests (Ret);

      --  Run the application tests at the end.
      ASF.Applications.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end ASF.Testsuite;
