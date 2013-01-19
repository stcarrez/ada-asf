-----------------------------------------------------------------------
--  asf-security-tests - Unit tests for ASF.Security
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

with Util.Tests;

package ASF.Security.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the security filter granting permission for a given URI.
   procedure Test_Security_Filter (T : in out Test);

   --  Test the security filter grants access to anonymous allowed pages.
   procedure Test_Anonymous_Access (T : in out Test);

private

   --  Check that the given URI reports the HTTP status.
   procedure Check_Security (T      : in out Test;
                             URI    : in String;
                             Result : in Natural);

end ASF.Security.Tests;
