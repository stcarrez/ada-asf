-----------------------------------------------------------------------
--  Security-permissions-tests - Unit tests for Security.Permissions
--  Copyright (C) 2011 Stephane Carrez
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

package Security.Permissions.Tests is

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test Create_Permission and Get_Permission_Name
   procedure Test_Create_Permission (T : in out Test);

   --  Test Has_Permission
   procedure Test_Has_Permission (T : in out Test);

   --  Test reading policy files
   procedure Test_Read_Policy (T : in out Test);

   type Test_Principal is new Principal with record
      Name  : Util.Strings.String_Ref;
      Roles : Permission_Map := (others => false);
   end record;

   --  Returns true if the given permission is stored in the user principal.
   function Has_Permission (User       : in Test_Principal;
                            Permission : in Permission_Type) return Boolean;

   --  Get the principal name.
   function Get_Name (From : in Test_Principal) return String;

end Security.Permissions.Tests;
