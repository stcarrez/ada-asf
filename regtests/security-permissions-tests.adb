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
with Util.Tests;

with Util.Files;
with Util.Test_Caller;
with Util.Measures;

package body Security.Permissions.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Security.Permissions.Create_Permission",
                       Test_Create_Permission'Access);
      Caller.Add_Test (Suite, "Test Security.Permissions.Has_Permission",
                       Test_Has_Permission'Access);
      Caller.Add_Test (Suite, "Test Security.Permissions.Read_Policy",
                       Test_Read_Policy'Access);
   end Add_Tests;

   --  ------------------------------
   --  Returns true if the given permission is stored in the user principal.
   --  ------------------------------
   function Has_Permission (User       : in Test_Principal;
                            Permission : in Permission_Type) return Boolean is
   begin
      return User.Roles (Permission);
   end Has_Permission;

   --  ------------------------------
   --  Get the principal name.
   --  ------------------------------
   function Get_Name (From : in Test_Principal) return String is
   begin
      return Util.Strings.To_String (From.Name);
   end Get_Name;

   --  ------------------------------
   --  Test Create_Permission and Get_Permission_Name
   --  ------------------------------
   procedure Test_Create_Permission (T : in out Test) is
      M    : Security.Permissions.Permission_Manager;
      Perm : Permission_Type;
   begin
      M.Create_Permission (Name       => "admin",
                           Permission => Perm);
      Assert_Equals (T, "admin", M.Get_Permission_Name (Perm), "Invalid name");

      for I in Perm + 1 .. Permission_Type'Last loop
         declare
            Name : constant String := "admin-" & Permission_Type'Image (I);
         begin
            M.Create_Permission (Name       => Name,
                                 Permission => Perm);
            Assert_Equals (T, Name, M.Get_Permission_Name (Perm), "Invalid name");
         end;
      end loop;
   end Test_Create_Permission;

   --  ------------------------------
   --  Test Has_Permission
   --  ------------------------------
   procedure Test_Has_Permission (T : in out Test) is
      M    : Security.Permissions.Permission_Manager;
      Perm : Permission_Type;
      User : Test_Principal;
   begin
      T.Assert (not M.Has_Permission (User, 1), "User has a non-existing permission");

      User.Roles (1) := True;
      T.Assert (M.Has_Permission (User, 1), "User does not have the permission");
   end Test_Has_Permission;

   --  ------------------------------
   --  Test reading policy files
   --  ------------------------------
   procedure Test_Read_Policy (T : in out Test) is
      M : Security.Permissions.Permission_Manager;
      Dir         : constant String := "regtests/files/permissions/";
      Path        : constant String := Util.Tests.Get_Path (Dir);
      User        : Test_Principal;
      Admin_Perm  : Permission_Type;
   begin
      M.Read_Policy (Util.Files.Compose (Path, "empty.xml"));

      M.Read_Policy (Util.Files.Compose (Path, "simple-policy.xml"));

      M.Build_Permission_Type (Name   => "admin",
                               Result => Admin_Perm);
      User.Roles (Admin_Perm) := True;

      declare
         S : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            declare
               URI : constant String := "/admin/home/" & Util.Strings.Image (I) & "/l.html";
               P   : constant URI_Permission (URI'Length)
                 := URI_Permission '(Len => URI'Length, URI => URI);
            begin
               T.Assert (M.Has_Permission (User       => User,
                                           Permission => P), "Permission not granted");
            end;
         end loop;
         Util.Measures.Report (S, "Has_Permission (1000 calls, cache miss)");
      end;

      declare
         S : Util.Measures.Stamp;
      begin
         for I in 1 .. 1_000 loop
            declare
               URI : constant String := "/admin/home/list.html";
               P   : constant URI_Permission (URI'Length)
                 := URI_Permission '(Len => URI'Length, URI => URI);
            begin
               T.Assert (M.Has_Permission (User       => User,
                                           Permission => P), "Permission not granted");
            end;
         end loop;
         Util.Measures.Report (S, "Has_Permission (1000 calls, cache hit)");
      end;

   end Test_Read_Policy;

end Security.Permissions.Tests;
