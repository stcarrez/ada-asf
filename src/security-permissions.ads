-----------------------------------------------------------------------
--  security-permissions -- Definition of permissions
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

with Ada.Finalization;
with Ada.Containers.Hashed_Maps;

with Util.Strings;

--  The <b>Security.Permissions</b> package defines the different permissions that can be
--  checked by the access control manager.
package Security.Permissions is

   --  The permission root class.
   type Abstract_Permission is abstract tagged limited null record;


   --  Each permission is represented by a <b>Permission_Type</b> number to provide a fast
   --  and efficient permission check.
   type Permission_Type is new Positive range 1 .. 64;

   --  The <b>Permission_Map</b> represents a set of permissions which are granted to a user.
   --  Each permission is represented by a boolean in the map.  The implementation is limited
   --  to 64 permissions.
   type Permission_Map is array (Permission_Type'Range) of Boolean;
   pragma Pack (Permission_Map);

   --  ------------------------------
   --  Principal
   --  ------------------------------
   type Principal is limited interface;
   type Principal_Access is access all Principal'Class;

   --  Returns true if the given permission is stored in the user principal.
   function Has_Permission (User       : in Principal;
                            Permission : in Permission_Type) return Boolean is abstract;

   --  Get the principal name.
   function Get_Name (From : in Principal) return String is abstract;

   --  ------------------------------
   --  Permission
   --  ------------------------------

   --  Represents a permission for a given role.
   type Permission (Role : Permission_Type) is new Abstract_Permission with null record;

   --  ------------------------------
   --  URI Permission
   --  ------------------------------
   --  Represents a permission to access a given URI.
   type URI_Permission (Len : Natural) is new Abstract_Permission with record
      URI : String (1 .. Len);
   end record;

   --  ------------------------------
   --  File Permission
   --  ------------------------------

   type File_Mode is (READ, WRITE);

   --  Represents a permission to access a given file.
   type File_Permission (Len  : Natural;
                         Mode : File_Mode) is new Abstract_Permission with record
      Path : String (1 .. Len);
   end record;

   --  ------------------------------
   --  Permission Manager
   --  ------------------------------
   --  The <b>Permission_Manager</b> verifies through some policy that a permission
   --  is granted to a user.
   type Permission_Manager is new Ada.Finalization.Limited_Controlled with private;

   --  Returns True if the user has the permission to access the given URI permission.
   function Has_Permission (Manager    : in Permission_Manager;
                            User       : in Principal'Class;
                            Permission : in URI_Permission'Class) return Boolean;

   --  Returns True if the user has the given role permission.
   function Has_Permission (Manager    : in Permission_Manager;
                            User       : in Principal'Class;
                            Permission : in Permission_Type) return Boolean;

   --  Get the permission name.
   function Get_Permission_Name (Manager    : in Permission_Manager;
                                 Permission : in Permission_Type) return String;

   --  Create a permission
   procedure Create_Permission (Manager    : in out Permission_Manager;
                                Name       : in String;
                                Permission : out Permission_Type);

   --  Grant the permission to access to the given <b>URI</b> to users having the <b>To</b>
   --  permissions.
   procedure Grant_URI_Permission (Manager : in out Permission_Manager;
                                   URI     : in String;
                                   To      : in String);

   --  Grant the permission to access to the given <b>Path</b> to users having the <b>To</b>
   --  permissions.
   procedure Grant_File_Permission (Manager : in out Permission_Manager;
                                    Path    : in String;
                                    To      : in String);

private

   use Util.Strings;

   type Permission_Name_Array is array (Permission_Type'Range) of Util.Strings.Name_Access;

   type Permission_Type_Array is array (1 .. 10) of Permission_Type;

   type Access_Rule is record
      Permissions : Permission_Type_Array;
      Last        : Natural;
   end record;

   package Rules_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => Name_Access,
                                                         Element_Type    => Access_Rule,
                                                         Hash            => Hash,
                                                         Equivalent_Keys => Equivalent_Keys);

   type Permission_Manager is new Ada.Finalization.Limited_Controlled with record
      Names           : Permission_Name_Array;
      Rules           : Rules_Maps.Map;
      Next_Permission : Permission_Type := Permission_Type'First;
   end record;

end Security.Permissions;
