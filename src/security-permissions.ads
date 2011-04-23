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
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with Util.Strings;
with Util.Refs;
with GNAT.Regexp;

--  The <b>Security.Permissions</b> package defines the different permissions that can be
--  checked by the access control manager.
package Security.Permissions is

   --  The permission root class.
   type Abstract_Permission is abstract tagged limited null record;


   --  Each permission is represented by a <b>Permission_Type</b> number to provide a fast
   --  and efficient permission check.
   type Permission_Type is new Natural range 0 .. 63;

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
   type Permission_Manager_Access is access all Permission_Manager'Class;

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

   --  Get or build a permission type for the given name.
   procedure Build_Permission_Type (Manager   : in out Permission_Manager;
                                    Name      : in String;
                                    Result    : out Permission_Type);

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

   --  Read the policy file
   procedure Read_Policy (Manager : in out Permission_Manager;
                          File    : in String);

   --  Initialize the permission manager.
   overriding
   procedure Initialize (Manager : in out Permission_Manager);

   --  Finalize the permission manager.
   overriding
   procedure Finalize (Manager : in out Permission_Manager);

private

   use Util.Strings;

   type Permission_Name_Array is
     array (Permission_Type'Range) of Ada.Strings.Unbounded.String_Access;

   type Permission_Type_Array is array (1 .. 10) of Permission_Type;

   --  The <b>Access_Rule</b> represents a list of permissions to verify to grant
   --  access to the resource.  To make it simple, the user must have one of the
   --  permission from the list.
   type Access_Rule is record
      Permissions : Permission_Type_Array := (others => Permission_Type'First);
      Last        : Natural := 0;
   end record;

   --  No rule
   No_Rule : constant Access_Rule := (Permissions => (others => Permission_Type'First),
                                      Last        => 0);

   --  Find the access rule of the policy that matches the given URI.
   --  Returns the No_Rule value (disable access) if no rule is found.
   function Find_Access_Rule (Manager : in Permission_Manager;
                              URI     : in String) return Access_Rule;

   --  The <b>Policy</b> defines the access rules that are applied on a given
   --  URL, set of URLs or files.
   type Policy is record
      Id      : Natural;
      Pattern : GNAT.Regexp.Regexp;
      Rule    : Access_Rule;
   end record;

   --  The <b>Policy_Vector</b> represents the whole permission policy.  The order of
   --  policy in the list is important as policies can override each other.
   package Policy_Vector is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Policy);

   package Rules_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => String_Ref,
                                                         Element_Type    => Access_Rule,
                                                         Hash            => Hash,
                                                         Equivalent_Keys => Equivalent_Keys);

   type Rules is new Util.Refs.Ref_Entity with record
      Map : Rules_Maps.Map;
   end record;
   type Rules_Access is access all Rules;

   package Rules_Ref is new Util.Refs.References (Rules, Rules_Access);

   type Rules_Ref_Access is access Rules_Ref.Atomic_Ref;

   type Permission_Manager is new Ada.Finalization.Limited_Controlled with record
      Names           : Permission_Name_Array;
      Cache           : Rules_Ref_Access;
      Next_Permission : Permission_Type := Permission_Type'First;
      Policies        : Policy_Vector.Vector;
   end record;

end Security.Permissions;
