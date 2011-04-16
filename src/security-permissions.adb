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

with Ada.Containers.Vectors;

with Util.Beans.Objects;
with Util.Beans.Objects.Vectors;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.Mappers.Vector_Mapper;

--  The <b>Security.Permissions</b> package defines the different permissions that can be
--  checked by the access control manager.
package body Security.Permissions is

   --  ------------------------------
   --  Permission Manager
   --  ------------------------------

   type Rule_Fields is (FIELD_ROLE, FIELD_URL_PATTERN);

   type Rule is record
      Roles    : Util.Beans.Objects.Vectors.Vector;
      Patterns : Util.Beans.Objects.Vectors.Vector;
   end record;
   type Rule_Access is access all Rule;

   package Rule_Vector is
     new Ada.Containers.Vectors (Element_Type => Rule,
                                 Index_Type   => Natural);

   procedure Set_Member (P     : in out Rule;
                         Field : in Rule_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_ROLE =>
            P.Roles.Append (Value);

         when FIELD_URL_PATTERN =>
            P.Patterns.Append (Value);

      end case;
   end Set_Member;

   package Rule_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Rule,
                                               Element_Type_Access => Rule_Access,
                                               Fields              => Rule_Fields,
                                               Set_Member          => Set_Member);

   package Rule_Vector_Mapper is
     new Util.Serialize.Mappers.Vector_Mapper (Vectors        => Rule_Vector,
                                               Element_Mapper => Rule_Mapper);

   --  ------------------------------
   --  Returns True if the user has the permission to access the given URI permission.
   --  ------------------------------
   function Has_Permission (Manager    : in Permission_Manager;
                            User       : in Principal'Class;
                            Permission : in URI_Permission'Class) return Boolean is
      Pos : constant Rules_Maps.Cursor := Manager.Rules.Find (Permission.URI'Unrestricted_Access);
   begin
      if not Rules_Maps.Has_Element (Pos) then
         return False;
      end if;
      declare
         Rule : constant Access_Rule := Rules_Maps.Element (Pos);
      begin
         for I in 1 .. Rule.Last loop
            if User.Has_Permission (Rule.Permissions (I)) then
               return True;
            end if;
         end loop;
      end;
      return False;
   end Has_Permission;

   --  ------------------------------
   --  Returns True if the user has the given role permission.
   --  ------------------------------
   function Has_Permission (Manager    : in Permission_Manager;
                            User       : in Principal'Class;
                            Permission : in Permission_Type) return Boolean is
   begin
      return User.Has_Permission (Permission);
   end Has_Permission;

   --  ------------------------------
   --  Get the permission name.
   --  ------------------------------
   function Get_Permission_Name (Manager    : in Permission_Manager;
                                 Permission : in Permission_Type) return String is
   begin
      if Manager.Names (Permission) = null then
         return "";
      else
         return Manager.Names (Permission).all;
      end if;
   end Get_Permission_Name;

   --  ------------------------------
   --  Create a permission
   --  ------------------------------
   procedure Create_Permission (Manager    : in out Permission_Manager;
                                Name       : in String;
                                Permission : out Permission_Type) is
   begin
      Permission := Manager.Next_Permission;
      Manager.Next_Permission := Manager.Next_Permission + 1;
      Manager.Names (Permission) := new String '(Name);
   end Create_Permission;

   --  Grant the permission to access to the given <b>URI</b> to users having the <b>To</b>
   --  permissions.
   procedure Grant_URI_Permission (Manager : in out Permission_Manager;
                                   URI     : in String;
                                   To      : in String) is
   begin
      null;
   end Grant_URI_Permission;

   --  Grant the permission to access to the given <b>Path</b> to users having the <b>To</b>
   --  permissions.
   procedure Grant_File_Permission (Manager : in out Permission_Manager;
                                    Path    : in String;
                                    To      : in String) is
   begin
      null;
   end Grant_File_Permission;

end Security.Permissions;
