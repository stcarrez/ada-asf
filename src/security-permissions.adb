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

with Ada.Unchecked_Deallocation;

with Util.Log.Loggers;
with Util.Beans.Objects;
with Util.Beans.Objects.Vectors;
with Util.Serialize.IO.XML;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.Mappers.Vector_Mapper;

--  The <b>Security.Permissions</b> package defines the different permissions that can be
--  checked by the access control manager.
package body Security.Permissions is

   use Util.Log;

   --  ------------------------------
   --  Permission Manager
   --  ------------------------------

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("Security.Permissions");

   --  ------------------------------
   --  Find the access rule of the policy that matches the given URI.
   --  Returns the No_Rule value (disable access) if no rule is found.
   --  ------------------------------
   function Find_Access_Rule (Manager : in Permission_Manager;
                              URI     : in String) return Access_Rule is

      Matched : Boolean := False;
      Result  : Access_Rule;

      procedure Match (P : in Policy);

      procedure Match (P : in Policy) is
      begin
         if GNAT.Regexp.Match (URI, P.Pattern) then
            Matched := True;
            Result  := P.Rule;
         end if;
      end Match;

      Last : constant Natural := Manager.Policies.Last_Index;
   begin
      for I in 1 .. Last loop
         Manager.Policies.Query_Element (I, Match'Access);
         if Matched then
            return Result;
         end if;
      end loop;
      return No_Rule;
   end Find_Access_Rule;

   --  ------------------------------
   --  Returns True if the user has the permission to access the given URI permission.
   --  ------------------------------
   function Has_Permission (Manager    : in Permission_Manager;
                            User       : in Principal'Class;
                            Permission : in URI_Permission'Class) return Boolean is
      Name  : constant String_Ref := To_String_Ref (Permission.URI);
      Ref   : constant Rules_Ref.Ref := Manager.Cache.Get;
      Rules : constant Rules_Access := Ref.Value;
      Pos   : constant Rules_Maps.Cursor := Rules.Map.Find (Name);
      Rule  : Access_Rule;
   begin
      --  If the rule is not in the cache, search for the access rule that
      --  matches our URI.  Update the cache.  This cache update is thread-safe
      --  as the cache map is never modified: a new cache map is installed.
      if not Rules_Maps.Has_Element (Pos) then
         declare
            New_Ref : constant Rules_Ref.Ref := Rules_Ref.Create;
         begin
            Rule := Manager.Find_Access_Rule (Permission.URI);
            New_Ref.Value.all.Map := Rules.Map;
            New_Ref.Value.all.Map.Insert (Name, Rule);
            Manager.Cache.Set (New_Ref);
         end;
      else
         Rule := Rules_Maps.Element (Pos);
      end if;

      --  Check if the user has one of the required permission.
      for I in Rule.Permissions'First .. Rule.Last loop
         if User.Has_Permission (Rule.Permissions (I)) then
            return True;
         end if;
      end loop;
      return False;
   end Has_Permission;

   --  ------------------------------
   --  Returns True if the user has the given role permission.
   --  ------------------------------
   function Has_Permission (Manager    : in Permission_Manager;
                            User       : in Principal'Class;
                            Permission : in Permission_Type) return Boolean is
      pragma Unreferenced (Manager);
   begin
      return User.Has_Permission (Permission);
   end Has_Permission;

   --  ------------------------------
   --  Get the permission name.
   --  ------------------------------
   function Get_Permission_Name (Manager    : in Permission_Manager;
                                 Permission : in Permission_Type) return String is
      use type Ada.Strings.Unbounded.String_Access;
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
      Log.Info ("Permission {0} is {1}", Name, Permission_Type'Image (Permission));

      if Manager.Next_Permission = Permission_Type'Last then
         Log.Error ("Too many permissions allocated.  Number of permissions is {0}",
                    Permission_Type'Image (Permission_Type'Last));
      else
         Manager.Next_Permission := Manager.Next_Permission + 1;
      end if;
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

   --  ------------------------------
   --  Get or build a permission type for the given name.
   --  ------------------------------
   procedure Build_Permission_Type (Manager   : in out Permission_Manager;
                                    Name      : in String;
                                    Result    : out Permission_Type) is
      use type Ada.Strings.Unbounded.String_Access;
   begin
      for I in Permission_Type'First .. Manager.Next_Permission loop
         exit when Manager.Names (I) = null;
         if Name = Manager.Names (I).all then
            Result := I;
            return;
         end if;
      end loop;
      Manager.Create_Permission (Name, Result);
   end Build_Permission_Type;

   type Policy_Fields is (FIELD_ID, FIELD_ROLE, FIELD_URL_PATTERN);

   type Policy_Record is record
      Id       : Natural := 0;
      Roles    : Util.Beans.Objects.Vectors.Vector;
      Patterns : Util.Beans.Objects.Vectors.Vector;
   end record;
   type Policy_Record_Access is access all Policy_Record;

   package Policy_Record_Vector is new Ada.Containers.Vectors (Index_Type => Positive,
                                                               Element_Type => Policy_Record);

   procedure Set_Member (P     : in out Policy_Record;
                         Field : in Policy_Fields;
                         Value : in Util.Beans.Objects.Object);

   procedure Set_Member (P     : in out Policy_Record;
                         Field : in Policy_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_ID =>
            P.Id := Util.Beans.Objects.To_Integer (Value);

         when FIELD_ROLE =>
            P.Roles.Append (Value);

         when FIELD_URL_PATTERN =>
            P.Patterns.Append (Value);

      end case;
   end Set_Member;

   package Policy_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Policy_Record,
                                               Element_Type_Access => Policy_Record_Access,
                                               Fields              => Policy_Fields,
                                               Set_Member          => Set_Member);

   package Policy_Vector_Mapper is
     new Util.Serialize.Mappers.Vector_Mapper (Vectors        => Policy_Record_Vector,
                                               Element_Mapper => Policy_Mapper);

   Policy_Mapping        : aliased Policy_Mapper.Mapper;
   Policy_Vector_Mapping : aliased Policy_Vector_Mapper.Mapper;

   --  ------------------------------
   --  Read the policy file
   --  ------------------------------
   procedure Read_Policy (Manager : in out Permission_Manager;
                          File    : in String) is

      use Util;

      Reader : Util.Serialize.IO.XML.Parser;
      List   : aliased Policy_Vector_Mapper.Vector;

      procedure Process (Policy : in Policy_Record);

      procedure Process (Policy : in Policy_Record) is
         Pol    : Security.Permissions.Policy;
         Iter   : Beans.Objects.Vectors.Cursor := Policy.Roles.First;
      begin
         --  Step 1: Initialize the list of permissions from the roles.
         while Beans.Objects.Vectors.Has_Element (Iter) loop
            declare
               Role : constant Beans.Objects.Object := Beans.Objects.Vectors.Element (Iter);
               Name : constant String := Beans.Objects.To_String (Role);
               Perm : Permission_Type;
            begin
               Manager.Build_Permission_Type (Name, Perm);
               if Pol.Rule.Last > Pol.Rule.Permissions'Last then
                  Log.Error ("Too many roles defined for policy");
               else
                  Pol.Rule.Last := Pol.Rule.Last + 1;
                  Pol.Rule.Permissions (Pol.Rule.Last) := Perm;
               end if;
            end;
            Beans.Objects.Vectors.Next (Iter);
         end loop;

         --  Step 2: Create one policy for each URL pattern
         Iter := Policy.Patterns.First;
         while Beans.Objects.Vectors.Has_Element (Iter) loop
            declare
               Pattern : constant Beans.Objects.Object := Beans.Objects.Vectors.Element (Iter);
            begin
               Pol.Id   := Policy.Id;
               Pol.Pattern := GNAT.Regexp.Compile (Beans.Objects.To_String (Pattern));
               Manager.Policies.Append (Pol);
            end;
            Beans.Objects.Vectors.Next (Iter);
         end loop;
      end Process;

   begin
      Log.Info ("Reading policy file {0}", File);

      Reader.Add_Mapping ("policy-rules/policy", Policy_Vector_Mapping'Access);
      Policy_Vector_Mapper.Set_Context (Reader, List'Unchecked_Access);
      Reader.Parse (File);

      Log.Info ("Found {0} rules in {1}", Ada.Containers.Count_Type'Image (List.Length), File);
      for I in 1 .. List.Last_Index loop
         List.Query_Element (I, Process'Access);
      end loop;
   end Read_Policy;

   --  ------------------------------
   --  Initialize the permission manager.
   --  ------------------------------
   overriding
   procedure Initialize (Manager : in out Permission_Manager) is
   begin
      Manager.Cache := new Rules_Ref.Atomic_Ref;
      Manager.Cache.Set (Rules_Ref.Create);
   end Initialize;

   --  ------------------------------
   --  Finalize the permission manager.
   --  ------------------------------
   overriding
   procedure Finalize (Manager : in out Permission_Manager) is
      use Ada.Strings.Unbounded;
      procedure Free is new Ada.Unchecked_Deallocation (Rules_Ref.Atomic_Ref, Rules_Ref_Access);
   begin
      Free (Manager.Cache);
      for I in Manager.Names'Range loop
         exit when Manager.Names (I) = null;
         Ada.Strings.Unbounded.Free (Manager.Names (I));
      end loop;
   end Finalize;

begin
   Policy_Mapping.Add_Mapping ("@id", FIELD_ID);
   Policy_Mapping.Add_Mapping ("role", FIELD_ROLE);
   Policy_Mapping.Add_Mapping ("url-pattern", FIELD_URL_PATTERN);
   Policy_Vector_Mapping.Set_Mapping (Policy_Mapping'Access);
end Security.Permissions;
