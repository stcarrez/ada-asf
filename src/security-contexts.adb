-----------------------------------------------------------------------
--  security-contexts -- Context to provide security information and verify permissions
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

with Ada.Task_Attributes;

package body Security.Contexts is

   package Task_Context is new Ada.Task_Attributes
     (Security_Context_Access, null);

   --  ------------------------------
   --  Get the application associated with the current service operation.
   --  ------------------------------
   function Get_User_Principal (Ctx : in Security_Context)
                                return Security.Permissions.Principal_Access is
   begin
      return Ctx.Principal;
   end Get_User_Principal;

   --  ------------------------------
   --  Check if the permission identified by <b>Permission</b> is allowed according to
   --  the current security context.  The result is cached in the security context and
   --  returned in <b>Result</b>.
   --  ------------------------------
   procedure Has_Permission (Ctx        : in out Security_Context;
                             Permission : in Security.Permissions.Permission_Type;
                             Result     : out Boolean) is
   begin
      Result := False;
   end Has_Permission;

   --  ------------------------------
   --  Check if the permission identified by <b>Permission</b> is allowed according to
   --  the current security context.  The result is cached in the security context and
   --  returned in <b>Result</b>.
   --  ------------------------------
   procedure Has_Permission (Ctx        : in out Security_Context;
                             Permission : in String;
                             Result     : out Boolean) is
   begin
      Result := False;
   end Has_Permission;

   --  ------------------------------
   --  Initializes the service context.  By creating the <b>Security_Context</b> variable,
   --  the instance will be associated with the current task attribute.  If the current task
   --  already has a security context, the new security context is installed, the old one
   --  being kept.
   --  ------------------------------
   overriding
   procedure Initialize (Ctx : in out Security_Context) is
   begin
      Ctx.Previous := Task_Context.Value;
      Task_Context.Set_Value (Ctx'Unchecked_Access);
   end Initialize;

   --  ------------------------------
   --  Finalize the security context releases any object.  The previous security context is
   --  restored to the current task attribute.
   --  ------------------------------
   overriding
   procedure Finalize (Ctx : in out Security_Context) is
   begin
      Task_Context.Set_Value (Ctx.Previous);
   end Finalize;

   --  ------------------------------
   --  Set the current application and user context.
   --  ------------------------------
   procedure Set_Context (Ctx       : in out Security_Context;
                          Manager   : in Security.Permissions.Permission_Manager_Access;
                          Principal : in Security.Permissions.Principal_Access) is
   begin
      Ctx.Manager := Manager;
      Ctx.Principal := Principal;
   end Set_Context;

   --  ------------------------------
   --  Get the current security context.
   --  Returns null if the current thread is not associated with any security context.
   --  ------------------------------
   function Current return Security_Context_Access is
   begin
      return Task_Context.Value;
   end Current;

   --  ------------------------------
   --  Check if the permission identified by <b>Permission</b> is allowed according to
   --  the current security context.  The result is cached in the security context and
   --  returned in <b>Result</b>.
   --  ------------------------------
   function Has_Permission (Permission : in Security.Permissions.Permission_Type) return Boolean is
      Result : Boolean;
   begin
      Current.Has_Permission (Permission, Result);
      return Result;
   end Has_Permission;

   --  ------------------------------
   --  Check if the permission identified by <b>Permission</b> is allowed according to
   --  the current security context.  The result is cached in the security context and
   --  returned in <b>Result</b>.
   --  ------------------------------
   function Has_Permission (Permission : in String) return Boolean is
      Result : Boolean;
   begin
      Current.Has_Permission (Permission, Result);
      return Result;
   end Has_Permission;

end Security.Contexts;
