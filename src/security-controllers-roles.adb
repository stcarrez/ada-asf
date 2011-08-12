-----------------------------------------------------------------------
--  security-controllers-roles -- Simple role base security
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

package body Security.Controllers.Roles is

   --  ------------------------------
   --  Returns true if the user associated with the security context <b>Context</b> has
   --  the role defined in the <b>Handler</b>.
   --  ------------------------------
   function Has_Permission (Handler : in Role_Controller;
                            Context : in Security.Contexts.Security_Context'Class)
                            return Boolean is
      use type Security.Permissions.Principal_Access;

      P : constant Security.Permissions.Principal_Access := Context.Get_User_Principal;
   begin
      if P = null then
         return False;
      else
         return P.Has_Permission (Handler.Role);
      end if;
   end Has_Permission;

   --  ------------------------------
   --  Create the role controller instance.
   --  ------------------------------
   function Create_Role_Controller return Controller_Access is
      Result : constant Role_Controller_Access := new Role_Controller;
   begin
      return Result.all'Access;
   end Create_Role_Controller;

end Security.Controllers.Roles;
