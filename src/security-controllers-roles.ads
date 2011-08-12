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

with Security.Contexts;
with Security.Permissions;

package Security.Controllers.Roles is

   --  ------------------------------
   --  Security Controller
   --  ------------------------------
   --  The <b>Role_Controller</b> implements a simple role based permissions check.
   --  The permission is granted if the user has the role defined by the controller.
   type Role_Controller is limited new Controller with record
      Role : Security.Permissions.Permission_Type;
   end record;
   type Role_Controller_Access is access all Role_Controller'Class;

   --  Returns true if the user associated with the security context <b>Context</b> has
   --  the role defined in the <b>Handler</b>.
   function Has_Permission (Handler : in Role_Controller;
                            Context : in Security.Contexts.Security_Context'Class)
                            return Boolean;

   --  Create the role controller instance.
   function Create_Role_Controller return Controller_Access;

end Security.Controllers.Roles;
