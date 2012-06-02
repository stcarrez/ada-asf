-----------------------------------------------------------------------
--  asf-security -- ASF Security
--  Copyright (C) 2012 Stephane Carrez
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
with Util.Beans.Objects;
with Security.Contexts; use Security;
package body ASF.Security is

   function Has_Permission (Value : in Util.Beans.Objects.Object)
                            return Util.Beans.Objects.Object;

   --  ------------------------------
   --  EL function to check if the given permission name is granted by the current
   --  security context.
   --  ------------------------------
   function Has_Permission (Value : in Util.Beans.Objects.Object)
                            return Util.Beans.Objects.Object is
      Name   : constant String := Util.Beans.Objects.To_String (Value);
   begin
      if Contexts.Has_Permission (Name) then
         return Util.Beans.Objects.To_Object (True);
      else
         return Util.Beans.Objects.To_Object (False);
      end if;
   end Has_Permission;

   --  ------------------------------
   --  Register a set of functions in the namespace
   --  xmlns:fn="http://code.google.com/p/ada-asf/auth"
   --  Functions:
   --    hasPermission(NAME)   --  Returns True if the permission NAME is granted
   --  ------------------------------
   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class) is
   begin
      Mapper.Set_Function (Name      => HAS_PERMISSION_FN,
                           Namespace => AUTH_NAMESPACE_URI,
                           Func      => Has_Permission'Access);
   end Set_Functions;

end ASF.Security;
