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

with EL.Functions;
package ASF.Security is

   --  EL function name exposed by Set_Functions.
   HAS_PERMISSION_FN  : constant String := "hasPermission";

   --  URI for the EL functions exposed by the security package (See Set_Functions).
   AUTH_NAMESPACE_URI : constant String := "http://code.google.com/p/ada-asf/auth";

   --  Register a set of functions in the namespace
   --  xmlns:fn="http://code.google.com/p/ada-asf/auth"
   --  Functions:
   --    hasPermission(NAME)   --  Returns True if the permission NAME is granted
   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class);

end ASF.Security;
