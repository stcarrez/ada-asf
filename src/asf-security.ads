-----------------------------------------------------------------------
--  asf-security -- ASF Security
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
