-----------------------------------------------------------------------
--  asf-rest -- REST Support
--  Copyright (C) 2016 Stephane Carrez
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

package body ASF.Rest.Definition is

   overriding
   procedure Dispatch (Handler : in Descriptor;
                       Req     : in out ASF.Rest.Request'Class;
                       Reply   : in out ASF.Rest.Response'Class) is
      Object : Object_Type;
   begin
      Handler.Handler (Object, Req, Reply);
   end Dispatch;

   package body Definition is
      P : aliased String := Pattern;
   begin
      Instance.Method     := Method;
      Instance.Permission := Permission;
      Instance.Handler    := Handler;
      Instance.Pattern    := P'Access;
      ASF.Rest.Register (Entries, Instance'Access);
   end Definition;

end ASF.Rest.Definition;
