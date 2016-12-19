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

   --  ------------------------------
   --  Register the list of APIs that have been created by instantiating the <tt>Definition</tt>
   --  package.  The REST servlet identified by <tt>Name</tt> is searched in the servlet registry
   --  and used as the servlet for processing the API requests.
   --  ------------------------------
   procedure Register (Registry  : in out ASF.Servlets.Servlet_Registry;
                       Name      : in String;
                       ELContext : in EL.Contexts.ELContext'Class) is
   begin
      ASF.Rest.Register (Registry => Registry,
                         Name     => Name,
                         URI      => URI,
                         ELContext => ELContext,
                         List     => Entries);
   end Register;

   overriding
   procedure Dispatch (Handler : in Descriptor;
                       Req     : in out ASF.Rest.Request'Class;
                       Reply   : in out ASF.Rest.Response'Class;
                       Stream  : in out ASF.Rest.Output_Stream'Class) is
      Object : Object_Type;
   begin
      Handler.Handler (Object, Req, Reply, Stream);
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
