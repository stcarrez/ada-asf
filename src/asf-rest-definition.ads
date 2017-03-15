-----------------------------------------------------------------------
--  asf-rest-definition -- REST API Definition
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
generic
   type Object_Type is limited private;
   URI : String;
package ASF.Rest.Definition is

   type Descriptor is new ASF.Rest.Descriptor with record
      Handler  : access procedure (Object : in out Object_Type;
                                   Req    : in out ASF.Rest.Request'Class;
                                   Reply  : in out ASF.Rest.Response'Class;
                                   Stream : in out ASF.Rest.Output_Stream'Class);
   end record;

   overriding
   procedure Dispatch (Handler : in Descriptor;
                       Req     : in out ASF.Rest.Request'Class;
                       Reply   : in out ASF.Rest.Response'Class;
                       Stream  : in out ASF.Rest.Output_Stream'Class);

   --  Definition of an API operation mapped to a given URI pattern and associated with
   --  the operation handler.
   generic
      Handler    : access procedure (Object : in out Object_Type;
                                     Req    : in out ASF.Rest.Request'Class;
                                     Reply  : in out ASF.Rest.Response'Class;
                                     Stream : in out ASF.Rest.Output_Stream'Class);
      Method     : Method_Type := ASF.Rest.GET;
      Pattern    : String;
      Permission : Security.Permissions.Permission_Index := Security.Permissions.NONE;
   package Definition is
      Instance : aliased Descriptor;
   end Definition;

   --  Register the list of APIs that have been created by instantiating the <tt>Definition</tt>
   --  package.  The REST servlet identified by <tt>Name</tt> is searched in the servlet registry
   --  and used as the servlet for processing the API requests.
   procedure Register (Registry  : in out ASF.Servlets.Servlet_Registry;
                       Name      : in String;
                       ELContext : in EL.Contexts.ELContext'Class);

private

   Entries : ASF.Rest.Descriptor_Access;

end ASF.Rest.Definition;
