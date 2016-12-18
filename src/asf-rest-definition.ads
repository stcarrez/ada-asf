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
                                   Reply  : in out ASF.Rest.Response'Class);
   end record;

   overriding
   procedure Dispatch (Handler : in Descriptor;
                       Req     : in out ASF.Rest.Request'Class;
                       Reply   : in out ASF.Rest.Response'Class);

   Entries : ASF.Rest.Descriptor_Access;

   generic
      Handler    : access procedure (Object : in out Object_Type;
                                     Req    : in out ASF.Rest.Request'Class;
                                     Reply  : in out ASF.Rest.Response'Class);
      Method     : Method_Type;
      Pattern    : String;
      Permission : Security.Permissions.Permission_Index;
   package Definition is
      Instance : aliased Descriptor;
   end Definition;

end ASF.Rest.Definition;
