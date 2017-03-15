-----------------------------------------------------------------------
--  asf-rest-operation -- REST API Operation Definition
--  Copyright (C) 2017 Stephane Carrez
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
   Handler    : Operation_Access;
   Method     : Method_Type := GET;
   URI        : String;
   Permission : Security.Permissions.Permission_Index := Security.Permissions.NONE;
package ASF.Rest.Operation is

   function Definition return Descriptor_Access;

end ASF.Rest.Operation;
