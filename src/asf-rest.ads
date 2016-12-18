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
with Util.Strings;
with ASF.Requests;
with ASF.Responses;
with Security.Permissions;

--  == REST ==
--  The <tt>ASF.Rest</tt> package provides support to implement easily some RESTful API.
package ASF.Rest is

   type Request is abstract new ASF.Requests.Request with null record;

   type Response is abstract new ASF.Responses.Response with null record;

   --  The HTTP rest method.
   type Method_Type is (GET, HEAD, POST, PUT, DELETE, OPTIONS);

   type Descriptor is abstract tagged limited private;
   type Descriptor_Access is access all Descriptor'Class;

   --  Get the permission index associated with the REST operation.
   function Get_Permission (Handler : in Descriptor)
                            return Security.Permissions.Permission_Index;

   --  Dispatch the request to the API handler.
   procedure Dispatch (Handler : in Descriptor;
                       Req     : in out ASF.Rest.Request'Class;
                       Reply   : in out ASF.Rest.Response'Class) is abstract;

private

   type Descriptor is abstract tagged limited record
      Next       : Descriptor_Access;
      Method     : Method_Type;
      Pattern    : Util.Strings.Name_Access;
      Permission : Security.Permissions.Permission_Index := 0;
   end record;

   --  Register the API descriptor in a list.
   procedure Register (List : in out Descriptor_Access;
                       Item : in Descriptor_Access);

end ASF.Rest;
