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
with Util.Serialize.IO;
with ASF.Requests;
with ASF.Responses;
with ASF.Servlets;
with EL.Contexts;
with Security.Permissions;

--  == REST ==
--  The <tt>ASF.Rest</tt> package provides support to implement easily some RESTful API.
package ASF.Rest is

   subtype Request is ASF.Requests.Request;

   subtype Response is ASF.Responses.Response;

   subtype Output_Stream is Util.Serialize.IO.Output_Stream;

   --  The HTTP rest method.
   type Method_Type is (GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT, OPTIONS);

   type Descriptor is abstract tagged limited private;
   type Descriptor_Access is access all Descriptor'Class;

   --  Get the permission index associated with the REST operation.
   function Get_Permission (Handler : in Descriptor)
                            return Security.Permissions.Permission_Index;

   --  Dispatch the request to the API handler.
   procedure Dispatch (Handler : in Descriptor;
                       Req     : in out ASF.Rest.Request'Class;
                       Reply   : in out ASF.Rest.Response'Class;
                       Stream  : in out Output_Stream'Class) is abstract;

   type Operation_Access is
      access procedure (Req    : in out ASF.Rest.Request'Class;
                        Reply  : in out ASF.Rest.Response'Class;
                        Stream : in out ASF.Rest.Output_Stream'Class);

   --  Register the API definition in the servlet registry.
   procedure Register (Registry   : in out ASF.Servlets.Servlet_Registry'Class;
                       Definition : in Descriptor_Access);

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

   --  Register the list of API descriptors for a given servlet and a root path.
   procedure Register (Registry  : in out ASF.Servlets.Servlet_Registry;
                       Name      : in String;
                       URI       : in String;
                       ELContext : in EL.Contexts.ELContext'Class;
                       List      : in Descriptor_Access);

   type Static_Descriptor is new Descriptor with record
      Handler : Operation_Access;
   end record;

   --  Dispatch the request to the API handler.
   overriding
   procedure Dispatch (Handler : in Static_Descriptor;
                       Req     : in out ASF.Rest.Request'Class;
                       Reply   : in out ASF.Rest.Response'Class;
                       Stream  : in out Output_Stream'Class);

end ASF.Rest;
