-----------------------------------------------------------------------
--  asf-clients-files -- HTTP Clients with file implementation (test only)
--  Copyright (C) 2011 Stephane Carrez
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

with Ada.Strings.Unbounded;
package ASF.Clients.Files is

   --  Register the Http manager.
   procedure Register;

   --  Set the path of the file that contains the response for the next
   --  <b>Do_Get</b> and <b>Do_Post</b> calls.
   procedure Set_File (Path : in String);

private

   type File_Http_Manager is new Http_Manager with record
      File : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type File_Http_Manager_Access is access all File_Http_Manager'Class;

   procedure Create (Manager  : in File_Http_Manager;
                     Http     : in out Client'Class);

   procedure Do_Get (Manager  : in File_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Reply    : out Response'Class);

   procedure Do_Post (Manager  : in File_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Data     : in String;
                      Reply    : out Response'Class);

   type File_Http_Request is new Http_Request with record
      Headers : Natural; --  AWS.Headers.List;
   end record;
   type File_Http_Request_Access is access all File_Http_Request'Class;

   --  Returns a boolean indicating whether the named request header has already
   --  been set.
   function Contains_Header (Http : in File_Http_Request;
                             Name : in String) return Boolean;

   --  Sets a request header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   procedure Set_Header (Http  : in out File_Http_Request;
                         Name  : in String;
                         Value : in String);

   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   procedure Add_Header (Http  : in out File_Http_Request;
                         Name  : in String;
                         Value : in String);

   type File_Http_Response is new Http_Response with record
      Data : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type File_Http_Response_Access is access all File_Http_Response'Class;

   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   function Contains_Header (Reply : in File_Http_Response;
                             Name  : in String) return Boolean;

   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   function Get_Header (Reply  : in File_Http_Response;
                        Name   : in String) return String;

   --  Get the response body as a string.
   function Get_Body (Reply : in File_Http_Response) return String;

end ASF.Clients.Files;
