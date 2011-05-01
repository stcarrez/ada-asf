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

with Util.Files;
package body ASF.Clients.Files is

   use Ada.Strings.Unbounded;

   Manager : aliased File_Http_Manager;

   --  ------------------------------
   --  Register the Http manager.
   --  ------------------------------
   procedure Register is
   begin
      Default_Http_Manager := Manager'Access;
   end Register;

   --  ------------------------------
   --  Set the path of the file that contains the response for the next
   --  <b>Do_Get</b> and <b>Do_Post</b> calls.
   --  ------------------------------
   procedure Set_File (Path : in String) is
   begin
      Manager.File := To_Unbounded_String (Path);
   end Set_File;

   procedure Create (Manager  : in File_Http_Manager;
                     Http     : in out Client'Class) is
      pragma Unreferenced (Manager);
   begin
      Http.Request := new File_Http_Request;
   end Create;


   procedure Do_Get (Manager  : in File_Http_Manager;
                     Http     : in Client'Class;
                     URI      : in String;
                     Reply    : out Response'Class) is
      pragma Unreferenced (Http, URI);

      Rep : constant File_Http_Response_Access := new File_Http_Response;
   begin
      Reply.Data := Rep.all'Access;
      Util.Files.Read_File (Path     => To_String (Manager.File),
                            Into     => Rep.Data,
                            Max_Size => 100000);
      Reply.Status := 200;
   end Do_Get;


   procedure Do_Post (Manager  : in File_Http_Manager;
                      Http     : in Client'Class;
                      URI      : in String;
                      Data     : in String;
                      Reply    : out Response'Class) is
      pragma Unreferenced (Data);
   begin
      Manager.Do_Get (Http, URI, Reply);
   end Do_Post;

   --  ------------------------------
   --  Returns a boolean indicating whether the named request header has already
   --  been set.
   --  ------------------------------
   function Contains_Header (Http : in File_Http_Request;
                             Name : in String) return Boolean is
      pragma Unreferenced (Http, Name);
   begin
      raise Program_Error with "Contains_Header is not implemented";
      return False;
   end Contains_Header;

   --  ------------------------------
   --  Sets a request header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   --  ------------------------------
   procedure Set_Header (Http  : in out File_Http_Request;
                         Name  : in String;
                         Value : in String) is
      pragma Unreferenced (Http, Name, Value);
   begin
      null;
   end Set_Header;

   --  ------------------------------
   --  Adds a request header with the given name and value.
   --  This method allows request headers to have multiple values.
   --  ------------------------------
   procedure Add_Header (Http  : in out File_Http_Request;
                         Name  : in String;
                         Value : in String) is
      pragma Unreferenced (Http, Name, Value);
   begin
      null;
   end Add_Header;

   --  ------------------------------
   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   --  ------------------------------
   function Contains_Header (Reply : in File_Http_Response;
                             Name  : in String) return Boolean is
      pragma Unreferenced (Reply, Name);
   begin
      return False;
   end Contains_Header;

   --  ------------------------------
   --  Returns the value of the specified response header as a String. If the response
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any response header.
   --  ------------------------------
   function Get_Header (Reply  : in File_Http_Response;
                        Name   : in String) return String is
      pragma Unreferenced (Reply, Name);
   begin
      return "";
   end Get_Header;

   --  ------------------------------
   --  Get the response body as a string.
   --  ------------------------------
   function Get_Body (Reply : in File_Http_Response) return String is
   begin
      return To_String (Reply.Data);
   end Get_Body;

end ASF.Clients.Files;
