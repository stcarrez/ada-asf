-----------------------------------------------------------------------
--  asf.requests.mockup -- ASF Requests mockup
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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
package body ASF.Requests.Mockup is

   function Find (Map  : in Util.Strings.Maps.Map;
                  Name : in String) return String;

   --  ------------------------------
   --  Find and return the string associated with a key in the map.
   --  ------------------------------
   function Find (Map  : in Util.Strings.Maps.Map;
                  Name : in String) return String is
      Pos : constant Util.Strings.Maps.Cursor := Map.Find (Name);
   begin
      if Util.Strings.Maps.Has_Element (Pos) then
         return Util.Strings.Maps.Element (Pos);
      else
         return "";
      end if;
   end Find;

   --  ------------------------------
   --  Returns the value of a request parameter as a String, or null if the
   --  parameter does not exist. Request parameters are extra information sent with
   --  the request. For HTTP servlets, parameters are contained in the query string
   --  or posted form data.
   --
   --  You should only use this method when you are sure the parameter has only one
   --  value. If the parameter might have more than one value, use
   --  Get_Parameter_Values(String).
   --
   --  If you use this method with a multivalued parameter, the value returned is
   --  equal to the first value in the array returned by Get_Parameter_Values.
   --
   --  If the parameter data was sent in the request body, such as occurs with
   --  an HTTP POST request, then reading the body directly via getInputStream()
   --  or getReader() can interfere with the execution of this method.
   --  ------------------------------
   function Get_Parameter (Req  : in Request;
                           Name : in String) return String is
   begin
      return Find (Req.Parameters, Name);
   end Get_Parameter;

   --  ------------------------------
   --  Set the parameter
   --  ------------------------------
   procedure Set_Parameter (Req   : in out Request;
                            Name  : in String;
                            Value : in String) is
   begin
      Req.Parameters.Include (Name, Value);
   end Set_Parameter;

   --  ------------------------------
   --  Returns the name of the HTTP method with which this request was made,
   --  for example, GET, POST, or PUT. Same as the value of the CGI variable
   --  REQUEST_METHOD.
   --  ------------------------------
   function Get_Method (Req : in Request) return String is
   begin
      return To_String (Req.Method);
   end Get_Method;

   --  ------------------------------
   --  Sets the HTTP method.
   --  ------------------------------
   procedure Set_Method (Req    : in out Request;
                         Method : in String) is
   begin
      Req.Method := To_Unbounded_String (Method);
   end Set_Method;

   --  ------------------------------
   --  Returns the name and version of the protocol the request uses in the form
   --  protocol/majorVersion.minorVersion, for example, HTTP/1.1. For HTTP servlets,
   --  the value returned is the same as the value of the CGI variable SERVER_PROTOCOL.
   --  ------------------------------
   function Get_Protocol (Req : in Request) return String is
   begin
      return To_String (Req.Protocol);
   end Get_Protocol;

   --  ------------------------------
   --  Sets the protocol version
   --  ------------------------------
   procedure Set_Protocol (Req      : in out Request;
                           Protocol : in String) is
   begin
      Req.Protocol := To_Unbounded_String (Protocol);
   end Set_Protocol;

   --  ------------------------------
   --  Returns the part of this request's URL from the protocol name up to the query
   --  string in the first line of the HTTP request. The web container does not decode
   --  this String. For example:
   --  First line of HTTP request    Returned Value
   --  POST /some/path.html HTTP/1.1        /some/path.html
   --  GET http://foo.bar/a.html HTTP/1.0       /a.html
   --  HEAD /xyz?a=b HTTP/1.1       /xyz
   --  ------------------------------
   function Get_Request_URI (Req : in Request) return String is
   begin
      return To_String (Req.URI);
   end Get_Request_URI;

   --  ------------------------------
   --  Set the request URI.
   --  ------------------------------
   procedure Set_Request_URI (Req : in out Request;
                              URI : in String) is
   begin
      Req.URI := To_Unbounded_String (URI);
   end Set_Request_URI;

   --  ------------------------------
   --  Returns the value of the specified request header as a String. If the request
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any request header.
   --  ------------------------------
   function Get_Header (Req  : in Request;
                        Name : in String) return String is
   begin
      return Find (Req.Headers, Name);
   end Get_Header;

   --  ------------------------------
   --  Sets the header
   --  ------------------------------
   procedure Set_Header (Req   : in out Request;
                         Name  : in String;
                         Value : in String) is
   begin
      Req.Headers.Include (Name, Value);
   end Set_Header;

   --  ------------------------------
   --  Returns all the values of the specified request header as an Enumeration
   --  of String objects.
   --
   --  Some headers, such as Accept-Language can be sent by clients as several headers
   --  each with a different value rather than sending the header as a comma
   --  separated list.
   --
   --  If the request did not include any headers of the specified name, this method
   --  returns an empty Enumeration. The header name is case insensitive. You can use
   --  this method with any request header.
   --  ------------------------------
   function Get_Headers (Req  : in Request;
                         Name : in String) return String is
   begin
      return Find (Req.Headers, Name);
   end Get_Headers;

   --  ------------------------------
   --  Iterate over the request headers and executes the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate_Headers (Req     : in Request;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is

      procedure Process_Wrapper (Position : in Util.Strings.Maps.Cursor);

      procedure Process_Wrapper (Position : in Util.Strings.Maps.Cursor) is
      begin
         Process.all (Name  => Util.Strings.Maps.Key (Position),
                      Value => Util.Strings.Maps.Element (Position));
      end Process_Wrapper;

   begin
      Req.Headers.Iterate (Process => Process_Wrapper'Access);
   end Iterate_Headers;

   --  ------------------------------
   --  Returns the Internet Protocol (IP) address of the client or last proxy that
   --  sent the request. For HTTP servlets, same as the value of the CGI variable
   --  REMOTE_ADDR.
   --  ------------------------------
   function Get_Remote_Addr (Req : in Request) return String is
   begin
      return To_String (Req.Peer);
   end Get_Remote_Addr;

   --  ------------------------------
   --  Sets the peer address
   --  ------------------------------
   procedure Set_Remote_Addr (Req  : in out Request;
                              Addr : in String) is
   begin
      Req.Peer := To_Unbounded_String (Addr);
   end Set_Remote_Addr;

   --  ------------------------------
   --  Get the number of parts included in the request.
   --  ------------------------------
   function Get_Part_Count (Req : in Request) return Natural is
      pragma Unreferenced (Req);
   begin
      return 0;
   end Get_Part_Count;

   --  ------------------------------
   --  Process the part at the given position and executes the <b>Process</b> operation
   --  with the part object.
   --  ------------------------------
   procedure Process_Part (Req      : in out Request;
                           Position : in Positive;
                           Process  : not null access
                             procedure (Data : in ASF.Parts.Part'Class)) is
   begin
      null;
   end Process_Part;

   --  ------------------------------
   --  Process the part identifed by <b>Id</b> and executes the <b>Process</b> operation
   --  with the part object.
   --  ------------------------------
   procedure Process_Part (Req      : in out Request;
                           Id       : in String;
                           Process  : not null access
                             procedure (Data : in ASF.Parts.Part'Class)) is
   begin
      null;
   end Process_Part;

   --  ------------------------------
   --  Set the request cookie by using the cookie returned in the response.
   --  ------------------------------
   procedure Set_Cookie (Req  : in out Request;
                         From : in ASF.Responses.Mockup.Response'Class) is
      C : constant String := From.Get_Header ("Set-Cookie");
   begin
      Req.Set_Header ("Cookie", C);
   end Set_Cookie;

end ASF.Requests.Mockup;
