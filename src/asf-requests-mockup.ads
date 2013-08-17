-----------------------------------------------------------------------
--  asf.requests.mockup -- ASF Requests mockup
--  Copyright (C) 2010, 2011, 2012, 2013 Stephane Carrez
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
with Util.Strings.Maps;
with ASF.Responses.Mockup;

--  The <b>ASF.Requests.Mockup</b> provides a fake request object to simulate
--  an HTTP request.
package ASF.Requests.Mockup is

   --  ------------------------------
   --  Request Mockup
   --  ------------------------------
   --  The request mockup provides additional procedures to set the request
   --  parameters, the URI, the peer address and other read-only values.
   type Request is new ASF.Requests.Request with private;
   type Request_Access is access all Request'Class;

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
   function Get_Parameter (Req  : Request;
                           Name : String) return String;

   --  Iterate over the request parameters and executes the <b>Process</b> procedure.
   procedure Iterate_Parameters (Req     : in Request;
                                 Process : not null access
                                   procedure (Name  : in String;
                                              Value : in String));

   --  Set the parameter
   procedure Set_Parameter (Req   : in out Request;
                            Name  : in String;
                            Value : in String);

   --  Returns the name of the HTTP method with which this request was made,
   --  for example, GET, POST, or PUT. Same as the value of the CGI variable
   --  REQUEST_METHOD.
   function Get_Method (Req : in Request) return String;

   --  Sets the HTTP method.
   procedure Set_Method (Req : in out Request;
                         Method : in String);

   --  Returns the name and version of the protocol the request uses in the form
   --  protocol/majorVersion.minorVersion, for example, HTTP/1.1. For HTTP servlets,
   --  the value returned is the same as the value of the CGI variable SERVER_PROTOCOL.
   function Get_Protocol (Req : in Request) return String;

   --  Sets the protocol version
   procedure Set_Protocol (Req      : in out Request;
                           Protocol : in String);

   --  Returns the part of this request's URL from the protocol name up to the query
   --  string in the first line of the HTTP request. The web container does not decode
   --  this String. For example:
   --  First line of HTTP request    Returned Value
   --  POST /some/path.html HTTP/1.1        /some/path.html
   --  GET http://foo.bar/a.html HTTP/1.0       /a.html
   --  HEAD /xyz?a=b HTTP/1.1       /xyz
   function Get_Request_URI (Req : in Request) return String;

   --  Set the request URI.
   procedure Set_Request_URI (Req : in out Request;
                              URI : in String);

   --  Returns the value of the specified request header as a String. If the request
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any request header.
   function Get_Header (Req  : in Request;
                        Name : in String) return String;

   --  Sets the header
   procedure Set_Header (Req   : in out Request;
                         Name  : in String;
                         Value : in String);

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
   function Get_Headers (Req  : in Request;
                         Name : in String) return String;

   --  Iterate over the request headers and executes the <b>Process</b> procedure.
   procedure Iterate_Headers (Req     : in Request;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String));

   --  Returns the Internet Protocol (IP) address of the client or last proxy that
   --  sent the request. For HTTP servlets, same as the value of the CGI variable
   --  REMOTE_ADDR.
   function Get_Remote_Addr (Req : in Request) return String;

   --  Sets the peer address
   procedure Set_Remote_Addr (Req  : in out Request;
                              Addr : in String);

   --  Get the number of parts included in the request.
   function Get_Part_Count (Req : in Request) return Natural;

   --  Process the part at the given position and executes the <b>Process</b> operation
   --  with the part object.
   procedure Process_Part (Req      : in out Request;
                           Position : in Positive;
                           Process  : not null access
                             procedure (Data : in ASF.Parts.Part'Class));

   --  Process the part identifed by <b>Id</b> and executes the <b>Process</b> operation
   --  with the part object.
   procedure Process_Part (Req      : in out Request;
                           Id       : in String;
                           Process  : not null access
                             procedure (Data : in ASF.Parts.Part'Class));

   --  Set the request cookie by using the cookie returned in the response.
   procedure Set_Cookie (Req  : in out Request;
                         From : in ASF.Responses.Mockup.Response'Class);

private

   type Request is new ASF.Requests.Request with record
      Headers    : Util.Strings.Maps.Map;
      Parameters : Util.Strings.Maps.Map;
      URI        : Unbounded_String;
      Protocol   : Unbounded_String;
      Method     : Unbounded_String;
      Peer       : Unbounded_String;
   end record;

end ASF.Requests.Mockup;
