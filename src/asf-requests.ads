-----------------------------------------------------------------------
--  asf.requests -- ASF Requests
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
with EL.Objects;
with Util.Locales;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Util.Beans.Objects.Maps;

with Ada.Finalization;

with ASF.Cookies;
with ASF.Sessions;
with ASF.Responses;
with ASF.Principals;
limited with ASF.Servlets;

--  The <b>ASF.Requests</b> package is an Ada implementation of
--  the Java servlet request (JSR 315 3. The Request).
package ASF.Requests is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Multi part content
   --  ------------------------------
   --  The <b>Part</b> type describes a mime part received in a request.
   --  The content is stored in a file and several operations are provided
   --  to manage the content.
   type Part is abstract new Ada.Finalization.Limited_Controlled with private;

   --  Get the size of the mime part.
   function Get_Size (Data : in Part) return Natural;

   --  Get the content name submitted in the mime part.
   function Get_Name (Data : in Part) return String;

   --  Get the content type of the part.
   function Get_Content_Type (Data : in Part) return String;

   --  Get
--     function Get_Stream (Data : in Part) return Util.Stream.Buffer;
   procedure Save (Data : in Part;
                   Path : in String);

   --  ------------------------------
   --  Request
   --  ------------------------------
   --  The <b>Request</b> type describes a web request that a servlet can process.
   type Request is abstract new Ada.Finalization.Limited_Controlled with private;
   type Request_Access is access all Request'Class;

   --  Returns the value of the named attribute as an Object, or null if no attribute
   --  of the given name exists.
   --
   --  Attributes can be set two ways. The servlet container may set attributes to make
   --  available custom information about a request. For example, for requests made
   --  using HTTPS, the attribute javax.servlet.request.X509Certificate can be used
   --  to retrieve information on the certificate of the client. Attributes can also
   --  be set programatically using setAttribute(String, Object).
   --  This allows information to be embedded into a request before
   --  a RequestDispatcher call.
   function Get_Attribute (Req  : in Request;
                           Name : in String) return EL.Objects.Object;

   --  Stores an attribute in this request. Attributes are reset between requests.
   --  This method is most often used in conjunction with RequestDispatcher.
   --
   --  If the object passed in is null, the effect is the same as calling
   --  removeAttribute(java.lang.String).  It is warned that when the request is
   --  dispatched from the servlet resides in a different web application by
   --  RequestDispatcher, the object set by this method may not be correctly
   --  retrieved in the caller servlet.
   procedure Set_Attribute (Req   : in out Request;
                            Name  : in String;
                            Value : in EL.Objects.Object);

   --  Removes an attribute from this request. This method is not generally needed
   --  as attributes only persist as long as the request is being handled.
   procedure Remove_Attribute (Req  : in out Request;
                               Name : in String);

   --  Iterate over the request attributes and executes the <b>Process</b> procedure.
   procedure Iterate_Attributes (Req     : in Request;
                                 Process : not null access
                                   procedure (Name : in String;
                                              Value : in EL.Objects.Object));

   --  Returns the name of the character encoding used in the body of this request.
   --  This method returns null if the request does not specify a character encoding
   function Get_Character_Encoding (Req : in Request) return String;

   --  Overrides the name of the character encoding used in the body of this request.
   --  This method must be called prior to reading request parameters or reading input
   --  using getReader(). Otherwise, it has no effect.
   procedure Set_Character_Encoding (Req : in out Request;
                                     Encoding : in String);

   --  Returns the length, in bytes, of the request body and made available by the
   --  input stream, or -1 if the length is not known. For HTTP servlets,
   --  same as the value of the CGI variable CONTENT_LENGTH.
   function Get_Content_Length (Req : in Request) return Integer;

   --  Returns the MIME type of the body of the request, or null if the type is
   --  not known. For HTTP servlets, same as the value of the CGI variable CONTENT_TYPE.
   function Get_Content_Type (Req : in Request) return String;

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
   --
   function Get_Parameter (Req  : in Request;
                           Name : in String)
                           return String is abstract;

   --  Returns an array of String objects containing all of the values the given
   --  request parameter has, or null if the parameter does not exist.
   --
   --  If the parameter has a single value, the array has a length of 1.
   function Get_Parameter_Values (Req  : in Request;
                                  Name : in String) return String;

   --  Returns the name and version of the protocol the request uses in the form
   --  protocol/majorVersion.minorVersion, for example, HTTP/1.1. For HTTP servlets,
   --  the value returned is the same as the value of the CGI variable SERVER_PROTOCOL.
   function Get_Protocol (Req : in Request) return String;

   --  Returns the name of the scheme used to make this request, for example, http,
   --  https, or ftp. Different schemes have different rules for constructing URLs,
   --  as noted in RFC 1738.
   function Get_Scheme (Req : in Request) return String;

   --  Returns the host name of the server to which the request was sent.
   --  It is the value of the part before ":" in the Host  header value, if any,
   --  or the resolved server name, or the server IP address.
   function Get_Server_Name (Req : in Request) return String;

   --  Returns the port number to which the request was sent. It is the value of the
   --  part after ":" in the Host  header value, if any, or the server port where the
   --  client connection was accepted on.
   function Get_Server_Port (Req : in Request) return Natural;

   --  Returns the Internet Protocol (IP) address of the client or last proxy that
   --  sent the request. For HTTP servlets, same as the value of the CGI variable
   --  REMOTE_ADDR.
   function Get_Remote_Addr (Req : in Request) return String;

   --  Returns the fully qualified name of the client or the last proxy that sent
   --  the request. If the engine cannot or chooses not to resolve the hostname
   --  (to improve performance), this method returns the dotted-string form of the
   --  IP address. For HTTP servlets, same as the value of the CGI variable REMOTE_HOST.
   function Get_Remote_Host (Req : in Request) return String;

   --  Returns the preferred Locale that the client will accept content in, based
   --  on the Accept-Language header. If the client request doesn't provide an
   --  Accept-Language header, this method returns the default locale for the server.
   function Get_Locale (Req : in Request) return Util.Locales.Locale;

   --  Returns an Enumeration of Locale objects indicating, in decreasing order
   --  starting with the preferred locale, the locales that are acceptable to the
   --  client based on the Accept-Language header. If the client request doesn't
   --  provide an Accept-Language header, this method returns an Enumeration containing
   --  one Locale, the default locale for the server.
   function Get_Locales (Req : in Request) return Util.Locales.Locale;

   --  Returns a boolean indicating whether this request was made using a secure
   --  channel, such as HTTPS.
   function Is_Secure (Req : in Request) return Boolean;

   --  Returns the Internet Protocol (IP) source port of the client or last proxy
   --  that sent the request.
   function Get_Remote_Port (Req : in Request) return Natural;

   --  Returns the host name of the Internet Protocol (IP) interface on which
   --  the request was received.
   function Get_Local_Name (Req : in Request) return String;

   --  Returns the Internet Protocol (IP) address of the interface on which the
   --  request was received.
   function Get_Local_Addr (Req : in Request) return String;

   --  Returns the Internet Protocol (IP) port number of the interface on which
   --  the request was received.
   function Get_Local_Port (Req : in Request) return Natural;

   --  Returns the name of the authentication scheme used to protect the servlet.
   --  All servlet containers support basic, form and client certificate authentication,
   --  and may additionally support digest authentication. If the servlet is not
   --  authenticated null is returned.
   function Get_Auth_Type (Req : in Request) return String;

   --  Returns an array containing all of the Cookie  objects the client sent with
   --  this request. This method returns null if no cookies were sent.
   function Get_Cookies (Req : in Request) return ASF.Cookies.Cookie_Array;

   --  Iterate over the request cookies and executes the <b>Process</b> procedure.
   procedure Iterate_Cookies (Req     : in Request;
                              Process : not null access
                                procedure (Cookie : in ASF.Cookies.Cookie));

   --  Returns the value of the specified request header as a long value that
   --  represents a Date object. Use this method with headers that contain dates,
   --  such as If-Modified-Since.
   --
   --  The date is returned as the number of milliseconds since January 1, 1970 GMT.
   --  The header name is case insensitive.
   --
   --  If the request did not have a header of the specified name, this method
   --  returns -1. If the header can't be converted to a date, the method throws
   --  an IllegalArgumentException.
   function Get_Date_Header (Req  : in Request;
                             Name : in String) return Ada.Calendar.Time;

   --  Returns the value of the specified request header as a String. If the request
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any request header.
   function Get_Header (Req  : in Request;
                        Name : in String) return String;

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

   --  Returns the value of the specified request header as an int. If the request
   --  does not have a header of the specified name, this method returns -1.
   --  If the header cannot be converted to an integer, this method throws
   --  a NumberFormatException.
   --
   --  The header name is case insensitive.
   function Get_Int_Header (Req  : in Request;
                            Name : in String) return Integer;

   --  Iterate over the request headers and executes the <b>Process</b> procedure.
   procedure Iterate_Headers (Req     : in Request;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is abstract;

   --  Returns the name of the HTTP method with which this request was made,
   --  for example, GET, POST, or PUT. Same as the value of the CGI variable
   --  REQUEST_METHOD.
   function Get_Method (Req : in Request) return String;

   --  Returns any extra path information associated with the URL the client sent when
   --  it made this request. The extra path information follows the servlet path but
   --  precedes the query string and will start with a "/" character.
   function Get_Path_Info (Req : in Request) return String;

   --  Returns the portion of the request URI that indicates the context of the
   --  request. The context path always comes first in a request URI. The path
   --  starts with a "/" character but does not end with a "/" character.
   --  For servlets in the default (root) context, this method returns "".
   --  The container does not decode this string.
   function Get_Context_Path (Req : in Request) return String;

   --  Returns the query string that is contained in the request URL after the path.
   --  This method returns null  if the URL does not have a query string. Same as the
   --  value of the CGI variable QUERY_STRING.
   function Get_Query_String (Req : in Request) return String;

   --  Returns the login of the user making this request, if the user has been
   --  authenticated, or null if the user has not been authenticated. Whether
   --  the user name is sent with each subsequent request depends on the browser
   --  and type of authentication. Same as the value of the CGI variable REMOTE_USER.
   function Get_Remote_User (Req : in Request) return String;

   --  Returns a Principal object containing the name of the current
   --  authenticated user. If the user has not been authenticated, the method returns null.
   function Get_User_Principal (Req : in Request) return ASF.Principals.Principal_Access;

   --  Returns the session ID specified by the client. This may not be the same as
   --  the ID of the current valid session for this request. If the client did not
   --  specify a session ID, this method returns null.
   function Get_Request_Session_Id (Req : in Request) return String;

   --  Returns the part of this request's URL from the protocol name up to the query
   --  string in the first line of the HTTP request. The web container does not decode
   --  this String. For example:
   --  First line of HTTP request    Returned Value
   --  POST /some/path.html HTTP/1.1        /some/path.html
   --  GET http://foo.bar/a.html HTTP/1.0       /a.html
   --  HEAD /xyz?a=b HTTP/1.1       /xyz
   function Get_Request_URI (Req : in Request) return String;

   --  Reconstructs the URL the client used to make the request. The returned URL
   --  contains a protocol, server name, port number, and server path, but it does
   --  not include query string parameters.
   --
   --  If this request has been forwarded using RequestDispatcher.forward(Request, Response),
   --  the server path in the reconstructed URL must reflect the path used to
   --  obtain the RequestDispatcher, and not the server path specified by the client.
   --
   --  Because this method returns a StringBuffer, not a string, you can modify the
   --  URL easily, for example, to append query parameters.
   --
   --  This method is useful for creating redirect messages and for reporting errors.
   function Get_Request_URL (Req : in Request) return Ada.Strings.Unbounded.Unbounded_String;

   --  Returns the part of this request's URL that calls the servlet. This path starts
   --  with a "/" character and includes either the servlet name or a path to the
   --  servlet, but does not include any extra path information or a query string.
   --  Same as the value of the CGI variable SCRIPT_NAME.
   --
   --  This method will return an empty string ("") if the servlet used to process
   --  this request was matched using the "/*" pattern.
   function Get_Servlet_Path (Req : in Request) return String;

   --  Returns the current HttpSession  associated with this request or, if there
   --  is no current session and create is true, returns a new session.
   --
   --  If create is false  and the request has no valid HttpSession, this method
   --  returns null.
   --
   --  To make sure the session is properly maintained, you must call this method
   --  before the response is committed. If the container is using cookies to maintain
   --  session integrity and is asked to create a new session when the response is
   --  committed, an IllegalStateException is thrown.
   function Get_Session (Req    : in Request;
                         Create : in Boolean := False) return ASF.Sessions.Session;

   --  Set the path info
   procedure Set_Path_Info (Req  : in out Request;
                            Path : in String);

   --  Get the number of parts included in the request.
   function Get_Part_Count (Req : in Request) return Natural is abstract;

   --  Process the part at the given position and executes the <b>Process</b> operation
   --  with the part object.
   procedure Process_Part (Req      : in out Request;
                           Position : in Positive;
                           Process  : not null access
                             procedure (Data : in Part'Class)) is abstract;

   --  Returns True if the request is an AJAX request.
   function Is_Ajax_Request (Req : in Request) return Boolean;

   --  Returns the absolute path of the resource identified by the given relative path.
   --  The resource is searched in a list of directories configured by the application.
   --  The path must begin with a "/" and is interpreted as relative to the current
   --  context root.
   --
   --  This method allows the servlet container to make a resource available to
   --  servlets from any source.
   --
   --  This method returns an empty string if the resource could not be localized.
   function Get_Resource (Req  : in Request;
                          Path : in String) return String;

   --  Initialize the request object.
   overriding
   procedure Initialize (Req : in out Request);

   --  Finalize the request object.
   overriding
   procedure Finalize (Req : in out Request);

private

   --  Make sure the cookies are loaded in the request object.
   procedure Load_Cookies (Req : in Request'Class);

   --  Get and check the request session
   function Has_Session (Req : in Request'Class) return Boolean;

   type Request_Data is record
      --  The session
      Session             : ASF.Sessions.Session;

      --  Indicates whether the session object is known.
      Session_Initialized : Boolean := False;

      --  The response object associated with the request.
      Response            : ASF.Responses.Response_Access;

      --  The request cookies.
      Cookies             : ASF.Cookies.Cookie_Array_Access := null;
   end record;
   type Request_Data_Access is access Request_Data;

   type Request is abstract new Ada.Finalization.Limited_Controlled with record
      Attributes : Util.Beans.Objects.Maps.Map;
      Path_Info  : Unbounded_String;
      Servlet    : access ASF.Servlets.Servlet'Class;
      Info       : Request_Data_Access := null;
   end record;

   type Part is abstract new Ada.Finalization.Limited_Controlled with record
      Path         : Unbounded_String;
      Size         : Natural;
      Name         : Unbounded_String;
      Content_Type : Unbounded_String;
   end record;

end ASF.Requests;
