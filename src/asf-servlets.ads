-----------------------------------------------------------------------
--  asf.servlets -- ASF Servlets
--  Copyright (C) 2010 Stephane Carrez
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
with ASF.Requests;
with ASF.Responses;

with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Calendar;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;
private with Util.Properties;

--  The <b>ASF.Servlets</b> package implements a subset of the
--  Java Servlet Specification adapted for the Ada language.
--
--  The rationale for this implementation is to provide a set of
--  interfaces and ways of developing a Web application which
--  benefit from the architecture expertise defined in Java applications.
--
--  The <b>ASF.Servlets</b>, <b>ASF.Requests</b>, <b>ASF.Responses</b>
--  and <b>ASF.Sessions</b> packages are independent of the web server
--  which will be used (such as <b>AWS</b>, <b>Apache</b> or <b>Lighthttpd</b>).
--
package ASF.Servlets is

   Servlet_Error : exception;

   --  Filter chain as defined by JSR 315 6. Filtering
   type Filter_Chain is limited private;

   --  Causes the next filter in the chain to be invoked, or if the calling
   --  filter is the last filter in the chain, causes the resource at the end
   --  of the chain to be invoked.
   procedure Do_Filter (Chain    : in out Filter_Chain;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class);

   type Filter is limited interface;
   type Filter_Access is access all Filter'Class;

   --  The Do_Filter method of the Filter is called by the container each time
   --  a request/response pair is passed through the chain due to a client request
   --  for a resource at the end of the chain.  The Filter_Chain passed in to this
   --  method allows the Filter to pass on the request and response to the next
   --  entity in the chain.
   --
   --  A typical implementation of this method would follow the following pattern:
   --  1. Examine the request
   --  2. Optionally wrap the request object with a custom implementation to
   --     filter content or headers for input filtering
   --  3. Optionally wrap the response object with a custom implementation to
   --     filter content or headers for output filtering
   --  4. Either invoke the next entity in the chain using the FilterChain
   --     object (chain.Do_Filter()),
   --     or, not pass on the request/response pair to the next entity in the
   --     filter chain to block the request processing
   --  5. Directly set headers on the response after invocation of the next
   --     entity in the filter chain.
   procedure Do_Filter (F        : in Filter;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class;
                        Chain    : in out Filter_Chain) is abstract;

   --  type Servlet_Registry;
   type Servlet_Registry is tagged limited private;

   type Servlet_Registry_Access is access all Servlet_Registry'Class;

   --  The <b>Servlet</b> represents the component that will handle
   --  an HTTP request received by the server.
   --
   --  JSR 315 - 2. The Servlet Interface
   type Servlet is tagged limited private;
   type Servlet_Access is access all Servlet'Class;

   --  Get the servlet name.
   function Get_Name (Server : in Servlet) return String;

   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   procedure Initialize (Server  : in out Servlet;
                         Context : in Servlet_Registry'Class);

   --  Receives standard HTTP requests from the public service method and dispatches
   --  them to the Do_XXX methods defined in this class. This method is an HTTP-specific
   --  version of the Servlet.service(Request, Response) method. There's no need
   --  to override this method.
   procedure Service (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   --  Returns the time the Request object was last modified, in milliseconds since
   --  midnight January 1, 1970 GMT.  If the time is unknown, this method returns
   --  a negative number (the default).
   --
   --  Servlets that support HTTP GET requests and can quickly determine their
   --  last modification time should override this method. This makes browser and
   --  proxy caches work more effectively, reducing the load on server and network
   --  resources.
   function Get_Last_Modified (Server  : in Servlet;
                               Request : in Requests.Request'Class)
                               return Ada.Calendar.Time;

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a GET request.
   --
   --  Overriding this method to support a GET request also automatically supports
   --  an HTTP HEAD request. A HEAD request is a GET request that returns no body
   --  in the response, only the request header fields.
   --
   --  When overriding this method, read the request data, write the response headers,
   --  get the response's writer or output stream object, and finally, write the
   --  response data. It's best to include content type and encoding.
   --  When using a PrintWriter object to return the response, set the content type
   --  before accessing the PrintWriter object.
   --
   --  The servlet container must write the headers before committing the response,
   --  because in HTTP the headers must be sent before the response body.
   --
   --  Where possible, set the Content-Length header (with the
   --  Response.Set_Content_Length method), to allow the servlet container
   --  to use a persistent connection to return its response to the client,
   --  improving performance. The content length is automatically set if the entire
   --  response fits inside the response buffer.
   --
   --  When using HTTP 1.1 chunked encoding (which means that the response has a
   --  Transfer-Encoding header), do not set the Content-Length header.
   --
   --  The GET method should be safe, that is, without any side effects for which
   --  users are held responsible. For example, most form queries have no side effects.
   --  If a client request is intended to change stored data, the request should use
   --  some other HTTP method.
   --
   --  The GET method should also be idempotent, meaning that it can be safely repeated.
   --  Sometimes making a method safe also makes it idempotent. For example, repeating
   --  queries is both safe and idempotent, but buying a product online or modifying
   --  data is neither safe nor idempotent.
   --
   --  If the request is incorrectly formatted, Do_Get  returns an HTTP "Bad Request"
   procedure Do_Get (Server   : in Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class);

   --  Receives an HTTP HEAD request from the protected service method and handles
   --  the request. The client sends a HEAD request when it wants to see only the
   --  headers of a response, such as Content-Type or Content-Length. The HTTP HEAD
   --  method counts the output bytes in the response to set the Content-Length header
   --  accurately.
   --
   --  If you override this method, you can avoid computing the response body and just
   --  set the response headers directly to improve performance. Make sure that the
   --  Do_Head method you write is both safe and idempotent (that is, protects itself
   --  from being called multiple times for one HTTP HEAD request).
   --
   --  If the HTTP HEAD request is incorrectly formatted, doHead returns an HTTP
   --  "Bad Request" message.
   procedure Do_Head (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a POST request. The HTTP POST method allows the client to send data of unlimited
   --  length to the Web server a single time and is useful when posting information
   --  such as credit card numbers.
   --
   --  When overriding this method, read the request data, write the response headers,
   --  get the response's writer or output stream object, and finally, write the
   --  response data. It's best to include content type and encoding. When using
   --  a PrintWriter object to return the response, set the content type before
   --  accessing the PrintWriter object.
   --
   --  The servlet container must write the headers before committing the response,
   --  because in HTTP the headers must be sent before the response body.
   --
   --  Where possible, set the Content-Length header (with the
   --  Response.Set_Content_Length method), to allow the servlet container to use
   --  a persistent connection to return its response to the client, improving
   --  performance. The content length is automatically set if the entire response
   --  fits inside the response buffer.
   --
   --  When using HTTP 1.1 chunked encoding (which means that the response has a
   --  Transfer-Encoding header), do not set the Content-Length header.
   --
   --  This method does not need to be either safe or idempotent. Operations
   --  requested through POST can have side effects for which the user can be held
   --  accountable, for example, updating stored data or buying items online.
   --
   --  If the HTTP POST request is incorrectly formatted, doPost returns
   --  an HTTP "Bad Request" message.
   procedure Do_Post (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a PUT request. The PUT operation allows a client to place a file on the server
   --  and is similar to sending a file by FTP.
   --
   --  When overriding this method, leave intact any content headers sent with
   --  the request (including Content-Length, Content-Type, Content-Transfer-Encoding,
   --  Content-Encoding, Content-Base, Content-Language, Content-Location,
   --  Content-MD5, and Content-Range). If your method cannot handle a content
   --  header, it must issue an error message (HTTP 501 - Not Implemented) and
   --  discard the request. For more information on HTTP 1.1, see RFC 2616 .
   --
   --  This method does not need to be either safe or idempotent. Operations that
   --  Do_Put performs can have side effects for which the user can be held accountable.
   --  When using this method, it may be useful to save a copy of the affected URL
   --  in temporary storage.
   --
   --  If the HTTP PUT request is incorrectly formatted, Do_Put returns
   --  an HTTP "Bad Request" message.
   procedure Do_Put (Server   : in Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a DELETE request. The DELETE operation allows a client to remove a document
   --  or Web page from the server.
   --
   --  This method does not need to be either safe or idempotent. Operations requested
   --  through DELETE can have side effects for which users can be held accountable.
   --  When using this method, it may be useful to save a copy of the affected URL in
   --  temporary storage.
   --
   --  If the HTTP DELETE request is incorrectly formatted, Do_Delete returns an HTTP
   --  "Bad Request" message.
   procedure Do_Delete (Server   : in Servlet;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class);

   --  Called by the server (via the service method) to allow a servlet to handle a
   --  OPTIONS request. The OPTIONS request determines which HTTP methods the server
   --  supports and returns an appropriate header. For example, if a servlet overrides
   --  Do_Get, this method returns the following header:
   --
   --  Allow: GET, HEAD, TRACE, OPTIONS
   --
   --  There's no need to override this method unless the servlet implements new
   --  HTTP methods, beyond those implemented by HTTP 1.1.
   procedure Do_Options (Server   : in Servlet;
                         Request  : in out Requests.Request'Class;
                         Response : in out Responses.Response'Class);

   --  Called by the server (via the service method) to allow a servlet to handle
   --  a TRACE request. A TRACE returns the headers sent with the TRACE request to
   --  the client, so that they can be used in debugging. There's no need to override
   --  this method.
   procedure Do_Trace (Server   : in Servlet;
                       Request  : in out Requests.Request'Class;
                       Response : in out Responses.Response'Class);

   --  JSR 315 9. Dispatching Requests
   type Request_Dispatcher is limited private;

   --  Forwards a request from a servlet to another resource
   --  (servlet, or HTML file) on the server. This method allows one servlet to do
   --  preliminary processing of a request and another resource to generate the response.
   --
   --  For a Request_Dispatcher obtained via Get_Request_Dispatcher(),
   --  the ServletRequest  object has its path elements and parameters adjusted
   --  to match the path of the target resource.
   --
   --  forward should be called before the response has been committed to the
   --  client (before response body output has been flushed). If the response
   --  already has been committed, this method throws an IllegalStateException.
   --  Uncommitted output in the response buffer is automatically cleared before
   --  the forward.
   --
   --  The request and response parameters must be either the same objects as were
   --  passed to the calling servlet's service method or be subclasses of the
   --  RequestWrapper or ResponseWrapper  classes that wrap them.
   procedure Forward (Dispatcher : in Request_Dispatcher;
                      Request    : in out Requests.Request'Class;
                      Response   : in out Responses.Response'Class);

   --  Includes the content of a resource (servlet, or, HTML file) in the response.
   --  In essence, this method enables programmatic server-side includes.
   --
   --  The Response object has its path elements and parameters remain
   --  unchanged from the caller's. The included servlet cannot change the response
   --  status code or set headers; any attempt to make a change is ignored.
   --
   --  The request and response parameters must be either the same objects as were
   --  passed to the calling servlet's service method or be subclasses of the
   --  RequestWrapper or ResponseWrapper  classes that wrap them.
   procedure Include (Dispatcher : in Request_Dispatcher;
                      Request    : in out Requests.Request'Class;
                      Response   : in out Responses.Response'Class);

--   type Servlet_Registry is tagged limited private;

   --  Returns a Request_Dispatcher object that acts as a wrapper for the resource
   --  located at the given path.  A Request_Dispatcher  object can be used to forward
   --  a request to the resource or to include the resource in a response.
   --  The resource can be dynamic or static.
   function Get_Request_Dispatcher (Context : in Servlet_Registry;
                                    Path    : in String)
                                    return Request_Dispatcher;

   --  Returns a Request_Dispatcher object that acts as a wrapper for the named servlet.
   --
   --  Servlets may be given names via server administration or via a web application
   --  deployment descriptor.  A servlet instance can determine its name using
   --  ServletConfig.getServletName().
   function Get_Name_Dispatcher (Context : in Servlet_Registry;
                                 Name    : in String)
                                 return Request_Dispatcher;

   --  Returns a String containing the value of the named context-wide initialization
   --  parameter, or null if the parameter does not exist.
   --
   --  This method can make available configuration information useful to an entire
   --  "web application". For example, it can provide a webmaster's email address
   --  or the name of a system that holds critical data.
   function Get_Init_Parameter (Context : in Servlet_Registry;
                                Name    : in String) return String;

   --
   procedure Set_Init_Parameter (Context : in out Servlet_Registry;
                                 Name    : in String;
                                 Value   : in String);

   --  Registers the given servlet instance with this ServletContext under
   --  the given servletName.
   --
   --  If this ServletContext already contains a preliminary
   --  ServletRegistration for a servlet with the given servletName,
   --  it will be completed (by assigning the class name of the given
   --  servlet instance to it) and returned.
   procedure Add_Servlet (Registry : in out Servlet_Registry;
                          Name     : in String;
                          Server   : in Servlet_Access);

   procedure Add_Filter (Registry : in out Servlet_Registry;
                         Name     : in String;
                         Filt     : in Filter_Access);

   procedure Add_Filter (Registry : in out Servlet_Registry;
                         Target   : in String;
                         Name     : in String);

   --  Add a servlet mapping with the given pattern
   --  If the URL pattern is already mapped to a different servlet,
   --  no updates will be performed.
   procedure Add_Mapping (Registry : in out Servlet_Registry;
                          Pattern  : in String;
                          Name     : in String);

   --  Add a servlet mapping with the given pattern
   --  If the URL pattern is already mapped to a different servlet,
   --  no updates will be performed.
   procedure Add_Mapping (Registry : in out Servlet_Registry;
                          Pattern  : in String;
                          Server   : in Servlet_Access);

private

   use Ada.Strings.Unbounded;

   type Filter_List is array (Natural range <>) of Filter_Access;
   type Filter_List_Access is access all Filter_List;

   type Mapping_Type is (MAP_URI_NODE, MAP_URI, MAP_WILDCARD, MAP_EXTENSION);

   type Mapping_Node;
   type Mapping_Access is access all Mapping_Node;

   type Mapping_Node is new Ada.Finalization.Limited_Controlled with record
      --  The next mapping node to check if the current node does not match.
      Next_Map         : Mapping_Access;

      --  The first child mapping to check if the current node matches.
      --  (used only for MAP_URI_NODE)
      Child_Map        : Mapping_Access;

      --  The next mapping for the servlet
      Next_Servlet_Map : Mapping_Access;

      --  The URI string component to check (or the extension part)
      URI       : String_Access;

      --  The mapping type
      Map_Type  : Mapping_Type;

      --  The servlet to invoke if there is a match
      Servlet   : Servlet_Access;

      --  The list of filters to invoke before the servlet
      Filters   : Filter_List_Access;
   end record;

   overriding
   procedure Finalize (Map : in out Mapping_Node);

   procedure Traverse (Map : in out Mapping_Node);

   procedure Dump_Map (Map : in Mapping_Node;
                       Indent : in String := "");

   --  Find the servlet and filter mapping that must be used for the given URI.
   --  Search the mapping according to Ch 12/SRV 11. Mapping Requests to Servlets:
   --  o look for an exact match,
   --  o look for the longest match,
   --  o look for an extension
   --  o use the default servlet mapping
   function Find_Mapping (Registry : in Servlet_Registry;
                          URI      : in String) return Mapping_Access;

   type Filter_Chain is limited record
      Filter_Pos : Natural;
      Filters    : Filter_List_Access;
      Servlet    : Servlet_Access;
   end record;

   type Request_Dispatcher is limited record
      Mapping : Mapping_Access := null;
      Filters : Filter_Chain;
   end record;

   type Servlet is new Ada.Finalization.Limited_Controlled with record
      Mappings  : Mapping_Access := null;
      Name      : Unbounded_String;
      Context   : Servlet_Registry_Access := null;
   end record;

   package Filter_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type     => Unbounded_String,
                                            Element_Type => Filter_Access,
                                            Hash         => Ada.Strings.Unbounded.Hash,
                                            Equivalent_Keys => "=");

   package Servlet_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type     => Unbounded_String,
                                            Element_Type => Servlet_Access,
                                            Hash         => Ada.Strings.Unbounded.Hash,
                                            Equivalent_Keys => "=");

   type Servlet_Registry is new Ada.Finalization.Limited_Controlled with record
      Config            : Util.Properties.Manager;
      Servlets          : Servlet_Maps.Map;
      Filters           : Filter_Maps.Map;
      Mappings          : Mapping_Access := null;
      Extension_Mapping : Mapping_Access := null;
   end record;

end ASF.Servlets;
