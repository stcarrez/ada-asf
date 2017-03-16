-----------------------------------------------------------------------
--  asf.servlets -- ASF Servlets
--  Copyright (C) 2010, 2011, 2012, 2013, 2015, 2017 Stephane Carrez
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
with System.Address_Image;
with Ada.Unchecked_Deallocation;

with ASF.Filters;
with ASF.Streams;
with ASF.Requests.Tools;
with ASF.Routes.Servlets;

with EL.Objects;
with EL.Contexts.Default;

with GNAT.Traceback.Symbolic;

with Util.Strings;
with Util.Strings.Maps;
with Util.Files;
with Util.Log.Loggers;


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
package body ASF.Servlets is

   use Ada.Finalization;

   use type ASF.Routes.Route_Type_Access;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Servlets");

   No_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year => 1901,
                                                                 Month => 1,
                                                                 Day   => 1);
   --  ------------------------------
   --  Get the filter name.
   --  ------------------------------
   function Get_Filter_Name (Config : in Filter_Config) return String is
   begin
      return To_String (Config.Name);
   end Get_Filter_Name;

   --  ------------------------------
   --  Returns a String containing the value of the named context-wide initialization
   --  parameter, or the default value if the parameter does not exist.
   --
   --  The filter parameter name is automatically prefixed by the filter name followed by '.'.
   --  ------------------------------
   function Get_Init_Parameter (Config  : in Filter_Config;
                                Name    : in String;
                                Default : in String := "") return String is
   begin
      return Config.Context.all.Get_Init_Parameter (To_String (Config.Name) & "." & Name, Default);
   end Get_Init_Parameter;

   function Get_Init_Parameter (Config : in Filter_Config;
                                Name    : in String;
                                Default : in String := "")
                                return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Config.Context.all.Get_Init_Parameter (To_String (Config.Name) & "." & Name, Default);
   end Get_Init_Parameter;

   --  ------------------------------
   --  Get the servlet context associated with the filter config.
   --  ------------------------------
   function Get_Servlet_Context (Config : in Filter_Config) return Servlet_Registry_Access is
   begin
      return Config.Context;
   end Get_Servlet_Context;

   --  ------------------------------
   --  Get the servlet name.
   --  ------------------------------
   function Get_Name (Server : in Servlet) return String is
   begin
      return To_String (Server.Name);
   end Get_Name;

   --  ------------------------------
   --  Get the servlet context associated with this servlet.
   --  ------------------------------
   function Get_Servlet_Context (Server : in Servlet) return Servlet_Registry_Access is
   begin
      return Server.Context;
   end Get_Servlet_Context;

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   procedure Initialize (Server  : in out Servlet;
                         Context : in Servlet_Registry'Class) is
   begin
      null;
   end Initialize;

   --  ------------------------------
   --  Receives standard HTTP requests from the public service method and dispatches
   --  them to the Do_XXX methods defined in this class. This method is an HTTP-specific
   --  version of the Servlet.service(Request, Response) method. There's no need
   --  to override this method.
   --  ------------------------------
   procedure Service (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class) is
      Method : constant String := Request.Get_Method;
   begin
      if Method = "GET" then
         declare
            Last_Mod : constant Ada.Calendar.Time
              := Servlet'Class (Server).Get_Last_Modified (Request);
            pragma Unreferenced (Last_Mod);
         begin
            Servlet'Class (Server).Do_Get (Request, Response);
         end;

      elsif Method = "POST" then
         Servlet'Class (Server).Do_Post (Request, Response);

      elsif Method = "PUT" then
         Servlet'Class (Server).Do_Put (Request, Response);

      elsif Method = "DELETE" then
         Servlet'Class (Server).Do_Delete (Request, Response);

      elsif Method = "HEAD" then
         Servlet'Class (Server).Do_Head (Request, Response);

      elsif Method = "OPTIONS" then
         Servlet'Class (Server).Do_Options (Request, Response);

      elsif Method = "TRACE" then
         Servlet'Class (Server).Do_Trace (Request, Response);

      else
         Response.Send_Error (Responses.SC_NOT_IMPLEMENTED);
      end if;
   end Service;

   --  ------------------------------
   --  Returns the time the Request object was last modified, in milliseconds since
   --  midnight January 1, 1970 GMT.  If the time is unknown, this method returns
   --  a negative number (the default).
   --
   --  Servlets that support HTTP GET requests and can quickly determine their
   --  last modification time should override this method. This makes browser and
   --  proxy caches work more effectively, reducing the load on server and network
   --  resources.
   --  ------------------------------
   function Get_Last_Modified (Server  : in Servlet;
                               Request : in Requests.Request'Class)
                               return Ada.Calendar.Time is
      pragma Unreferenced (Server, Request);
   begin
      return No_Time;
   end Get_Last_Modified;

   --  ------------------------------
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
   --  ------------------------------
   procedure Do_Get (Server   : in Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server, Request);
   begin
      Response.Send_Error (Responses.SC_METHOD_NOT_ALLOWED);
   end Do_Get;

   --  ------------------------------
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
   --  ------------------------------
   procedure Do_Head (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server, Request);
   begin
      null;
   end Do_Head;

   --  ------------------------------
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
   --  ------------------------------
   procedure Do_Post (Server   : in Servlet;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server, Request);
   begin
      Response.Send_Error (Responses.SC_METHOD_NOT_ALLOWED);
   end Do_Post;

   --  ------------------------------
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
   --  ------------------------------
   procedure Do_Put (Server   : in Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server, Request);
   begin
      Response.Send_Error (Responses.SC_METHOD_NOT_ALLOWED);
   end Do_Put;

   --  ------------------------------
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
   --  ------------------------------
   procedure Do_Delete (Server   : in Servlet;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server, Request);
   begin
      Response.Send_Error (Responses.SC_METHOD_NOT_ALLOWED);
   end Do_Delete;

   --  ------------------------------
   --  Called by the server (via the service method) to allow a servlet to handle a
   --  OPTIONS request. The OPTIONS request determines which HTTP methods the server
   --  supports and returns an appropriate header. For example, if a servlet overrides
   --  Do_Get, this method returns the following header:
   --
   --  Allow: GET, HEAD, TRACE, OPTIONS
   --
   --  There's no need to override this method unless the servlet implements new
   --  HTTP methods, beyond those implemented by HTTP 1.1.
   --  ------------------------------
   procedure Do_Options (Server   : in Servlet;
                         Request  : in out Requests.Request'Class;
                         Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server, Request);
   begin
      Response.Send_Error (Responses.SC_METHOD_NOT_ALLOWED);
   end Do_Options;

   --  ------------------------------
   --  Called by the server (via the service method) to allow a servlet to handle
   --  a TRACE request. A TRACE returns the headers sent with the TRACE request to
   --  the client, so that they can be used in debugging. There's no need to override
   --  this method.
   --  ------------------------------
   procedure Do_Trace (Server   : in Servlet;
                       Request  : in out Requests.Request'Class;
                       Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server, Request);
   begin
      Response.Send_Error (Responses.SC_METHOD_NOT_ALLOWED);
   end Do_Trace;

   --  ------------------------------
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
   --  ------------------------------
   procedure Forward (Dispatcher : in Request_Dispatcher;
                      Request    : in out Requests.Request'Class;
                      Response   : in out Responses.Response'Class) is
      use type ASF.Filters.Filter_List_Access;
   begin
      if Dispatcher.Servlet = null then
         Response.Send_Error (Responses.SC_NOT_FOUND);
      elsif Dispatcher.Filters = null then
         ASF.Requests.Tools.Set_Context (Request,
                                         Response'Unchecked_Access,
                                         Dispatcher.Context'Unrestricted_Access);
         Dispatcher.Servlet.Service (Request, Response);
      else
         --  If we have some filters, create the filter chain
         --  and invoke the first filter.
         declare
            Chain : Filter_Chain;
         begin
            ASF.Requests.Tools.Set_Context (Request,
                                            Response'Unchecked_Access,
                                            Dispatcher.Context'Unrestricted_Access);
            Chain.Filters := Dispatcher.Filters.all'Access;
            Chain.Servlet := Dispatcher.Servlet;
            Chain.Filter_Pos := Chain.Filters'Last;
            Do_Filter (Chain    => Chain,
                       Request  => Request,
                       Response => Response);
         end;
      end if;
   end Forward;

   --  ------------------------------
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
   --  ------------------------------
   procedure Include (Dispatcher : in Request_Dispatcher;
                      Request    : in out Requests.Request'Class;
                      Response   : in out Responses.Response'Class) is
   begin
      if Dispatcher.Servlet = null then
         Response.Send_Error (Responses.SC_NOT_FOUND);
      else
         Dispatcher.Servlet.Service (Request, Response);
      end if;
   end Include;

   --  ------------------------------
   --  Returns the servlet that will be called when forwarding the request.
   --  ------------------------------
   function Get_Servlet (Dispatcher : in Request_Dispatcher) return Servlet_Access is
   begin
      return Dispatcher.Servlet;
   end Get_Servlet;

   --  ------------------------------
   --  Returns a Request_Dispatcher object that acts as a wrapper for the resource
   --  located at the given path.  A Request_Dispatcher  object can be used to forward
   --  a request to the resource or to include the resource in a response.
   --  The resource can be dynamic or static.
   --  ------------------------------
   function Get_Request_Dispatcher (Context : in Servlet_Registry;
                                    Path    : in String)
                                    return Request_Dispatcher is
      use type ASF.Filters.Filter_List_Access;

      Route : ASF.Routes.Route_Type_Access;
   begin
      return R : Request_Dispatcher do
         Context.Routes.Find_Route (Path, R.Context);
         Route := ASF.Routes.Get_Route (R.Context);
         if Route /= null then
            if Route.all in ASF.Routes.Servlets.Servlet_Route_Type'Class then
               declare
                  Servlet_Route : constant ASF.Routes.Servlets.Servlet_Route_Type_Access
                    := ASF.Routes.Servlets.Servlet_Route_Type'Class (Route.all)'Access;
                  Proxy : ASF.Routes.Servlets.Proxy_Route_Type_Access;
               begin
                  if Servlet_Route.Filters /= null then
                     R.Filters := Servlet_Route.Filters.all'Access;
                  end if;
                  if Servlet_Route.all in ASF.Routes.Servlets.Proxy_Route_Type'Class then
                     Proxy := Routes.Servlets.Proxy_Route_Type'Class (Servlet_Route.all)'Access;
                     ASF.Routes.Change_Route (R.Context, Proxy.Route.all'Access);
                     R.Servlet := Proxy.Route.Servlet;
                  else
                     R.Servlet := Servlet_Route.Servlet;
                  end if;
               end;
            end if;
         end if;
         R.Pos := ASF.Routes.Get_Path_Pos (R.Context);
      end return;
   end Get_Request_Dispatcher;

   --  ------------------------------
   --  Returns a Request_Dispatcher object that acts as a wrapper for the named servlet.
   --
   --  Servlets may be given names via server administration or via a web application
   --  deployment descriptor.  A servlet instance can determine its name using
   --  ServletConfig.getServletName().
   --  ------------------------------
   function Get_Name_Dispatcher (Context : in Servlet_Registry;
                                 Name    : in String)
                                 return Request_Dispatcher is
      Pos : constant Servlet_Maps.Cursor := Context.Servlets.Find (Name);
   begin
      if not Servlet_Maps.Has_Element (Pos) then
         raise Servlet_Error with "No servlet " & Name;
      end if;
      return R : Request_Dispatcher do
         R.Servlet := Servlet_Maps.Element (Pos);
      end return;
   end Get_Name_Dispatcher;

   --  ------------------------------
   --  Returns the context path of the web application.
   --  The context path is the portion of the request URI that is used to select the context
   --  of the request.  The context path always comes first in a request URI.  The path starts
   --  with a "/" character but does not end with a "/" character. For servlets in the default
   --  (root) context, this method returns "".
   --  ------------------------------
   function Get_Context_Path (Context : in Servlet_Registry) return String is
   begin
      return To_String (Context.Context_Path);
   end Get_Context_Path;

   --  ------------------------------
   --  Returns a String containing the value of the named context-wide initialization
   --  parameter, or null if the parameter does not exist.
   --
   --  This method can make available configuration information useful to an entire
   --  "web application". For example, it can provide a webmaster's email address
   --  or the name of a system that holds critical data.
   --  ------------------------------
   function Get_Init_Parameter (Context : in Servlet_Registry;
                                Name    : in String;
                                Default : in String := "") return String is
   begin
      return Context.Config.Get (Name, Default);
   end Get_Init_Parameter;

   function Get_Init_Parameter (Context : in Servlet_Registry;
                                Name    : in String;
                                Default : in String := "")
                                return Ada.Strings.Unbounded.Unbounded_String is
   begin
      if Context.Config.Exists (Name) then
         return Context.Config.Get (Name);
      else
         return Ada.Strings.Unbounded.To_Unbounded_String (Default);
      end if;
   end Get_Init_Parameter;

   --  ------------------------------
   --  Set the init parameter identified by <b>Name</b> to the value <b>Value</b>.
   --  ------------------------------
   procedure Set_Init_Parameter (Context : in out Servlet_Registry;
                                 Name    : in String;
                                 Value   : in String) is
   begin
      Log.Debug ("Set {0}={1}", Name, Value);

      Context.Config.Set (Name, Value);
   end Set_Init_Parameter;

   --  ------------------------------
   --  Set the init parameters by copying the properties defined in <b>Params</b>.
   --  Existing parameters will be overriding by the new values.
   --  ------------------------------
   procedure Set_Init_Parameters (Context : in out Servlet_Registry;
                                  Params  : in Util.Properties.Manager'Class) is
   begin
      Context.Config.Copy (Params);
   end Set_Init_Parameters;

   --  ------------------------------
   --  Get access to the init parameters.
   --  ------------------------------
   procedure Get_Init_Parameters (Context : in Servlet_Registry;
                                  Process : not null access
                                    procedure (Params : in Util.Properties.Manager'Class)) is
   begin
      Process (Context.Config);
   end Get_Init_Parameters;

   --  ------------------------------
   --  Returns the absolute path of the resource identified by the given relative path.
   --  The resource is searched in a list of directories configured by the application.
   --  The path must begin with a "/" and is interpreted as relative to the current
   --  context root.
   --
   --  This method allows the servlet container to make a resource available to
   --  servlets from any source.
   --
   --  This method returns an empty string if the resource could not be localized.
   --  ------------------------------
   function Get_Resource (Context : in Servlet_Registry;
                          Path    : in String) return String is
      Paths  : constant String := Context.Get_Init_Parameter ("view.dir");
      Result : constant String := Util.Files.Find_File_Path (Name => Path, Paths => Paths);
   begin
      if Result = Path then
         return "";
      else
         return Result;
      end if;
   end Get_Resource;

   --  ------------------------------
   --  Registers the given servlet instance with this ServletContext under
   --  the given servletName.
   --
   --  If this ServletContext already contains a preliminary
   --  ServletRegistration for a servlet with the given servletName,
   --  it will be completed (by assigning the class name of the given
   --  servlet instance to it) and returned.
   --  ------------------------------
   procedure Add_Servlet (Registry : in out Servlet_Registry;
                          Name     : in String;
                          Server   : in Servlet_Access) is
   begin
      Log.Info ("Add servlet '{0}'", Name);

      if Server.Context /= null then
         Log.Error ("Servlet '{0}' already registered in a servlet registry", Server.Get_Name);
         raise Servlet_Error;
      end if;

      Server.Name := To_Unbounded_String (Name);
      Server.Context := Registry'Unchecked_Access;
      Servlet_Maps.Include (Registry.Servlets, Name, Server);

      Server.Initialize (Registry);
   end Add_Servlet;

   --  ------------------------------
   --  Registers the given filter instance with this Servlet context.
   --  ------------------------------
   procedure Add_Filter (Registry : in out Servlet_Registry;
                         Name     : in String;
                         Filter   : in Filter_Access) is
   begin
      Log.Info ("Add servlet filter '{0}'", Name);

      Filter_Maps.Include (Registry.Filters, Name,
                           Filter.all'Unchecked_Access);
   end Add_Filter;

   procedure Add_Filter (Registry : in out Servlet_Registry;
                         Target   : in String;
                         Name     : in String) is
   begin
      null;
   end Add_Filter;

   --  ------------------------------
   --  Causes the next filter in the chain to be invoked, or if the calling
   --  filter is the last filter in the chain, causes the resource at the end
   --  of the chain to be invoked.
   --  ------------------------------
   procedure Do_Filter (Chain    : in out Filter_Chain;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class) is
   begin
      if Chain.Filter_Pos = 0 then
         Chain.Servlet.Service (Request, Response);
      else
         declare
            Filter : constant ASF.Filters.Filter_Access := Chain.Filters (Chain.Filter_Pos);
         begin
            Chain.Filter_Pos := Chain.Filter_Pos - 1;
            Filter.Do_Filter (Request, Response, Chain);
         end;
      end if;
   end Do_Filter;

   --  ------------------------------
   --  Get the servlet context associated with the filter chain.
   --  ------------------------------
   function Get_Servlet_Context (Chain : in Filter_Chain) return Servlet_Registry_Access is
   begin
      return Chain.Servlet.Context;
   end Get_Servlet_Context;

   function Match_Pattern (Pattern : in String;
                           URI     : in String) return Boolean;

   function Match_Pattern (Pattern : in String;
                           URI     : in String) return Boolean is
      Pos  : Natural := Pattern'First;
      Last : Natural;
   begin
      --  *.html,       /home/*.html -> True
      --  /home/*.html, /home/users/:id ->False
      --  /home/*.html, /home/users/:id/view.html -> True
      --  /home/*.html, /home/index.html -> True
      --  /home/*,      /home/users/:id -> True
      --  /home/*,      /home/admin/*.html -> True
      loop
         if Pattern (Pos) = '*' then
            if Pos = Pattern'Last then
               return True;
            end if;
            Pos := Pos + 1;
            if Pattern (Pos) /= '.' then
               return False;
            end if;
            Last := Util.Strings.Rindex (URI, '.');
            if Last = 0 then
               return False;
            end if;
            if Pattern (Pos .. Pattern'Last) = URI (Last .. URI'Last) then
               return True;
            else
               return False;
            end if;
         elsif Pos > URI'Last then
            return False;
         elsif Pattern (Pos) /= URI (Pos) then
            return False;
         end if;
         Pos := Pos + 1;
         if Pos > Pattern'Last then
            if Pos > URI'Last then
               return True;
            else
               return False;
            end if;
         end if;
      end loop;
   end Match_Pattern;

   --  ------------------------------
   --  Install the servlet filters after all the mappings have been registered.
   --  ------------------------------
   procedure Install_Filters (Registry : in out Servlet_Registry) is
      procedure Process (URI   : in String;
                         Route : in ASF.Routes.Route_Type_Access);
      procedure Make_Route;
      procedure Initialize_Filter (Key    : in String;
                                   Filter : in Filter_Access);

      procedure Process (URI   : in String;
                         Route : in ASF.Routes.Route_Type_Access) is
         Iter : Util.Strings.Vectors.Cursor := Registry.Filter_Patterns.First;
         Servlet_Route : ASF.Routes.Servlets.Servlet_Route_Type_Access;
      begin
         if not (Route.all in ASF.Routes.Servlets.Servlet_Route_Type'Class) then
            return;
         end if;
         Servlet_Route := ASF.Routes.Servlets.Servlet_Route_Type'Class (Route.all)'Access;
         while Util.Strings.Vectors.Has_Element (Iter) loop
            declare
               Pattern : constant String := Util.Strings.Vectors.Element (Iter);
               Filter  : Filter_List_Access;
            begin
               if Match_Pattern (Pattern, URI) then
                  Filter := Registry.Filter_Rules.Element (Pattern);
                  for I in Filter'Range loop
                     ASF.Routes.Servlets.Append_Filter (Servlet_Route.all, Filter (I).all'Access);
                  end loop;
               end if;
            end;
            Util.Strings.Vectors.Next (Iter);
         end loop;
      end Process;

      --  ------------------------------
      --  Setup a route for each filter mapping so that we can configure the filter
      --  for the filter mapping route.
      --  ------------------------------
      procedure Make_Route is
         Context : aliased EL.Contexts.Default.Default_Context;
         Iter    : Util.Strings.Vectors.Cursor := Registry.Filter_Patterns.First;
      begin
         while Util.Strings.Vectors.Has_Element (Iter) loop
            declare
               use ASF.Routes.Servlets;
               procedure Insert (Ref : in out ASF.Routes.Route_Type_Ref);

               Pattern : constant String := Util.Strings.Vectors.Element (Iter);
               Route   : ASF.Routes.Route_Context_Type;

               procedure Insert (Ref : in out ASF.Routes.Route_Type_Ref) is
                  Proxy   : ASF.Routes.Servlets.Proxy_Route_Type_Access;
               begin
                  if Ref.Is_Null then
                     Proxy := new ASF.Routes.Servlets.Proxy_Route_Type;
                     Proxy.Route := Servlet_Route_Type'Class (Route.Get_Route.all)'Access;

                     --  If the route is also a proxy, get the real route pointed to by the proxy.
                     if Proxy.Route.all in Proxy_Route_Type'Class then
                        Proxy.Route := Proxy_Route_Type'Class (Proxy.Route.all).Route;
                     end if;
                     Ref := ASF.Routes.Route_Type_Refs.Create (Proxy.all'Access);
                  end if;
               end Insert;

            begin
               Registry.Routes.Find_Route (Pattern, Route);
               if Route.Get_Route /= null then
                  Registry.Routes.Add_Route (Pattern, Context, Insert'Access);
               end if;
            end;
            Util.Strings.Vectors.Next (Iter);
         end loop;
      end Make_Route;

      Config : Filter_Config;

      procedure Initialize_Filter (Key    : in String;
                                   Filter : in Filter_Access) is
      begin
         Config.Name := To_Unbounded_String (Key);
         Filter.Initialize (Config);
      end Initialize_Filter;

      Iter   : Filter_Maps.Cursor := Registry.Filters.First;
   begin
      Config.Context := Registry'Unchecked_Access;
      while Filter_Maps.Has_Element (Iter) loop
         Filter_Maps.Query_Element (Position => Iter, Process => Initialize_Filter'Access);
         Filter_Maps.Next (Iter);
      end loop;
      Make_Route;
      Registry.Routes.Iterate (Process'Access);
   end Install_Filters;

   --  ------------------------------
   --  Dump the routes and filter configuration in the log with the given log level.
   --  ------------------------------
   procedure Dump_Routes (Registry : in out Servlet_Registry;
                          Level    : in Util.Log.Level_Type) is
      use type ASF.Filters.Filter_List_Access;
      use type ASF.Filters.Filter_Access;

      function Get_Servlet_Name (Servlet : in Servlet_Access) return String;
      function Get_Filter_Names (Filters : in ASF.Filters.Filter_List_Access) return String;
      procedure Print_Route (URI   : in String;
                             Route : in ASF.Routes.Route_Type_Access);
      procedure Collect_Servlet (Pos : in Servlet_Maps.Cursor);
      procedure Collect_Filter (Pos : in Filter_Maps.Cursor);

      Maps : Util.Strings.Maps.Map;

      function Get_Servlet_Name (Servlet : in Servlet_Access) return String is
      begin
         if Servlet = null then
            return "null";
         else
            declare
               N : constant String := System.Address_Image (Servlet.all'Address);
            begin
               if Maps.Contains (N) then
                  return Maps.Element (N);
               else
                  return " ? at " & N;
               end if;
            end;
         end if;
      end Get_Servlet_Name;

      function Get_Filter_Names (Filters : in ASF.Filters.Filter_List_Access) return String is
         Result : Ada.Strings.Unbounded.Unbounded_String;
      begin
         for I in Filters'Range loop
            if Length (Result) > 0 then
               Append (Result, ", ");
            end if;
            if Filters (I) = null then
               Append (Result, "null");
            else
               declare
                  N : constant String := System.Address_Image (Filters (I).all'Address);
               begin
                  if Maps.Contains (N) then
                     Append (Result, Maps.Element (N));
                  else
                     Append (Result, " ? at " & N);
                  end if;
               end;
            end if;
         end loop;
         return To_String (Result);
      end Get_Filter_Names;

      procedure Print_Route (URI   : in String;
                             Route : in ASF.Routes.Route_Type_Access) is
         Servlet_Route : ASF.Routes.Servlets.Servlet_Route_Type_Access;
      begin
         if not (Route.all in ASF.Routes.Servlets.Servlet_Route_Type'Class) then
            Log.Print (Level, "Route {0} to {1}", URI, System.Address_Image (Route.all'Address));
         else
            Servlet_Route := ASF.Routes.Servlets.Servlet_Route_Type'Class (Route.all)'Access;
            if Servlet_Route.Filters /= null then
               Log.Print (Level, "Route {0} to {1} with filters {2}",
                          URI, Get_Servlet_Name (Servlet_Route.Get_Servlet),
                          Get_Filter_Names (Servlet_Route.Filters));
            else
               Log.Print (Level, "Route {0} to {1}", URI,
                          Get_Servlet_Name (Servlet_Route.Get_Servlet));
            end if;
         end if;
      end Print_Route;

      procedure Collect_Servlet (Pos : in Servlet_Maps.Cursor) is
         Key     : constant String := Servlet_Maps.Key (Pos);
         Servlet : constant Servlet_Access := Servlet_Maps.Element (Pos);
      begin
         Maps.Include (System.Address_Image (Servlet.all'Address), Key);
      end Collect_Servlet;

      procedure Collect_Filter (Pos : in Filter_Maps.Cursor) is
         Key     : constant String := Filter_Maps.Key (Pos);
         Filter : constant Filter_Access := Filter_Maps.Element (Pos);
      begin
         Maps.Include (System.Address_Image (Filter.all'Address), Key);
      end Collect_Filter;

   begin
      Registry.Servlets.Iterate (Collect_Servlet'Access);
      Registry.Filters.Iterate (Collect_Filter'Access);
      Registry.Routes.Iterate (Print_Route'Access);
   end Dump_Routes;

   --  ------------------------------
   --  Start the application.
   --  ------------------------------
   procedure Start (Registry : in out Servlet_Registry) is
   begin
      Install_Filters (Registry);
   end Start;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => ASF.Filters.Filter_List,
                                     Name   => Filter_List_Access);

   --  ------------------------------
   --  Add a filter mapping with the given pattern
   --  If the URL pattern is already mapped to a different servlet,
   --  no updates will be performed.
   --  ------------------------------
   procedure Add_Filter_Mapping (Registry : in out Servlet_Registry;
                                 Pattern  : in String;
                                 Name     : in String) is
      procedure Append (Key  : in String;
                        List : in out Filter_List_Access);

      Pos  : constant Filter_Maps.Cursor := Registry.Filters.Find (Name);
      Rule : constant Filter_List_Maps.Cursor := Registry.Filter_Rules.Find (Pattern);

      --  ------------------------------
      --  Append the filter in the filter list.
      --  ------------------------------
      procedure Append (Key  : in String;
                        List : in out Filter_List_Access) is
         pragma Unreferenced (Key);
         use type ASF.Filters.Filter_Access;
         use type ASF.Filters.Filter_List;
         use ASF.Filters;

         Filter   : constant ASF.Filters.Filter_Access := Filter_Maps.Element (Pos).all'Access;
         New_List : Filter_List_Access;
      begin
         --  Check that the filter is not already executed.
         for I in List'Range loop
            if List (I) = Filter then
               return;
            end if;
         end loop;

         New_List := new ASF.Filters.Filter_List (1 .. List'Last + 1);
         New_List.all (2 .. New_List'Last) := List.all;
         New_List (New_List'First) := Filter;
         Free (List);
         List := New_List;
      end Append;

      List : Filter_List_Access;

   begin
      Log.Info ("Add filter mapping {0} -> {1}", Pattern, Name);

      if not Filter_Maps.Has_Element (Pos) then
         Log.Error ("No servlet filter {0}", Name);
         raise Servlet_Error with "No servlet filter " & Name;
      end if;
      if not Filter_List_Maps.Has_Element (Rule) then
         Registry.Filter_Patterns.Append (Pattern);
         List := new ASF.Filters.Filter_List (1 .. 1);
         List (List'First) := Filter_Maps.Element (Pos).all'Access;
         Registry.Filter_Rules.Insert (Pattern, List);
      else
         Registry.Filter_Rules.Update_Element (Rule, Append'Access);
      end if;
   end Add_Filter_Mapping;

   --  ------------------------------
   --  Add a servlet mapping with the given pattern
   --  If the URL pattern is already mapped to a different servlet,
   --  no updates will be performed.
   --  ------------------------------
   procedure Add_Mapping (Registry : in out Servlet_Registry;
                          Pattern  : in String;
                          Name     : in String) is
      Pos : constant Servlet_Maps.Cursor := Registry.Servlets.Find (Name);
   begin
      if not Servlet_Maps.Has_Element (Pos) then
         Log.Error ("No servlet {0}", Name);
         raise Servlet_Error with "No servlet " & Name;
      end if;

      Log.Info ("Add servlet mapping {0} -> {1}", Pattern, Name);
      Registry.Add_Mapping (Pattern, Servlet_Maps.Element (Pos));
   end Add_Mapping;

   --  ------------------------------
   --  Add a servlet mapping with the given pattern
   --  If the URL pattern is already mapped to a different servlet,
   --  no updates will be performed.
   --  ------------------------------
   procedure Add_Mapping (Registry : in out Servlet_Registry;
                          Pattern  : in String;
                          Server   : in Servlet_Access) is
      procedure Insert (Route : in out ASF.Routes.Route_Type_Ref);

      procedure Insert (Route : in out ASF.Routes.Route_Type_Ref) is
         To     : ASF.Routes.Servlets.Servlet_Route_Type_Access;
      begin
         if Route.Is_Null then
            To := new ASF.Routes.Servlets.Servlet_Route_Type;
            To.Servlet := Server;
            Route := ASF.Routes.Route_Type_Refs.Create (To.all'Access);
         else
            Log.Warn ("Mapping {0} already defined", Pattern);
         end if;
      end Insert;

      Context   : aliased EL.Contexts.Default.Default_Context;
   begin
      if Pattern'Length = 0 or Server = null then
         return;
      end if;
      Registry.Routes.Add_Route (Pattern, Context, Insert'Access);
   end Add_Mapping;

   --  ------------------------------
   --  Add a route associated with the given path pattern.  The pattern is split into components.
   --  Some path components can be a fixed string (/home) and others can be variable.
   --  When a path component is variable, the value can be retrieved from the route context.
   --  Once the route path is created, the <tt>Process</tt> procedure is called with the route
   --  reference.
   --  ------------------------------
   procedure Add_Route (Registry  : in out Servlet_Registry;
                        Pattern   : in String;
                        ELContext : in EL.Contexts.ELContext'Class;
                        Process   : not null access
                          procedure (Route : in out ASF.Routes.Route_Type_Ref)) is
   begin
      Registry.Routes.Add_Route (Pattern, ELContext, Process);
   end Add_Route;

   --  ------------------------------
   --  Set the error page that will be used if a servlet returns an error.
   --  ------------------------------
   procedure Set_Error_Page (Server : in out Servlet_Registry;
                             Error  : in Integer;
                             Page   : in String) is
   begin
      Log.Info ("Using page {0} for http error {1}", Page, Integer'Image (Error));

      Server.Error_Pages.Include (Error, Page);
   end Set_Error_Page;

   function Hash (N : Integer) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (N);
   end Hash;

   --  ------------------------------
   --  Send the error page content defined by the response status.
   --  ------------------------------
   procedure Send_Error_Page (Server   : in Servlet_Registry;
                              Request  : in out Requests.Request'Class;
                              Response : in out Responses.Response'Class) is
      URI    : constant String  := Request.Get_Request_URI;
      Status : constant Natural := Response.Get_Status;
      Pos    : constant Error_Maps.Cursor := Server.Error_Pages.Find (Status);
   begin
      Request.Set_Attribute ("servlet.error.status_code", EL.Objects.To_Object (Integer (Status)));
      Request.Set_Attribute ("servlet.error.request_uri", EL.Objects.To_Object (URI));

      if Error_Maps.Has_Element (Pos) then
         declare
            Page       : constant String := Error_Maps.Element (Pos);
            Dispatcher : constant Request_Dispatcher
              := Server.Get_Request_Dispatcher (Page);
         begin
            Forward (Dispatcher, Request, Response);
            return;

         exception
            when others =>
               null;
         end;
      end if;

      Response.Set_Content_Type ("text/html");
      declare
         Output : ASF.Streams.Print_Stream := Response.Get_Output_Stream;
         Value  : EL.Objects.Object;
      begin
         Output.Write ("<html><head><title>Server error</title>"
                       & "<style><!--H1 {font-family:Tahoma,Arial,sans-serif;color:white;"
                       & "background-color:#525D76;font-size:22px;} "
                       & "H2 {font-family:Tahoma,Arial,sans-serif;color:white;"
                       & "background-color:#525D76;font-size:16px;} "
                       & "H3 {font-family:Tahoma,Arial,sans-serif;color:white;"
                       & " background-color:#525D76;font-size:14px;} "
                       & "BODY {font-family:Tahoma,Arial,sans-serif;color:black;"
                       & "background-color:white;} "
                       & "B {font-family:Tahoma,Arial,sans-serif;color:white;"
                       & "background-color:#525D76;} "
                       & "P {font-family:Tahoma,Arial,sans-serif;background:white;"
                       & "color:black;font-size:12px;}A {color : black;}"
                       & "table {width:100%;} table td:first-child {width:20%}"
                       & "table th {text-align:left;color:white;background-color:#525D76;}"
                       & "A.name {color : black;}HR {color : #525D76;}--></style><body>"
                       & "<h1>HTTP error ");
         Output.Write (Status);
         Output.Write ("</h1><hr size='1' noshade='noshade'></hr><table>");
         Output.Write ("<tr><td>Error</td><td>");
         Output.Write (Status);
         Output.Write ("</td></tr><tr><td>Page</td><td>");
         Output.Write (URI);
         Output.Write ("</td></tr>");

         Value := Request.Get_Attribute ("servlet.error.message");
         if not EL.Objects.Is_Null (Value) then
            Output.Write ("<tr><td>Message:</td><td>");
            Output.Write (Value);
            Output.Write ("</td></tr>");
         end if;

         Value := Request.Get_Attribute ("servlet.error.exception_type");
         if not EL.Objects.Is_Null (Value) then
            Output.Write ("<tr><td>Exception:</td><td>");
            Output.Write (Value);
            Output.Write ("</td></tr>");
         end if;

         Value := Request.Get_Attribute ("servlet.error.exception");
         if not EL.Objects.Is_Null (Value) then
            Output.Write ("<tr><td>Traceback:</td><td><pre>");
            Output.Write (Value);
            Output.Write ("</pre></td></tr>");
         end if;

         Output.Write ("<tr><td colspan='2'>");
         Output.Write (ASF.Requests.Tools.To_String (Req              => Request,
                                                     Html             => True,
                                                     Print_Headers    => True,
                                                     Print_Attributes => True));
         Output.Write ("<td></tr></table></body>");
      end;
   end Send_Error_Page;

   --  ------------------------------
   --  Report an error when an exception occurred while processing the request.
   --  ------------------------------
   procedure Error (Registry : in Servlet_Registry;
                    Request  : in out Requests.Request'Class;
                    Response : in out Responses.Response'Class;
                    Ex       : in Ada.Exceptions.Exception_Occurrence) is
      use GNAT.Traceback.Symbolic;
      use Ada.Exceptions;

      Name          : constant String := Exception_Name (Ex);
      Message       : constant String := Exception_Message (Ex);
      Has_Traceback : constant Boolean := True;
   begin
      Request.Set_Attribute ("servlet.error.exception_type",
                             EL.Objects.To_Object (Name));
      Request.Set_Attribute ("servlet.error.message",
                             EL.Objects.To_Object (Message));
      if Has_Traceback then
         Request.Set_Attribute ("servlet.error.exception",
                                EL.Objects.To_Object (Symbolic_Traceback (Ex)));
      end if;
      Response.Set_Status (Responses.SC_INTERNAL_SERVER_ERROR);
      Registry.Send_Error_Page (Request, Response);
   end Error;

   --  ------------------------------
   --  Register the application represented by <b>Registry</b> under the base URI defined
   --  by <b>URI</b>.  This is called by the Web container when the application is registered.
   --  The default implementation keeps track of the base URI to implement the context path
   --  operation.
   --  ------------------------------
   procedure Register_Application (Registry : in out Servlet_Registry;
                                   URI      : in String) is
   begin
      Registry.Context_Path := To_Unbounded_String (URI);
   end Register_Application;

   --  ------------------------------
   --  Finalize the servlet registry releasing the internal mappings.
   --  ------------------------------
   overriding
   procedure Finalize (Registry : in out Servlet_Registry) is
   begin
      --  Release the filter mapping lists that have been allocated.
      while not Registry.Filter_Rules.Is_Empty loop
         declare
            Pos    : Filter_List_Maps.Cursor := Registry.Filter_Rules.First;
            Filter : Filter_List_Access := Filter_List_Maps.Element (Pos).all'Access;
         begin
            Free (Filter);
            Registry.Filter_Rules.Delete (Pos);
         end;
      end loop;
      ASF.Sessions.Factory.Session_Factory (Registry).Finalize;
   end Finalize;

end ASF.Servlets;
