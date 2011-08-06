-----------------------------------------------------------------------
--  asf.servlets -- ASF Servlets
--  Copyright (C) 2010, 2011 Stephane Carrez
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

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with ASF.Filters;
with ASF.Streams;
with ASF.Requests.Tools;

with EL.Objects;
with GNAT.Traceback.Symbolic;

with Util.Strings;
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

   use Ada.Strings.Fixed;
   use Ada.Finalization;

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Servlets");

   No_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year => 1901,
                                                                 Month => 1,
                                                                 Day   => 1);

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
   begin
      Request.Set_Path_Info (To_String (Dispatcher.Path));

      if Dispatcher.Servlet /= null then
         ASF.Requests.Tools.Set_Context (Request, Dispatcher.Servlet.all'Access,
                                         Response'Unchecked_Access);
         Dispatcher.Servlet.Service (Request, Response);

      elsif Dispatcher.Mapping = null or else Dispatcher.Mapping.Servlet = null then
         Response.Send_Error (Responses.SC_NOT_FOUND);

         --  If we have some filters, create the filter chain
         --  and invoke the first filter.
      elsif Dispatcher.Mapping.Filters /= null then
         declare
            Chain : Filter_Chain;
         begin
            ASF.Requests.Tools.Set_Context (Request, Dispatcher.Mapping.Servlet.all'Access,
                                            Response'Unchecked_Access);
            Chain.Filters := Dispatcher.Mapping.Filters;
            Chain.Servlet := Dispatcher.Mapping.Servlet;
            Chain.Filter_Pos := Chain.Filters'Last;
            Do_Filter (Chain    => Chain,
                       Request  => Request,
                       Response => Response);
         end;

      else
         ASF.Requests.Tools.Set_Context (Request, Dispatcher.Mapping.Servlet.all'Access,
                                         Response'Unchecked_Access);
         Dispatcher.Mapping.Servlet.Service (Request, Response);
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
      if Dispatcher.Mapping = null or else Dispatcher.Mapping.Servlet = null then
         Response.Send_Error (Responses.SC_NOT_FOUND);
      else
         Dispatcher.Mapping.Servlet.Service (Request, Response);
      end if;
   end Include;

   --  ------------------------------
   --  Returns a Request_Dispatcher object that acts as a wrapper for the resource
   --  located at the given path.  A Request_Dispatcher  object can be used to forward
   --  a request to the resource or to include the resource in a response.
   --  The resource can be dynamic or static.
   --  ------------------------------
   function Get_Request_Dispatcher (Context : in Servlet_Registry;
                                    Path    : in String)
                                    return Request_Dispatcher is
   begin
      return R : Request_Dispatcher do
         R.Mapping := Context.Find_Mapping (URI => Path);
         if R.Mapping = null then
            R.Path := To_Unbounded_String (Path);
         elsif Path'First + R.Mapping.Path_Pos < Path'Last then
            R.Path := To_Unbounded_String
              (Path (Path'First + R.Mapping.Path_Pos - 1 .. Path'Last));
         else
            R.Path := Null_Unbounded_String;
         end if;
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
      Pos : constant Servlet_Maps.Cursor := Context.Servlets.Find (To_Unbounded_String (Name));
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

   --  ------------------------------
   --  Set the init parameter identified by <b>Name</b> to the value <b>Value</b>.
   --  ------------------------------
   procedure Set_Init_Parameter (Context : in out Servlet_Registry;
                                 Name    : in String;
                                 Value   : in String) is
   begin
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
      Servlet_Maps.Include (Registry.Servlets, Server.Name, Server);

      Server.Initialize (Registry);
   end Add_Servlet;

   --  ------------------------------
   --  Registers the given filter instance with this Servlet context.
   --  ------------------------------
   procedure Add_Filter (Registry : in out Servlet_Registry;
                         Name     : in String;
                         Filter   : access ASF.Filters.Filter'Class) is
   begin
      Log.Info ("Add servlet filter '{0}'", Name);

      Filter_Maps.Include (Registry.Filters, To_Unbounded_String (Name),
                           Filter.all'Unchecked_Access);

      Filter.Initialize (Registry);
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
         Chain.Filter_Pos := Chain.Filter_Pos - 1;
         Chain.Filters (Chain.Filter_Pos + 1).Do_Filter (Request, Response, Chain);
      end if;
   end Do_Filter;

   --  ------------------------------
   --  Get the servlet context associated with the filter chain.
   --  ------------------------------
   function Get_Servlet_Context (Chain : in Filter_Chain) return Servlet_Registry_Access is
   begin
      return Chain.Servlet.Context;
   end Get_Servlet_Context;

   procedure Free is
     new Ada.Unchecked_Deallocation (Filter_List, Filter_List_Access);

   procedure Finalize (Map : in out Mapping_Node) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Mapping_Node, Mapping_Access);

   begin
      loop
         declare
            N : Mapping_Access := Map.Child_Map;
         begin
            exit when N = null;
            Map.Child_Map := N.Next_Map;
            Free (N);
         end;
      end loop;
      Free (Map.URI);
      Free (Map.Filters);
   end Finalize;

   procedure Traverse (Map : in out Mapping_Node) is
   begin
      null;
   end Traverse;

   procedure Dump_Map (Map : in Mapping_Node;
                       Indent : in String := "") is
      function Get_Name return String;
      pragma Inline (Get_Name);

      function Get_Name return String is
      begin
         if Map.Servlet /= null then
            return Map.Servlet.Get_Name;
         else
            return "";
         end if;
      end Get_Name;

      Name : constant String := Get_Name;
   begin
      case Map.Map_Type is
         when MAP_URI_NODE =>
            Log.Info ("{0} +- [{1}] => {2}", Indent, Map.URI.all, Name);

         when MAP_URI =>
            Log.Info ("{0} +- [{1}] => {2}", Indent, Map.URI.all, Name);

         when MAP_WILDCARD =>
            Log.Info ("{0} +- [*] => {2}", Indent, Name);

         when MAP_EXTENSION =>
            Log.Info ("{0} +- [*.{1}] => {2}", Indent, Map.URI.all, Name);

      end case;
      if Map.Child_Map /= null then
         Map.Child_Map.Dump_Map (Indent & " ");
      end if;
      if Map.Next_Map /= null then
         Map.Next_Map.Dump_Map (Indent);
      end if;
   end Dump_Map;

   --  ------------------------------
   --  Append the filter to the filter list defined by the mapping node.
   --  ------------------------------
   procedure Append_Filter (Mapping : in out Mapping_Node;
                            Filter  : in Filter_Access) is
      List : Filter_List_Access;
   begin
      --  Filters are executed through the <b>Filter_Chain.Do_Filter</b> method
      --  starting from the last position to the first.  To append a filter,
      --  it must be inserted as first position of the list.
      if Mapping.Filters = null then
         List := new Filter_List (1 .. 1);
      else
         List := new Filter_List (1 .. Mapping.Filters'Last + 1);
         List (2 .. List'Last) := Mapping.Filters.all;
         Free (Mapping.Filters);
      end if;
      List (List'First) := Filter;
      Mapping.Filters := List;
   end Append_Filter;

   --  ------------------------------
   --  Add a filter mapping with the given pattern
   --  If the URL pattern is already mapped to a different servlet,
   --  no updates will be performed.
   --  ------------------------------
   procedure Add_Filter_Mapping (Registry : in out Servlet_Registry;
                                 Pattern  : in String;
                                 Name     : in String) is
      Key : constant Unbounded_String    := To_Unbounded_String (Name);
      Pos : constant Filter_Maps.Cursor := Registry.Filters.Find (Key);
   begin
      if not Filter_Maps.Has_Element (Pos) then
         Log.Error ("No servlet filter {0}", Name);
         raise Servlet_Error with "No servlet filter " & Name;
      end if;
      declare
         Mapping : Mapping_Access := Registry.Find_Mapping (URI => Pattern);
         Copy_Mapping : Mapping_Access;
      begin
         if Mapping = null then
            Log.Error ("No servlet mapping for URI {0}", Pattern);
            return;
         end if;

         --  SCz 2011-06-09: this is still not perfect.... If we have a servlet
         --  mapping to some extension (*.html), and we want to install a filter
         --  for a specific page, we have to create a new URI mapping for that specific
         --  page so that it can have dedicated filters.
         if Mapping.Map_Type = MAP_EXTENSION then
            if Pattern'Length < 3 or else
              Pattern (Pattern'First) /= '*' or else Pattern (Pattern'First + 1) /= '.' then
               Registry.Add_Mapping (Pattern => Pattern,
                                     Server  => Mapping.Servlet);
               Copy_Mapping := Registry.Find_Mapping (URI => Pattern);
               Copy_Mapping.Path_Pos := 1;
               if Mapping.Filters /= null then
                  for I in Mapping.Filters.all'Range loop
                     Copy_Mapping.Append_Filter (Mapping.Filters (I));
                  end loop;
               end if;
               Mapping := Copy_Mapping;
            end if;
         end if;
         Mapping.Append_Filter (Filter_Maps.Element (Pos));
      end;
   end Add_Filter_Mapping;

   --  ------------------------------
   --  Add a servlet mapping with the given pattern
   --  If the URL pattern is already mapped to a different servlet,
   --  no updates will be performed.
   --  ------------------------------
   procedure Add_Mapping (Registry : in out Servlet_Registry;
                          Pattern  : in String;
                          Name     : in String) is
      Key : constant Unbounded_String    := To_Unbounded_String (Name);
      Pos : constant Servlet_Maps.Cursor := Registry.Servlets.Find (Key);
   begin
      if not Servlet_Maps.Has_Element (Pos) then
         Log.Error ("No servlet {0}", Name);
         raise Servlet_Error with "No servlet " & Name;
      end if;

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
      First_Pos : Natural;
      Pos       : Natural;
      Last_Pos  : Natural;
      Node      : Mapping_Access;
      Prev_Node : Mapping_Access;
      Is_Last   : Boolean := False;
      Is_Wildcard : Boolean := False;
      Is_Extension : Boolean := False;
      URI       : String_Access;
      Map       : Mapping_Access;
   begin
      if Pattern'Length = 0 or Server = null then
         return;
      end if;

      Log.Info ("Add mapping {0} -> {1}", Pattern, To_String (Server.Name));

      --  Add an extension mapping
      if Pattern'Length >= 3 and then
         Pattern (Pattern'First) = '*' and then Pattern (Pattern'First + 1) = '.' then
         URI := new String '(Pattern (Pattern'First + 2 .. Pattern'Last));
         Map := new Mapping_Node '(Limited_Controlled with
                                   URI              => URI,
                                   Map_Type         => MAP_EXTENSION,
                                   Child_Map        => null,
                                   Next_Servlet_Map => Server.Mappings,
                                   Filters          => null,
                                   Path_Pos         => 1,
                                   Servlet          => Server,
                                   Next_Map         => Registry.Extension_Mapping);
         Registry.Extension_Mapping := Map;
         Server.Mappings := Map;
         return;
      end if;

      --  Find the position in the map tree for this mapping.
      Node      := Registry.Mappings;
      First_Pos := Pattern'First;
      Last_Pos  := Pattern'Last;
      if Pattern (First_Pos) = '/' then
         First_Pos := First_Pos + 1;
      end if;
      while Node /= null loop
         Pos := Index (Pattern, "/", First_Pos);
         if Pos > First_Pos then
            Last_Pos := Pos - 1;
         else
            Last_Pos    := Pattern'Last;
            Is_Last     := True;
            Is_Wildcard := Pattern (First_Pos .. Last_Pos) = "*";
            Is_Extension := Pattern (First_Pos) = '*' and then Last_Pos >= First_Pos + 1
              and then Pattern (First_Pos + 1) = '.';
         end if;

         --  Look for the node that matches
         while Node /= null loop
            case Node.Map_Type is
               when MAP_URI =>
                  if Node.URI.all = Pattern (First_Pos .. Last_Pos) then
                     Prev_Node := Node;
                     Node := Node.Child_Map;
                     First_Pos := Last_Pos + 2;
                     exit;
                  end if;

               when MAP_URI_NODE =>
                  if not Is_Wildcard and Node.URI.all = Pattern (First_Pos .. Last_Pos) then
                     Prev_Node := Node;
                     Node := Node.Child_Map;
                     First_Pos := Last_Pos + 2;
                     exit;
                  end if;

               when MAP_WILDCARD | MAP_EXTENSION =>
                  null;

            end case;
            Node := Node.Next_Map;
         end loop;
      end loop;

      while First_Pos <= Pattern'Last loop
         Pos := Index (Pattern, "/", First_Pos);
         if Pos > First_Pos then
            Last_Pos := Pos - 1;
         else
            Last_Pos    := Pattern'Last;
            Is_Last     := True;
            Is_Wildcard := Pattern (First_Pos .. Last_Pos) = "*";
            Is_Extension := Pattern (First_Pos) = '*' and then Last_Pos >= First_Pos + 1
              and then Pattern (First_Pos + 1) = '.';
         end if;

         URI := new String '(Pattern (First_Pos .. Last_Pos));
         if Is_Wildcard then
            Node :=  new Mapping_Node '(Limited_Controlled with
                                        URI              => URI,
                                        Map_Type         => MAP_WILDCARD,
                                        Child_Map        => null,
                                        Filters          => null,
                                        Servlet          => Server,
                                        Next_Servlet_Map => Server.Mappings,
                                        Path_Pos         => First_Pos - 1,
                                        Next_Map         => null);

         elsif Is_Extension then
            Node :=  new Mapping_Node '(Limited_Controlled with
                                        URI              => URI,
                                        Map_Type         => MAP_EXTENSION,
                                        Child_Map        => null,
                                        Filters          => null,
                                        Servlet          => Server,
                                        Next_Servlet_Map => Server.Mappings,
                                        Path_Pos         => Last_Pos,
                                        Next_Map         => null);

         elsif Is_Last then
            Node :=  new Mapping_Node '(Limited_Controlled with
                                        URI              => URI,
                                        Map_Type         => MAP_URI,
                                        Child_Map        => null,
                                        Filters          => null,
                                        Servlet          => Server,
                                        Next_Servlet_Map => Server.Mappings,
                                        Path_Pos         => Last_Pos,
                                        Next_Map         => null);

         else
            Node :=  new Mapping_Node '(Limited_Controlled with
                                        URI              => URI,
                                        Map_Type         => MAP_URI_NODE,
                                        Child_Map        => null,
                                        Filters          => null,
                                        Next_Servlet_Map => null,
                                        Servlet          => null,
                                        Path_Pos         => 0,
                                        Next_Map         => null);
         end if;

         --  Link the new node into the tree.
         if Prev_Node = null then
            if Registry.Mappings /= null then
               Node.Next_Map := Registry.Mappings;
            end if;
            Registry.Mappings := Node;
         else
            if Prev_Node.Child_Map /= null then
               Node.Next_Map := Prev_Node.Child_Map;
            end if;
            Prev_Node.Child_Map := Node;
         end if;

         Prev_Node := Node;
         First_Pos := Last_Pos + 2;
      end loop;
   end Add_Mapping;

   --  ------------------------------
   --  Find the servlet and filter mapping that must be used for the given URI.
   --  Search the mapping according to Ch 12/SRV 11. Mapping Requests to Servlets:
   --  o look for an exact match,
   --  o look for the longest match,
   --  o look for an extension
   --  o use the default servlet mapping
   --  ------------------------------
   function Find_Mapping (Registry : in Servlet_Registry;
                          URI      : in String) return Mapping_Access is
      use Ada.Strings;
      use Util.Strings;

      First_Pos : Natural := URI'First;
      Pos       : Natural;
      Last_Pos  : Natural;
      Node      : Mapping_Access := Registry.Mappings;
      Is_Last   : Boolean := False;
   begin
      if URI'Length > 0 and then URI (First_Pos) = '/' then
         First_Pos := First_Pos + 1;
      end if;

      --  Scan each component path descending in the map tree until
      --  we have a match (exact or wildcard).
      while Node /= null loop
         Pos := Index (URI, '/', First_Pos);
         if Pos > First_Pos then
            Last_Pos := Pos - 1;
         else
            Last_Pos := URI'Last;
            Is_Last  := True;
         end if;
         while Node /= null loop
            case Node.Map_Type is
               --  Check for an exact match if this is the last component.
               when MAP_URI =>
                  if Node.URI.all = URI (First_Pos .. Last_Pos) then
                     if Is_Last then
                        return Node;
                     end if;
                     Node := Node.Child_Map;
                     First_Pos := Last_Pos + 2;
                     exit;
                  end if;

               --  Check for a component path, descend the map tree
               when MAP_URI_NODE =>
                  if Node.URI.all = URI (First_Pos .. Last_Pos) then
                     Node := Node.Child_Map;
                     First_Pos := Last_Pos + 2;
                     exit;
                  end if;

               --  If we have a wildcard in the map tree, this is a match.
               when MAP_WILDCARD =>
                  return Node;

               --  We have an extension
               when MAP_EXTENSION =>
                  Pos  := Util.Strings.Rindex (Source => URI, Ch => '.');
                  if Pos > 0 and then
                    Node.URI (Node.URI'First + 1 .. Node.URI'Last) = URI (Pos .. URI'Last) then
                     return Node;
                  end if;

            end case;
            Node := Node.Next_Map;
         end loop;
      end loop;

      --  No exact match and no wildcard match.
      --  Look for an extension.
      Pos  := Util.Strings.Rindex (Source => URI, Ch => '.');
      if Pos > 0 then
         Pos  := Pos + 1;
         Node := Registry.Extension_Mapping;
         while Node /= null loop
            if Node.URI.all = URI (Pos .. URI'Last) then
               return Node;
            end if;
            Node := Node.Next_Map;
         end loop;
      end if;
      return null;
   end Find_Mapping;

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
            Page       : constant Unbounded_String := Error_Maps.Element (Pos);
            Dispatcher : constant Request_Dispatcher
              := Server.Get_Request_Dispatcher (To_String (Page));
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
                       &" background-color:#525D76;font-size:14px;} "
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

end ASF.Servlets;
