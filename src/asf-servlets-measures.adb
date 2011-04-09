-----------------------------------------------------------------------
--  asf.servlets.measures -- Dump performance measurements
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

with Util.Streams.Texts;
with ASF.Streams;
package body ASF.Servlets.Measures is

   --  ------------------------------
   --  Called by the servlet container to indicate to a servlet that the servlet
   --  is being placed into service.
   --  ------------------------------
   procedure Initialize (Server  : in out Measure_Servlet;
                         Context : in Servlet_Registry'Class) is
      pragma Unreferenced (Context);
   begin
      Server.Current := Server.Measures'Unchecked_Access;
   end Initialize;

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
   overriding
   function Get_Last_Modified (Server  : in Measure_Servlet;
                               Request : in Requests.Request'Class)
                               return Ada.Calendar.Time is
      pragma Unreferenced (Server, Request);
   begin
      return Ada.Calendar.Clock;
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
   overriding
   procedure Do_Get (Server   : in Measure_Servlet;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class) is
      pragma Unreferenced (Request);

      use type Util.Measures.Measure_Set_Access;

      Output   : ASF.Streams.Print_Stream := Response.Get_Output_Stream;

      procedure Print (Into : in out Util.Streams.Texts.Print_Stream'Class) is
      begin
         Util.Measures.Write (Measures => Server.Current.all,
                              Title    => "",
                              Stream   => Into);
      end Print;

   begin
      Response.Set_Status (Responses.SC_OK);
      Response.Set_Content_Type ("text/xml");
      Output.Write (Print'Access);
   end Do_Get;

   --  ------------------------------
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
   --  ------------------------------
   overriding
   procedure Do_Filter (F        : in Measure_Servlet;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class;
                        Chain    : in out ASF.Servlets.Filter_Chain) is
      URI   : constant String := Request.Get_Path_Info;
      Stamp : Util.Measures.Stamp;
   begin
      Util.Measures.Set_Current (F.Current);
      ASF.Servlets.Do_Filter (Chain    => Chain,
                              Request  => Request,
                              Response => Response);
      Util.Measures.Report (Measures => F.Current.all,
                            S        => Stamp,
                            Title    => URI);
      Util.Measures.Set_Current (null);

   exception
      when others =>
         Util.Measures.Report (Measures => F.Current.all,
                               S        => Stamp,
                               Title    => URI);
         Util.Measures.Set_Current (null);
         raise;
   end Do_Filter;

end ASF.Servlets.Measures;
