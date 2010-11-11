-----------------------------------------------------------------------
--  asf.filters -- ASF Filters
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
with ASF.Servlets;

--  The <b>ASF.Filters</b> package defines the servlet filter
--  interface described in Java Servlet Specification, JSR 315, 6. Filtering.
--
package ASF.Filters is

   --  ------------------------------
   --  Filter interface
   --  ------------------------------
   --  The <b>Filter</b> interface defines one mandatory procedure through
   --  which the request/response pair are passed.
   --
   --  The <b>Filter</b> instance must be registered in the <b>Servlet_Registry</b>
   --  by using the <b>Add_Filter</b> procedure.  The same filter instance is used
   --  to process multiple requests possibly at the same time.
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
                        Chain    : in out ASF.Servlets.Filter_Chain) is abstract;

   --  Called by the servlet container to indicate to a filter that the filter
   --  instance is being placed into service.
   procedure Initialize (Server  : in out Filter;
                         Context : in ASF.Servlets.Servlet_Registry'Class) is null;

end ASF.Filters;
