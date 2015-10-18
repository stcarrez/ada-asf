-----------------------------------------------------------------------
--  asf-routes-servlets -- Servlet request routing
--  Copyright (C) 2015 Stephane Carrez
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

with ASF.Filters;
with ASF.Servlets;
package ASF.Routes.Servlets is

   type Servlet_Route_Type is new ASF.Routes.Route_Type with record
      Filters : ASF.Filters.Filter_List_Access;
      Servlet : ASF.Servlets.Servlet_Access;
   end record;
   type Servlet_Route_Type_Access is access all Servlet_Route_Type'Class;

   --  Get the servlet to call for the route.
   function Get_Servlet (Route : in Servlet_Route_Type) return ASF.Servlets.Servlet_Access;

   --  Append the filter to the filter list defined by the mapping node.
   procedure Append_Filter (Route  : in out Servlet_Route_Type;
                            Filter : in ASF.Filters.Filter_Access);

   --  Release the storage held by the route.
   overriding
   procedure Finalize (Route : in out Servlet_Route_Type);

   type Proxy_Route_Type is new Servlet_Route_Type with record
      Route   : Servlet_Route_Type_Access;
   end record;
   type Proxy_Route_Type_Access is access all Proxy_Route_Type'Class;

   --  Get the servlet to call for the route.
   overriding
   function Get_Servlet (Route : in Proxy_Route_Type) return ASF.Servlets.Servlet_Access;

end ASF.Routes.Servlets;
