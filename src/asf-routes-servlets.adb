-----------------------------------------------------------------------
--  asf-routes -- Request routing
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

package body ASF.Routes.Servlets is

   use type ASF.Filters.Filter_Access;
   use type ASF.Filters.Filter_List_Access;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => ASF.Filters.Filter_List,
                                     Name   => ASF.Filters.Filter_List_Access);

   --  ------------------------------
   --  Get the servlet to call for the route.
   --  ------------------------------
   function Get_Servlet (Route : in Servlet_Route_Type) return ASF.Servlets.Servlet_Access is
   begin
      return Route.Servlet;
   end Get_Servlet;

   --  ------------------------------
   --  Append the filter to the filter list defined by the mapping node.
   --  ------------------------------
   procedure Append_Filter (Route  : in out Servlet_Route_Type;
                            Filter : in ASF.Filters.Filter_Access) is
      List : ASF.Filters.Filter_List_Access;
   begin
      --  Filters are executed through the <b>Filter_Chain.Do_Filter</b> method
      --  starting from the last position to the first.  To append a filter,
      --  it must be inserted as first position of the list.
      if Route.Filters = null then
         List := new ASF.Filters.Filter_List (1 .. 1);
      else
         --  Check that the filter is not already executed.
         for I in Route.Filters'Range loop
            if Route.Filters (I) = Filter then
               return;
            end if;
         end loop;
         List := new ASF.Filters.Filter_List (1 .. Route.Filters'Last + 1);
         List (2 .. List'Last) := Route.Filters.all;
         Free (Route.Filters);
      end if;
      List (List'First) := Filter;
      Route.Filters := List;
   end Append_Filter;

   --  ------------------------------
   --  Release the storage held by the route.
   --  ------------------------------
   overriding
   procedure Finalize (Route : in out Servlet_Route_Type) is
   begin
      Free (Route.Filters);
   end Finalize;

   --  ------------------------------
   --  Get the servlet to call for the route.
   --  ------------------------------
   overriding
   function Get_Servlet (Route : in Proxy_Route_Type) return ASF.Servlets.Servlet_Access is
   begin
      if Route.Route /= null then
         return Route.Route.Get_Servlet;
      else
         return Route.Servlet;
      end if;
   end Get_Servlet;

end ASF.Routes.Servlets;
