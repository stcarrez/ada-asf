-----------------------------------------------------------------------
--  asf-routes -- Request routing
--  Copyright (C) 2015, 2016, 2017, 2018, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Servlet.Routes.Servlets;

package ASF.Routes is

   type Faces_Route_Type is new Servlet.Routes.Servlets.Servlet_Route_Type with record
      View    : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Faces_Route_Type_Access is access all Faces_Route_Type'Class;

   subtype Route_Type is Servlet.Routes.Route_Type;
   subtype Route_Type_Access is Servlet.Routes.Route_Type_Access;
   subtype Route_Type_Accessor is Servlet.Routes.Route_Type_Accessor;
   subtype Route_Type_Ref is Servlet.Routes.Route_Type_Ref;

   subtype Route_Context_Type is Servlet.Routes.Route_Context_Type;

   package Route_Type_Refs renames Servlet.Routes.Route_Type_Refs;

end ASF.Routes;
