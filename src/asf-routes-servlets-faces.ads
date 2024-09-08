-----------------------------------------------------------------------
--  asf-routes-servlets-faces -- faces request routing
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
package ASF.Routes.Servlets.Faces is

   type Faces_Route_Type is new Servlet_Route_Type with record
      View    : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Faces_Route_Type_Access is access all Faces_Route_Type'Class;

end ASF.Routes.Servlets.Faces;
