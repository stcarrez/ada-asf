-----------------------------------------------------------------------
--  asf-routes -- Request routing
--  Copyright (C) 2015, 2016, 2017, 2018, 2019 Stephane Carrez
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
