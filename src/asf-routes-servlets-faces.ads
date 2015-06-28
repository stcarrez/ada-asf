-----------------------------------------------------------------------
--  asf-routes-servlets-faces -- faces request routing
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
with Ada.Finalization;
with Ada.Strings.Unbounded;
with ASF.Filters;
with ASF.Servlets;
package ASF.Routes.Servlets.Faces is

   type Faces_Route_Type is new Servlet_Route_Type with record
      View    : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Faces_Route_Type_Access is access all Faces_Route_Type'Class;

   --  Release the storage held by the route.
--     overriding
--     procedure Finalize (Route : in out Servlet_Route_Type);

end ASF.Routes.Servlets.Faces;
