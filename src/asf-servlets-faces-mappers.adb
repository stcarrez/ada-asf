-----------------------------------------------------------------------
--  asf-servlets-faces-mappers -- Read faces specific configuration files
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
with Ada.Unchecked_Deallocation;
with ASF.Routes.Servlets.Faces;
package body ASF.Servlets.Faces.Mappers is

   use type ASF.Routes.Route_Type_Access;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => ASF.Routes.Servlets.Faces.Faces_Route_Type'Class,
                                     Name   => ASF.Routes.Servlets.Faces.Faces_Route_Type_Access);

   function Find_Servlet (Container : in Servlet_Registry_Access;
                          View      : in String)
                          return ASF.Servlets.Servlet_Access is
      Disp  : constant Request_Dispatcher := Container.Get_Request_Dispatcher (View);
      Route : constant ASF.Routes.Route_Type_Access := Disp.Context.Get_Route;
   begin
      if Route = null then
         raise Util.Serialize.Mappers.Field_Error with "No servlet mapped to view " & View;
      end if;
      if not (Route.all in ASF.Routes.Servlets.Servlet_Route_Type'Class) then
         raise Util.Serialize.Mappers.Field_Error with "View " & View & " not mapped to a servlet";
      end if;
      return ASF.Routes.Servlets.Servlet_Route_Type'Class (Route.all).Servlet;
   end Find_Servlet;

   --  ------------------------------
   --  Save in the servlet config object the value associated with the given field.
   --  When the URL_MAPPING, URL_PATTERN, VIEW_ID field
   --  is reached, insert the new configuration rule in the servlet registry.
   --  ------------------------------
   procedure Set_Member (N     : in out Servlet_Config;
                         Field : in Servlet_Fields;
                         Value : in Util.Beans.Objects.Object) is
      use Util.Beans.Objects;

      Route : ASF.Routes.Servlets.Faces.Faces_Route_Type_Access;
   begin
      case Field is
         when URL_PATTERN =>
            N.URL_Pattern := Value;

         when VIEW_ID =>
            N.View_Id := Value;

         when URL_MAPPING =>
            declare
               View    : constant String := To_String (N.View_Id);
               Servlet : constant ASF.Servlets.Servlet_Access := Find_Servlet (N.Handler, View);
            begin
               Route := new ASF.Routes.Servlets.Faces.Faces_Route_Type;
               Route.View    := To_Unbounded_String (View);
               Route.Servlet := Servlet;
               N.Handler.Add_Route (Pattern   => To_String (N.URL_Pattern),
                                    To        => Route.all'Access,
                                    ELContext => N.Context.all);
            exception
               when others =>
                  Free (Route);
                  raise;
            end;
      end case;
   end Set_Member;

   SMapper : aliased Servlet_Mapper.Mapper;

   --  ------------------------------
   --  Setup the XML parser to read the servlet and mapping rules <b>url-mapping</b>.
   --  ------------------------------
   package body Reader_Config is
   begin
      Reader.Add_Mapping ("faces-config", SMapper'Access);
      Reader.Add_Mapping ("module", SMapper'Access);
      Reader.Add_Mapping ("web-app", SMapper'Access);
      Config.Handler := Handler;
      Config.Context := Context;
      Servlet_Mapper.Set_Context (Reader, Config'Unchecked_Access);
   end Reader_Config;

begin
   SMapper.Add_Mapping ("url-mapping", URL_MAPPING);
   SMapper.Add_Mapping ("url-mapping/pattern", URL_PATTERN);
   SMapper.Add_Mapping ("url-mapping/view-id", VIEW_ID);
end ASF.Servlets.Faces.Mappers;