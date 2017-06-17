-----------------------------------------------------------------------
--  asf-rest -- REST Support
--  Copyright (C) 2016, 2017 Stephane Carrez
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
with ASF.Routes;
with ASF.Routes.Servlets.Rest;
with ASF.Servlets.Rest;
with EL.Contexts.Default;
with Util.Log.Loggers;
package body ASF.Rest is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Rest");

   --  ------------------------------
   --  Get the permission index associated with the REST operation.
   --  ------------------------------
   function Get_Permission (Handler : in Descriptor)
                            return Security.Permissions.Permission_Index is
   begin
      return Handler.Permission;
   end Get_Permission;

   --  ------------------------------
   --  Register the API descriptor in a list.
   --  ------------------------------
   procedure Register (List : in out Descriptor_Access;
                       Item : in Descriptor_Access) is
   begin
      Item.Next := List;
      List := Item;
   end Register;

   --  ------------------------------
   --  Register the list of API descriptors for a given servlet and a root path.
   --  ------------------------------
   procedure Register (Registry  : in out ASF.Servlets.Servlet_Registry;
                       Name      : in String;
                       URI       : in String;
                       ELContext : in EL.Contexts.ELContext'Class;
                       List      : in Descriptor_Access) is
      procedure Insert (Route : in out ASF.Routes.Route_Type_Ref);
      use type ASF.Routes.Route_Type_Access;
      Item : Descriptor_Access := List;

      procedure Insert (Route : in out ASF.Routes.Route_Type_Ref) is
         R : constant ASF.Routes.Route_Type_Access := Route.Value;
         D : ASF.Routes.Servlets.Rest.API_Route_Type_Access;
      begin
         if R /= null then
            if not (R.all in ASF.Routes.Servlets.Rest.API_Route_Type'Class) then
               Log.Error ("Route API for {0}/{1} already used by another page",
                          URI, Item.Pattern.all);
               return;
            end if;
            D := ASF.Routes.Servlets.Rest.API_Route_Type (R.all)'Access;
         else
            D := ASF.Servlets.Rest.Create_Route (Registry, Name);
            Route := ASF.Routes.Route_Type_Refs.Create (D.all'Access);
         end if;
         if D.Descriptors (Item.Method) /= null then
            Log.Error ("Route API for {0}/{1} is already used", URI, Item.Pattern.all);
         end if;
         D.Descriptors (Item.Method) := Item;
      end Insert;

   begin
      Log.Info ("Adding API route {0}", URI);
      while Item /= null loop
         Log.Debug ("Adding API route {0}/{1}", URI, Item.Pattern.all);
         Registry.Add_Route (URI & "/" & Item.Pattern.all, ELContext, Insert'Access);
         Item := Item.Next;
      end loop;
   end Register;

   --  Dispatch the request to the API handler.
   overriding
   procedure Dispatch (Handler : in Static_Descriptor;
                       Req     : in out ASF.Rest.Request'Class;
                       Reply   : in out ASF.Rest.Response'Class;
                       Stream  : in out Output_Stream'Class) is
   begin
      Handler.Handler (Req, Reply, Stream);
   end Dispatch;

   --  Register the API definition in the servlet registry.
   procedure Register (Registry   : in out ASF.Servlets.Servlet_Registry'Class;
                       Definition : in Descriptor_Access) is
      use type ASF.Routes.Route_Type_Access;
      use type ASF.Servlets.Servlet_Access;

      Dispatcher : constant ASF.Servlets.Request_Dispatcher
         := Registry.Get_Request_Dispatcher (Definition.Pattern.all);
      Servlet    : constant ASF.Servlets.Servlet_Access := ASF.Servlets.Get_Servlet (Dispatcher);

      procedure Insert (Route : in out ASF.Routes.Route_Type_Ref) is
         R : constant ASF.Routes.Route_Type_Access := Route.Value;
         D : ASF.Routes.Servlets.Rest.API_Route_Type_Access;
      begin
         if R /= null then
            if not (R.all in ASF.Routes.Servlets.Rest.API_Route_Type'Class) then
               Log.Error ("Route API for {0} already used by another page",
                          Definition.Pattern.all);
               D := ASF.Servlets.Rest.Create_Route (Servlet);
               Route := ASF.Routes.Route_Type_Refs.Create (D.all'Access);
            else
               D := ASF.Routes.Servlets.Rest.API_Route_Type (R.all)'Access;
            end if;
         else
            D := ASF.Servlets.Rest.Create_Route (Servlet);
            Route := ASF.Routes.Route_Type_Refs.Create (D.all'Access);
         end if;
         if D.Descriptors (Definition.Method) /= null then
            Log.Error ("Route API for {0} is already used", Definition.Pattern.all);
         end if;
         D.Descriptors (Definition.Method) := Definition;
      end Insert;

      Ctx     : EL.Contexts.Default.Default_Context;
   begin
      if Servlet = null then
         Log.Error ("Cannot register REST operation {0}: no REST servlet",
                    Definition.Pattern.all);
         return;
      end if;
      Registry.Add_Route (Definition.Pattern.all, Ctx, Insert'Access);
   end Register;

end ASF.Rest;
