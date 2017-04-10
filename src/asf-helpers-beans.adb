-----------------------------------------------------------------------
--  asf-helpers-beans -- Helper packages to write ASF applications
--  Copyright (C) 2012, 2017 Stephane Carrez
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
with Util.Beans.Objects;
with ASF.Contexts.Faces;
package body ASF.Helpers.Beans is

   --  ------------------------------
   --  Get a bean instance associated under the given name from the current faces context.
   --  A null value is returned if the bean does not exist or is not of the good type.
   --  ------------------------------
   function Get_Bean (Name : in String) return Element_Access is
      use type ASF.Contexts.Faces.Faces_Context_Access;
      use type Util.Beans.Basic.Readonly_Bean_Access;

      Context : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
   begin
      if Context = null then
         return null;
      end if;
      declare
         Bean : constant Util.Beans.Basic.Readonly_Bean_Access := Context.Get_Bean (Name);
      begin
         if Bean = null or else not (Bean.all in Element_Type'Class) then
            return null;
         else
            return Element_Type'Class (Bean.all)'Access;
         end if;
      end;
   end Get_Bean;

   --  ------------------------------
   --  Get a bean instance associated under the given name from the request.
   --  A null value is returned if the bean does not exist or is not of the good type.
   --  ------------------------------
   function Get_Request_Bean (Request : in ASF.Requests.Request'Class;
                              Name    : in String) return Element_Access is
      Value : constant Util.Beans.Objects.Object := Request.Get_Attribute (Name);
      Bean  : constant access Util.Beans.Basic.Readonly_Bean'Class
        := Util.Beans.Objects.To_Bean (Value);
   begin
      if Bean = null or else not (Bean.all in Element_Type'Class) then
         return null;
      else
         return Element_Type'Class (Bean.all)'Access;
      end if;
   end Get_Request_Bean;

end ASF.Helpers.Beans;
