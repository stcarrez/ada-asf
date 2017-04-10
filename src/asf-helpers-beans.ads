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
with Util.Beans.Basic;

with ASF.Requests;
package ASF.Helpers.Beans is

   --  Get a bean instance associated under the given name from the current faces context.
   --  A null value is returned if the bean does not exist or is not of the good type.
   generic
      type Element_Type is new Util.Beans.Basic.Readonly_Bean with private;
      type Element_Access is access all Element_Type'Class;
   function Get_Bean (Name : in String) return Element_Access;

   --  Get a bean instance associated under the given name from the request.
   --  A null value is returned if the bean does not exist or is not of the good type.
   generic
      type Element_Type is new Util.Beans.Basic.Readonly_Bean with private;
      type Element_Access is access all Element_Type'Class;
   function Get_Request_Bean (Request : in ASF.Requests.Request'Class;
                              Name    : in String) return Element_Access;

end ASF.Helpers.Beans;
