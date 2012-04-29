-----------------------------------------------------------------------
--  asf-beans-headers -- Bean giving access to the request headers
--  Copyright (C) 2011, 2012 Stephane Carrez
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
with Util.Beans.Objects;

package ASF.Beans.Headers is

   --  Context variable giving access to the request headers.
   HEADER_ATTRIBUTE_NAME    : constant String := "header";

   --  ------------------------------
   --  Request Header Bean
   --  ------------------------------
   --  The <b>Header_Bean</b> gives access to the request headers.
   --  The bean instance is global to the application.
   type Header_Bean is new Util.Beans.Basic.Readonly_Bean with private;

   --  Get the request header identified by the given name.
   --  Returns Null_Object if the request does not define such header.
   overriding
   function Get_Value (Bean : in Header_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Return the Header_Bean instance.
   function Instance return Util.Beans.Objects.Object;

private

   type Header_Bean is new Util.Beans.Basic.Readonly_Bean with null record;

end ASF.Beans.Headers;
