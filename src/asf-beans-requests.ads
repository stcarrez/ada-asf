-----------------------------------------------------------------------
--  asf-beans-requests -- Bean giving access to the request object
--  Copyright (C) 2017 Stephane Carrez
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

package ASF.Beans.Requests is

   --  Context variable giving access to the request object.
   REQUEST_ATTRIBUTE_NAME    : constant String := "requestScope";

   --  ------------------------------
   --  Request Bean
   --  ------------------------------
   --  The <b>Request_Bean</b> gives access to the request object.
   --  The bean instance is global to the application.
   type Request_Bean is new Util.Beans.Basic.Readonly_Bean with private;

   --  Get from the request object the value identified by the given name.
   --  Returns Null_Object if the request does not define such name.
   overriding
   function Get_Value (Bean : in Request_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Return the Request_Bean instance.
   function Instance return Util.Beans.Objects.Object;

private

   type Request_Bean is new Util.Beans.Basic.Readonly_Bean with null record;

end ASF.Beans.Requests;
