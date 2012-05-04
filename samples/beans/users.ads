-----------------------------------------------------------------------
--  users - Gives access to the OpenID principal through an Ada bean
--  Copyright (C) 2012 Stephane Carrez
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
with Util.Beans.Basic;

package Users is

   --  A bean that expose information about the user
   type User_Info is new Util.Beans.Basic.Readonly_Bean with null record;

   function Get_Value (From : in User_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   --  An application bean.  This instance is shared by all requests.
   User         : aliased Users.User_Info;

end Users;
