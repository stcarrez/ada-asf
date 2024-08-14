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
with Ada.Strings.Unbounded;

with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Beans.Methods;

package Users is

   --  Given an Email address, return the Gravatar link to the user image.
   --  (See http://en.gravatar.com/site/implement/hash/ and
   --  http://en.gravatar.com/site/implement/images/)
   function Get_Gravatar_Link (Email : in String) return String;

   --  A bean that expose information about the user
   type User_Info is new Util.Beans.Basic.Readonly_Bean
     and Util.Beans.Methods.Method_Bean with null record;

   --  Get the user information identified by the given name.
   overriding
   function Get_Value (From : in User_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Logout by dropping the user session.
   procedure Logout (From    : in out User_Info;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in User_Info)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  An application bean.  This instance is shared by all requests.
   User         : aliased Users.User_Info;

end Users;
