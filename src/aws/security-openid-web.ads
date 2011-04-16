-----------------------------------------------------------------------
--  openid -- Open ID 2.0 Support
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

package Security.Openid.Web is

   --  ------------------------------
   --  Open ID Manager using AWS
   --  ------------------------------
   type Manager is new Openid.Manager with private;

   overriding
   procedure Get_Request (Realm  : in Manager;
                          URI    : in String;
                          Accept_Format : in String;
                          Result : out Ada.Strings.Unbounded.Unbounded_String);

   overriding
   procedure Post_Request (Realm  : in Manager;
                           URI    : in String;
                           Params : in String;
                           Result : out Ada.Strings.Unbounded.Unbounded_String);

private

   type Manager is new Openid.Manager with null record;

end Security.Openid.Web;
