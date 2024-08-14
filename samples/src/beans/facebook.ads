-----------------------------------------------------------------------
--  facebook - Use Facebook Graph API
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
with Util.Beans.Methods;
with Util.Beans.Basic;
with Util.Beans.Basic.Lists;

with Security.OAuth.Clients;
package Facebook is

   --  Get the access token from the user session.
   function Get_Access_Token return String;

   --  ------------------------------
   --  Friend information
   --  ------------------------------
   --  The JSON data contains: { "name": "John", "id": "680918286" }
   type Friend_Info is new Util.Beans.Basic.Readonly_Bean with record
      Name : Util.Beans.Objects.Object;
      Id   : Util.Beans.Objects.Object;
   end record;
   type Friend_Info_Access is access all Friend_Info;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Friend_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   package Friend_List is new Util.Beans.Basic.Lists (Element_Type => Friend_Info);

   type Friend_List_Bean is new Friend_List.List_Bean with record
      Has_Access_Token : Boolean := False;
   end record;
   type Friend_List_Bean_Access is access all Friend_List_Bean'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Friend_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Create a Friend_List bean instance.
   function Create_Friends_Bean return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Feed information
   --  ------------------------------
   --  The JSON structure is a little bit more complex.  We only map a subset of it.
   type Feed_Info is new Util.Beans.Basic.Readonly_Bean with record
      Id          : Util.Beans.Objects.Object;
      From        : Util.Beans.Objects.Object;
      Name        : Util.Beans.Objects.Object;
      Message     : Util.Beans.Objects.Object;
      Picture     : Util.Beans.Objects.Object;
      Link        : Util.Beans.Objects.Object;
      Description : Util.Beans.Objects.Object;
      Icon        : Util.Beans.Objects.Object;
   end record;
   type Feed_Info_Access is access all Feed_Info;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Feed_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   package Feed_List is new Util.Beans.Basic.Lists (Element_Type => Feed_Info);

   --  Build and return a Facebook feed list.
   function Create_Feed_List_Bean return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Facebook Auth
   --  ------------------------------
   --  Authenticate this application on OAuth Facebook.  This is a global bean shared
   --  by several concurrent requests.  It gives access to the Facebook authenticate URI
   --  that must be used and provides the authenticate method that must be invoked upon
   --  successfully authentication by the user (ie, through the OAuth callback).
   type Facebook_Auth is new Security.OAuth.Clients.Application
     and Util.Beans.Basic.Readonly_Bean
     and Util.Beans.Methods.Method_Bean with private;

   --  Get the user information identified by the given name.
   overriding
   function Get_Value (From : in Facebook_Auth;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Authenticate result from Facebook.
   procedure Authenticate (From    : in out Facebook_Auth;
                           Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Facebook_Auth)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

private

   type Facebook_Auth is new Security.OAuth.Clients.Application
     and Util.Beans.Basic.Readonly_Bean
     and Util.Beans.Methods.Method_Bean with null record;

end Facebook;
