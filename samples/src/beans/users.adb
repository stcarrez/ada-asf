-----------------------------------------------------------------------
--  users - Gives access to the OpenID principal through an Ada bean
--  Copyright (C) 2012, 2013, 2022 Stephane Carrez
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

with ASF.Contexts.Faces;
with ASF.Contexts.Flash;
with ASF.Sessions;
with ASF.Principals;
with ASF.Cookies;
with ASF.Events.Faces.Actions;
with ASF.Applications.Messages.Factory;

with GNAT.MD5;

with Security.Auth;
with ASF.Security.Filters;

with Util.Strings.Transforms;
package body Users is

   use Ada.Strings.Unbounded;
   use Security.Auth;
   use type ASF.Principals.Principal_Access;
   use type ASF.Contexts.Faces.Faces_Context_Access;

   procedure Remove_Cookie (Name : in String);

   --  ------------------------------
   --  Given an Email address, return the Gravatar link to the user image.
   --  (See http://en.gravatar.com/site/implement/hash/ and
   --  http://en.gravatar.com/site/implement/images/)
   --  ------------------------------
   function Get_Gravatar_Link (Email : in String) return String is
      E : constant String := Util.Strings.Transforms.To_Lower_Case (Email);
      C : constant GNAT.MD5.Message_Digest := GNAT.MD5.Digest (E);
   begin
      return "http://www.gravatar.com/avatar/" & C;
   end Get_Gravatar_Link;

   --  ------------------------------
   --  Get the user information identified by the given name.
   --  ------------------------------
   overriding
   function Get_Value (From : in User_Info;
                       Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (From);

      F : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      S : ASF.Sessions.Session;
      P : ASF.Principals.Principal_Access := null;
      U : Security.Auth.Principal_Access := null;
   begin
      if F /= null then
         S := F.Get_Session;
         if S.Is_Valid then
            P := S.Get_Principal;
            if P /= null then
               U := Security.Auth.Principal'Class (P.all)'Access;
            end if;
         end if;
      end if;
      if Name = "authenticated" then
         return Util.Beans.Objects.To_Object (U /= null);
      end if;
      if U = null then
         return Util.Beans.Objects.Null_Object;
      end if;
      if Name = "email" then
         return Util.Beans.Objects.To_Object (U.Get_Email);
      end if;
      if Name = "language" then
         return Util.Beans.Objects.To_Object (Get_Language (U.Get_Authentication));
      end if;
      if Name = "first_name" then
         return Util.Beans.Objects.To_Object (Get_First_Name (U.Get_Authentication));
      end if;
      if Name = "last_name" then
         return Util.Beans.Objects.To_Object (Get_Last_Name (U.Get_Authentication));
      end if;
      if Name = "full_name" then
         return Util.Beans.Objects.To_Object (Get_Full_Name (U.Get_Authentication));
      end if;
      if Name = "id" then
         return Util.Beans.Objects.To_Object (Get_Claimed_Id (U.Get_Authentication));
      end if;
      if Name = "country" then
         return Util.Beans.Objects.To_Object (Get_Country (U.Get_Authentication));
      end if;
      if Name = "sessionId" then
         return Util.Beans.Objects.To_Object (S.Get_Id);
      end if;
      if Name = "gravatar" then
         return Util.Beans.Objects.To_Object (Get_Gravatar_Link (U.Get_Email));
      end if;

      return Util.Beans.Objects.To_Object (U.Get_Name);
   end Get_Value;

   --  ------------------------------
   --  Helper to send a remove cookie in the current response
   --  ------------------------------
   procedure Remove_Cookie (Name : in String) is
      Ctx : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      C   : ASF.Cookies.Cookie := ASF.Cookies.Create (Name, "");
   begin
      ASF.Cookies.Set_Path (C, Ctx.Get_Request.Get_Context_Path);
      ASF.Cookies.Set_Max_Age (C, 0);
      Ctx.Get_Response.Add_Cookie (Cookie => C);
   end Remove_Cookie;

   --  ------------------------------
   --  Logout by dropping the user session.
   --  ------------------------------
   procedure Logout (From    : in out User_Info;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (From);

      F : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      S : ASF.Sessions.Session := F.Get_Session;
      P : ASF.Principals.Principal_Access := null;
      U : Security.Auth.Principal_Access := null;
   begin
      if S.Is_Valid then
         P := S.Get_Principal;
         if P /= null then
            U := Security.Auth.Principal'Class (P.all)'Access;
         end if;
         S.Invalidate;
      end if;
      if U /= null then
         F.Get_Flash.Set_Keep_Messages (True);
         ASF.Applications.Messages.Factory.Add_Message (F.all, "samples.openid_logout_message",
                                                        Get_Full_Name (U.Get_Authentication));
      end if;
      Remove_Cookie (ASF.Security.Filters.SID_COOKIE);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("logout_success");
   end Logout;

   package Logout_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => User_Info,
                                                      Method => Logout,
                                                      Name   => "logout");

   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Logout_Binding.Proxy'Unchecked_Access);

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in User_Info)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Binding_Array'Access;
   end Get_Method_Bindings;

end Users;
