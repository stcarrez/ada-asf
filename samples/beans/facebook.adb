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
with Util.Log.Loggers;

with ASF.Sessions;
with ASF.Contexts.Faces;
with ASF.Events.Faces.Actions;

package body Facebook is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Facebook");

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Friend_Info;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "name" then
         return From.Name;
      elsif Name = "id" then
         return From.Id;
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Friend_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "hasAccessToken" then
         return Util.Beans.Objects.To_Object (False);
      end if;
      return Friend_List.List_Bean (From).Get_Value (Name);
   end Get_Value;

   --  ------------------------------
   --  Create a Friend_List bean instance.
   --  ------------------------------
   function Create_Friends_Bean return Util.Beans.Basic.Readonly_Bean_Access is
      List : Friend_List_Bean_Access := new Friend_List_Bean;
      F    : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      S    : ASF.Sessions.Session;
   begin
--        if F /= null then
--           S := F.Get_Session;
--           if S.Is_Valid then
--              S.Get_Attribute ("facebook_access");
--              P := S.Get_Principal;
--              if P /= null then
--                 U := Security.Openid.Principal'Class (P.all)'Access;
--              end if;
--           end if;
--        end if;

      return List.all'Access;
   end Create_Friends_Bean;

   --  ------------------------------
   --  Get the user information identified by the given name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Facebook_Auth;
                       Name : in String) return Util.Beans.Objects.Object is
      use type ASF.Contexts.Faces.Faces_Context_Access;

      F    : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
   begin
      if F /= null and Name = "authenticate_url" then
         declare
            S      : ASF.Sessions.Session := F.Get_Session (True);
            Id     : constant String := S.Get_Id;
            State  : constant String := From.Get_State (Id);
            Params : constant String := From.Get_Auth_Params (State, "");
         begin
            Log.Info ("OAuth params: {0}", Params);

            return Util.Beans.Objects.To_Object ("https://www.facebook.com/dialog/oauth?"
                                                 & Params);
         end;
      end if;
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Authenticate result from Facebook.
   --  ------------------------------
   procedure Authenticate (From    : in out Facebook_Auth;
                           Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      F       : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Session : ASF.Sessions.Session := F.Get_Session;
      State   : constant String := F.Get_Parameter (Security.OAuth.State);
      Code    : constant String := F.Get_Parameter (Security.OAuth.Code);
   begin
      Log.Info ("Auth code {0} for state {1}", Code, State);
      if Session.Is_Valid then
         if From.Is_Valid_State (Session.Get_Id, State) then
            declare
               Acc : Security.OAuth.Clients.Access_Token_Access
                 := From.Request_Access_Token (Code);
            begin
               Log.Info ("Access token is {0}", Acc.Get_Name);
            end;
         end if;
      end if;
   end Authenticate;

   package Authenticate_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Facebook_Auth,
                                                      Method => Authenticate,
                                                      Name   => "authenticate");

   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (1 => Authenticate_Binding.Proxy'Unchecked_Access);

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Facebook_Auth)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
   begin
      return Binding_Array'Access;
   end Get_Method_Bindings;

end Facebook;
