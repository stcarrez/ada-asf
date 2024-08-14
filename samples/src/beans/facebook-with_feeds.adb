-----------------------------------------------------------------------
--  facebook - Use Facebook Graph API
--  Copyright (C) 2012, 2013, 2014, 2015, 2022 Stephane Carrez
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
with Util.Http.Rest.Rest_Get_Vector;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.Mappers.Vector_Mapper;

with ASF.Sessions;
with ASF.Contexts.Faces;
with ASF.Events.Faces.Actions;

package body Facebook is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Facebook");

   type Friend_Field_Type is (FIELD_NAME, FIELD_ID);
   type Feed_Field_Type is (FIELD_ID, FIELD_NAME, FIELD_FROM, FIELD_MESSAGE,
                            FIELD_PICTURE, FIELD_LINK, FIELD_DESCRIPTION, FIELD_ICON);

   procedure Set_Member (Into : in out Friend_Info;
                         Field : in Friend_Field_Type;
                         Value : in Util.Beans.Objects.Object);

   procedure Set_Member (Into : in out Feed_Info;
                         Field : in Feed_Field_Type;
                         Value : in Util.Beans.Objects.Object);

   procedure Set_Member (Into : in out Friend_Info;
                         Field : in Friend_Field_Type;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_ID =>
            Into.Id := Value;

         when FIELD_NAME =>
            Into.Name := Value;

      end case;
   end Set_Member;

   procedure Set_Member (Into : in out Feed_Info;
                         Field : in Feed_Field_Type;
                         Value : in Util.Beans.Objects.Object) is
   begin
      Log.Info ("Set field {0} to {1}", Feed_Field_Type'Image (Field),
                Util.Beans.Objects.To_String (Value));

      case Field is
         when FIELD_ID =>
            Into.Id := Value;

         when FIELD_NAME =>
            Into.Name := Value;

         when FIELD_FROM =>
            Into.From := Value;

         when FIELD_MESSAGE =>
            Into.Message := Value;

         when FIELD_LINK =>
            Into.Link := Value;

         when FIELD_PICTURE =>
            Into.Picture := Value;

         when FIELD_ICON =>
            Into.Icon := Value;

         when FIELD_DESCRIPTION =>
            Into.Description := Value;

      end case;
   end Set_Member;

   package Friend_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Friend_Info,
                                               Element_Type_Access => Friend_Info_Access,
                                               Fields              => Friend_Field_Type,
                                               Set_Member          => Set_Member);

   package Friend_Vector_Mapper is
      new Util.Serialize.Mappers.Vector_Mapper (Vectors        => Friend_List.Vectors,
                                                Element_Mapper => Friend_Mapper);

   package Feed_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Feed_Info,
                                               Element_Type_Access => Feed_Info_Access,
                                               Fields              => Feed_Field_Type,
                                               Set_Member          => Set_Member);

   package Feed_Vector_Mapper is
     new Util.Serialize.Mappers.Vector_Mapper (Vectors        => Feed_List.Vectors,
                                               Element_Mapper => Feed_Mapper);

   Friend_Map        : aliased Friend_Mapper.Mapper;
   Friend_Vector_Map : aliased Friend_Vector_Mapper.Mapper;
   Feed_Map          : aliased Feed_Mapper.Mapper;
   Feed_Vector_Map   : aliased Feed_Vector_Mapper.Mapper;

   procedure Get_Friends is
     new Util.Http.Rest.Rest_Get_Vector (Vector_Mapper => Friend_Vector_Mapper);

   procedure Get_Feeds is
     new Util.Http.Rest.Rest_Get_Vector (Vector_Mapper => Feed_Vector_Mapper);

   --  ------------------------------
   --  Get the access token from the user session.
   --  ------------------------------
   function Get_Access_Token return String is
      use type ASF.Contexts.Faces.Faces_Context_Access;

      Context : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
   begin
      if Context = null then
         return "";
      end if;
      declare
         S : constant ASF.Sessions.Session := Context.Get_Session;
      begin
         if not S.Is_Valid then
            return "";
         end if;
         declare
            Token : constant Util.Beans.Objects.Object := S.Get_Attribute ("access_token");
         begin
            if Util.Beans.Objects.Is_Null (Token) then
               return "";
            else
               return Util.Beans.Objects.To_String (Token);
            end if;
         end;
      end;
   end Get_Access_Token;

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
   function Get_Value (From : in Feed_Info;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "id" then
         return From.Id;
      elsif Name = "name" then
         return From.Name;
      elsif Name = "from" then
         return From.From;
      elsif Name = "message" then
         return From.Message;
      elsif Name = "picture" then
         return From.Picture;
      elsif Name = "link" then
         return From.Link;
      elsif Name = "description" then
         return From.Description;
      elsif Name = "icon" then
         return From.Icon;
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
      List  : Friend_List_Bean_Access := new Friend_List_Bean;
      Token : constant String := Get_Access_Token;
   begin
      if Token'Length > 0 then
         Log.Info ("Getting the Facebook friends");

         Get_Friends ("https://graph.facebook.com/me/friends?access_token="
                      & Token,
                      Friend_Vector_Map'Access,
                      "/data",
                      List.List'Access);
      end if;

      return List.all'Access;
   end Create_Friends_Bean;

   --  ------------------------------
   --  Build and return a Facebook feed list.
   --  ------------------------------
   function Create_Feed_List_Bean return Util.Beans.Basic.Readonly_Bean_Access is
      List  : Feed_List.List_Bean_Access := new Feed_List.List_Bean;
      Token : constant String := Get_Access_Token;
   begin
      if Token'Length > 0 then
         Log.Info ("Getting the Facebook feeds");

         Get_Feeds ("https://graph.facebook.com/me/home?access_token="
                    & Token,
                    Feed_Vector_Map'Access,
                    "/data",
                    List.List'Access);
      end if;

      return List.all'Access;
   end Create_Feed_List_Bean;

   --  ------------------------------
   --  Get the user information identified by the given name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Facebook_Auth;
                       Name : in String) return Util.Beans.Objects.Object is
      use type ASF.Contexts.Faces.Faces_Context_Access;

      F    : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
   begin
      if F /= null and then Name = "authenticate_url" then
         declare
            S      : constant ASF.Sessions.Session := F.Get_Session (True);
            Id     : constant String := S.Get_Id;
            State  : constant String := From.Get_State (Id);
            Params : constant String := From.Get_Auth_Params (State, "user_posts");
         begin
            Log.Info ("OAuth params: {0}", Params);

            return Util.Beans.Objects.To_Object ("https://www.facebook.com/dialog/oauth?"
                                                 & Params);
         end;
      elsif F /= null and then Name = "isAuthenticated" then
         declare
            S      : constant ASF.Sessions.Session := F.Get_Session (False);
         begin
            if S.Is_Valid and
            then not Util.Beans.Objects.Is_Null (S.Get_Attribute ("access_token"))
            then
               return Util.Beans.Objects.To_Object (True);
            else
               return Util.Beans.Objects.To_Object (False);
            end if;
         end;
      end if;
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Authenticate result from Facebook.
   --  ------------------------------
   procedure Authenticate (From    : in out Facebook_Auth;
                           Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);

      use type Security.OAuth.Clients.Access_Token_Access;

      F       : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Session : ASF.Sessions.Session := F.Get_Session;
      State   : constant String := F.Get_Parameter (Security.OAuth.STATE);
      Code    : constant String := F.Get_Parameter (Security.OAuth.CODE);
   begin
      Log.Info ("Auth code {0} for state {1}", Code, State);
      if Session.Is_Valid then
         if From.Is_Valid_State (Session.Get_Id, State) then
            declare
               Acc : constant Security.OAuth.Clients.Access_Token_Access
                 := From.Request_Access_Token (Code);
            begin
               if Acc /= null then
                  Log.Info ("Access token is {0}", Acc.Get_Name);
                  Session.Set_Attribute ("access_token",
                                         Util.Beans.Objects.To_Object (Acc.Get_Name));
               end if;
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
      pragma Unreferenced (From);
   begin
      return Binding_Array'Access;
   end Get_Method_Bindings;

begin
   Friend_Map.Add_Default_Mapping;
   Friend_Vector_Map.Set_Mapping (Friend_Map'Access);

   Feed_Map.Add_Mapping ("id", FIELD_ID);
   Feed_Map.Add_Mapping ("name", FIELD_NAME);
   Feed_Map.Add_Mapping ("message", FIELD_MESSAGE);
   Feed_Map.Add_Mapping ("description", FIELD_DESCRIPTION);
   Feed_Map.Add_Mapping ("from/name", FIELD_FROM);
   Feed_Map.Add_Mapping ("picture", FIELD_PICTURE);
   Feed_Map.Add_Mapping ("link", FIELD_LINK);
   Feed_Map.Add_Mapping ("icon", FIELD_ICON);
   Feed_Vector_Map.Set_Mapping (Feed_Map'Access);
end Facebook;
