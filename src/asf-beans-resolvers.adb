-----------------------------------------------------------------------
--  asf-beans-resolvers -- Resolver to create and give access to managed beans
--  Copyright (C) 2013, 2017 Stephane Carrez
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

with ASF.Sessions;
with ASF.Beans.Params;
with ASF.Beans.Flash;
with ASF.Beans.Globals;
with ASF.Beans.Headers;
with ASF.Beans.Requests;
package body ASF.Beans.Resolvers is

   --  ------------------------------
   --  Initialize the EL resolver to use the application bean factory and the given request.
   --  ------------------------------
   procedure Initialize (Resolver : in out ELResolver;
                         App      : in ASF.Applications.Main.Application_Access;
                         Request  : in ASF.Requests.Request_Access) is
   begin
      Resolver.Application       := App;
      Resolver.Request           := Request;
      Resolver.Attributes_Access := Resolver.Attributes'Unchecked_Access;
   end Initialize;

   --  ------------------------------
   --  Resolve the name represented by <tt>Name</tt> according to a base object <tt>Base</tt>.
   --  The resolver tries to look first in pre-defined objects (params, flash, headers, initParam).
   --  It then looks in the request and session attributes for the value.  If the value was
   --  not in the request or session, it uses the application bean factory to create the
   --  new managed bean and adds it to the request or session.
   --  ------------------------------
   overriding
   function Get_Value (Resolver : in ELResolver;
                       Context  : in EL.Contexts.ELContext'Class;
                       Base     : access Util.Beans.Basic.Readonly_Bean'Class;
                       Name     : in Ada.Strings.Unbounded.Unbounded_String)
                       return Util.Beans.Objects.Object is
      use type Util.Beans.Basic.Readonly_Bean_Access;
      use type ASF.Requests.Request_Access;

      Key    : constant String := To_String (Name);
   begin
      if Base /= null then
         return Base.Get_Value (Key);

      elsif Key = ASF.Beans.Params.PARAM_ATTRIBUTE_NAME then
         return ASF.Beans.Params.Instance;

      elsif Key = ASF.Beans.Headers.HEADER_ATTRIBUTE_NAME then
         return ASF.Beans.Headers.Instance;

      elsif Key = ASF.Beans.Flash.FLASH_ATTRIBUTE_NAME then
         return ASF.Beans.Flash.Instance;

      elsif Key = ASF.Beans.Globals.INIT_PARAM_ATTRIBUTE_NAME then
         return ASF.Beans.Globals.Instance;

      elsif Key = ASF.Beans.Requests.REQUEST_ATTRIBUTE_NAME then
         return ASF.Beans.Requests.Instance;
      end if;
      declare
         Result : Util.Beans.Objects.Object;
         Scope  : Scope_Type;
         Bean   : Util.Beans.Basic.Readonly_Bean_Access;

         --  Look in the resolver's attributes first.
         Pos    : constant Util.Beans.Objects.Maps.Cursor := Resolver.Attributes.Find (Key);
      begin
         if Util.Beans.Objects.Maps.Has_Element (Pos) then
            return Util.Beans.Objects.Maps.Element (Pos);

         elsif Resolver.Request /= null then
            Result := Resolver.Request.Get_Attribute (Key);
            if not Util.Beans.Objects.Is_Null (Result) then
               return Result;
            end if;

            --  If there is a session, look if the attribute is defined there.
            declare
               Session : ASF.Sessions.Session := Resolver.Request.Get_Session;
            begin
               if Session.Is_Valid then
                  Result := Session.Get_Attribute (Key);
                  if not Util.Beans.Objects.Is_Null (Result) then
                     return Result;
                  end if;
               end if;

               Resolver.Application.Create (Name, Context, Bean, Scope);
               if Bean = null then
                  return Resolver.Application.Get_Global (Name, Context);
               end if;
               Result := Util.Beans.Objects.To_Object (Bean);
               if Scope = SESSION_SCOPE then
                  Session.Set_Attribute (Name  => Key,
                                         Value => Result);
               else
                  Resolver.Request.Set_Attribute (Name  => Key,
                                                  Value => Result);
               end if;
               return Result;
            end;
         else
            Resolver.Application.Create (Name, Context, Bean, Scope);
            if Bean = null then
               return Resolver.Application.Get_Global (Name, Context);
            end if;
            Result := Util.Beans.Objects.To_Object (Bean);
            Resolver.Attributes_Access.Include (Key, Result);
            return Result;
         end if;
      end;
   end Get_Value;

   --  ------------------------------
   --  Sets the value represented by the <tt>Name</tt> in the base object <tt>Base</tt>.
   --  If there is no <tt>Base</tt> object, the request attribute with the given name is
   --  updated to the given value.
   --  ------------------------------
   overriding
   procedure Set_Value (Resolver : in out ELResolver;
                        Context  : in EL.Contexts.ELContext'Class;
                        Base     : access Util.Beans.Basic.Bean'Class;
                        Name     : in Ada.Strings.Unbounded.Unbounded_String;
                        Value    : in Util.Beans.Objects.Object) is
      pragma Unreferenced (Context);

      Key : constant String := To_String (Name);
   begin
      if Base /= null then
         Base.Set_Value (Name => Key, Value => Value);
      else
         Resolver.Request.Set_Attribute (Name => Key, Value => Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Initialize the resolver.
   --  ------------------------------
   overriding
   procedure Initialize (Resolver : in out ELResolver) is
   begin
      Resolver.Attributes_Access := Resolver.Attributes'Unchecked_Access;
   end Initialize;

end ASF.Beans.Resolvers;
