-----------------------------------------------------------------------
--  asf-beans-injections -- Injection of parameters, headers, cookies in beans
--  Copyright (C) 2015, 2022 Stephane Carrez
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
with ASF.Cookies;
package body ASF.Beans.Injections is

   --  ------------------------------
   --  Inject the request header whose name is defined by Descriptor.Param.
   --  ------------------------------
   procedure Header (Bean       : in out Util.Beans.Basic.Bean'Class;
                     Descriptor : in Inject_Type;
                     Request    : in ASF.Requests.Request'Class) is
      Value : constant String := Request.Get_Header (Descriptor.Param.all);
   begin
      Bean.Set_Value (Descriptor.Name.all, Util.Beans.Objects.To_Object (Value));
   end Header;

   --  ------------------------------
   --  Inject the request query string parameter whose name is defined by Descriptor.Param.
   --  ------------------------------
   procedure Query_Param (Bean       : in out Util.Beans.Basic.Bean'Class;
                          Descriptor : in Inject_Type;
                          Request    : in ASF.Requests.Request'Class) is
      Value : constant String := Request.Get_Parameter (Descriptor.Param.all);
   begin
      Bean.Set_Value (Descriptor.Name.all, Util.Beans.Objects.To_Object (Value));
   end Query_Param;

   --  ------------------------------
   --  Inject the request cookie whose name is defined by Descriptor.Param.
   --  ------------------------------
   procedure Cookie (Bean       : in out Util.Beans.Basic.Bean'Class;
                     Descriptor : in Inject_Type;
                     Request    : in ASF.Requests.Request'Class) is
      Cookies : constant ASF.Cookies.Cookie_Array := Request.Get_Cookies;
   begin
      for I in Cookies'Range loop
         if ASF.Cookies.Get_Name (Cookies (I)) = Descriptor.Param.all then
            Bean.Set_Value (Descriptor.Name.all,
                            Util.Beans.Objects.To_Object (ASF.Cookies.Get_Value (Cookies (I))));
         end if;
      end loop;
   end Cookie;

   --  ------------------------------
   --  Inject the request URI path component whose position is defined by Descriptor.Pos.
   --  ------------------------------
   procedure Path_Param (Bean       : in out Util.Beans.Basic.Bean'Class;
                         Descriptor : in Inject_Type;
                         Request    : in ASF.Requests.Request'Class) is
      URI   : constant String := Request.Get_Path_Info;
      Pos   : Natural := URI'First;
      Count : Natural := Descriptor.Pos;
      Next_Pos : Natural;
   begin
      while Count > 0 and then Pos < URI'Last loop
         Next_Pos := Util.Strings.Index (URI, '/', Pos + 1);
         Count := Count - 1;
         if Count = 0 then
            if Next_Pos = 0 then
               Next_Pos := URI'Last;
            else
               Next_Pos := Next_Pos - 1;
            end if;
            Bean.Set_Value (Descriptor.Name.all,
                            Util.Beans.Objects.To_Object (URI (Pos + 1 .. Next_Pos)));
            return;
         end if;
         Pos := Next_Pos;
      end loop;
   end Path_Param;

   --  ------------------------------
   --  Inject into the Ada bean a set of information extracted from the request object.
   --  The value is obtained from a request header, a cookie, a query string parameter or
   --  from a URI path component.  The value is injected by using the bean operation
   --  <tt>Set_Value</tt>.
   --  ------------------------------
   procedure Inject (Into    : in out Util.Beans.Basic.Bean'Class;
                     List    : in Inject_Array_Type;
                     Request : in ASF.Requests.Request'Class) is
   begin
      for I in List'Range loop
         List (I).Handler (Into, List (I), Request);
      end loop;
   end Inject;

end ASF.Beans.Injections;
