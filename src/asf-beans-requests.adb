-----------------------------------------------------------------------
--  asf-beans-requests -- Bean giving access to the request object
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Contexts.Faces;
with ASF.Requests;
package body ASF.Beans.Requests is

   Bean : aliased Request_Bean;

   --  ------------------------------
   --  Get from the request object the value identified by the given name.
   --  Returns Null_Object if the request does not define such name.
   --  ------------------------------
   overriding
   function Get_Value (Bean : in Request_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (Bean);
      use type ASF.Contexts.Faces.Faces_Context_Access;

      Ctx : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
   begin
      if Ctx = null then
         return Util.Beans.Objects.Null_Object;
      end if;
      declare
         Req    : constant ASF.Requests.Request_Access := Ctx.Get_Request;
      begin
         return Req.Get_Attribute (Name);
      end;
   end Get_Value;

   --  ------------------------------
   --  Return the Request_Bean instance.
   --  ------------------------------
   function Instance return Util.Beans.Objects.Object is
   begin
      return Util.Beans.Objects.To_Object (Bean'Access, Util.Beans.Objects.STATIC);
   end Instance;

end ASF.Beans.Requests;
