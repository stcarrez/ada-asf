-----------------------------------------------------------------------
--  asf-beans-params -- Bean giving access to the request parameters
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Contexts.Faces;
with ASF.Requests;
package body ASF.Beans.Params is

   Bean : aliased Param_Bean;

   --  ------------------------------
   --  Get the request parameter identified by the given name.
   --  Returns Null_Object if the request does not define such parameter.
   --  ------------------------------
   overriding
   function Get_Value (Bean : in Param_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (Bean);
      use type ASF.Contexts.Faces.Faces_Context_Access;

      Ctx : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
   begin
      if Ctx = null then
         return Util.Beans.Objects.Null_Object;
      end if;
      declare
         Req   : constant ASF.Requests.Request_Access := Ctx.Get_Request;
         Param : constant String := Req.Get_Parameter (Name);
      begin
         if Param = "" then
            return Util.Beans.Objects.Null_Object;
         else
            return Util.Beans.Objects.To_Object (Param);
         end if;
      end;
   end Get_Value;

   --  ------------------------------
   --  Return the Param_Bean instance.
   --  ------------------------------
   function Instance return Util.Beans.Objects.Object is
   begin
      return Util.Beans.Objects.To_Object (Bean'Access, Util.Beans.Objects.STATIC);
   end Instance;

end ASF.Beans.Params;
