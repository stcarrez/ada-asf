-----------------------------------------------------------------------
--  asf-beans-flash -- Bean giving access to the flash context
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Contexts.Faces;
with ASF.Contexts.Flash;
package body ASF.Beans.Flash is

   Bean : aliased Flash_Bean;

   --  ------------------------------
   --  Get the request header identified by the given name.
   --  Returns Null_Object if the request does not define such header.
   --  ------------------------------
   overriding
   function Get_Value (Bean : in Flash_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (Bean);
      use type ASF.Contexts.Faces.Faces_Context_Access;

      Ctx   : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Flash : ASF.Contexts.Faces.Flash_Context_Access;
   begin
      if Ctx = null then
         return Util.Beans.Objects.Null_Object;
      end if;
      Flash := Ctx.Get_Flash;
      if Name = KEEP_MESSAGES_ATTR_NAME then
         return Util.Beans.Objects.To_Object (Flash.Is_Keep_Messages);

      else
         return Flash.Get_Attribute (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Return the Flash_Bean instance.
   --  ------------------------------
   function Instance return Util.Beans.Objects.Object is
   begin
      return Util.Beans.Objects.To_Object (Bean'Access, Util.Beans.Objects.STATIC);
   end Instance;

end ASF.Beans.Flash;
