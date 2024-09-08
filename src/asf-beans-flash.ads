-----------------------------------------------------------------------
--  asf-beans-flash -- Bean giving access to the flash context
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Basic;
with Util.Beans.Objects;

package ASF.Beans.Flash is

   --  Context variable giving access to the request headers.
   FLASH_ATTRIBUTE_NAME    : constant String := "flash";

   KEEP_MESSAGES_ATTR_NAME : constant String := "keepMessages";
   REDIRECT_ATTR_NAME      : constant String := "redirect";

   --  ------------------------------
   --  Flash Bean
   --  ------------------------------
   --  The <b>Flash_Bean</b> gives access to the flash context.
   type Flash_Bean is new Util.Beans.Basic.Readonly_Bean with private;

   --  Get the request header identified by the given name.
   --  Returns Null_Object if the request does not define such header.
   overriding
   function Get_Value (Bean : in Flash_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Return the Flash_Bean instance.
   function Instance return Util.Beans.Objects.Object;

private

   type Flash_Bean is new Util.Beans.Basic.Readonly_Bean with null record;

end ASF.Beans.Flash;
