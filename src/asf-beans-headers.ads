-----------------------------------------------------------------------
--  asf-beans-headers -- Bean giving access to the request headers
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Basic;
with Util.Beans.Objects;

package ASF.Beans.Headers is

   --  Context variable giving access to the request headers.
   HEADER_ATTRIBUTE_NAME    : constant String := "header";

   --  ------------------------------
   --  Request Header Bean
   --  ------------------------------
   --  The <b>Header_Bean</b> gives access to the request headers.
   --  The bean instance is global to the application.
   type Header_Bean is new Util.Beans.Basic.Readonly_Bean with private;

   --  Get the request header identified by the given name.
   --  Returns Null_Object if the request does not define such header.
   overriding
   function Get_Value (Bean : in Header_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Return the Header_Bean instance.
   function Instance return Util.Beans.Objects.Object;

private

   type Header_Bean is new Util.Beans.Basic.Readonly_Bean with null record;

end ASF.Beans.Headers;
