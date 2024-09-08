-----------------------------------------------------------------------
--  asf-beans-requests -- Bean giving access to the request object
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Basic;
with Util.Beans.Objects;

package ASF.Beans.Requests is

   --  Context variable giving access to the request object.
   REQUEST_ATTRIBUTE_NAME    : constant String := "requestScope";

   --  ------------------------------
   --  Request Bean
   --  ------------------------------
   --  The <b>Request_Bean</b> gives access to the request object.
   --  The bean instance is global to the application.
   type Request_Bean is new Util.Beans.Basic.Readonly_Bean with private;

   --  Get from the request object the value identified by the given name.
   --  Returns Null_Object if the request does not define such name.
   overriding
   function Get_Value (Bean : in Request_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Return the Request_Bean instance.
   function Instance return Util.Beans.Objects.Object;

private

   type Request_Bean is new Util.Beans.Basic.Readonly_Bean with null record;

end ASF.Beans.Requests;
