-----------------------------------------------------------------------
--  asf-beans-params -- Bean giving access to the request parameters
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Basic;
with Util.Beans.Objects;

package ASF.Beans.Params is

   --  Context variable giving access to the request parameters.
   PARAM_ATTRIBUTE_NAME    : constant String := "param";

   --  ------------------------------
   --  Request Parameter Bean
   --  ------------------------------
   --  The <b>Param_Bean</b> gives access to the request parameter.
   --  The bean instance is global to the application.
   type Param_Bean is new Util.Beans.Basic.Readonly_Bean with private;

   --  Get the request parameter identified by the given name.
   --  Returns Null_Object if the request does not define such parameter.
   overriding
   function Get_Value (Bean : in Param_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Return the Param_Bean instance.
   function Instance return Util.Beans.Objects.Object;

private

   type Param_Bean is new Util.Beans.Basic.Readonly_Bean with null record;

end ASF.Beans.Params;
