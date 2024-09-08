-----------------------------------------------------------------------
--  asf-helpers-beans -- Helper packages to write ASF applications
--  Copyright (C) 2012, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Beans.Basic;

with ASF.Requests;
package ASF.Helpers.Beans is

   --  Get a bean instance associated under the given name from the current faces context.
   --  A null value is returned if the bean does not exist or is not of the good type.
   generic
      type Element_Type is new Util.Beans.Basic.Readonly_Bean with private;
      type Element_Access is access all Element_Type'Class;
   function Get_Bean (Name : in String) return Element_Access;

   --  Get a bean instance associated under the given name from the request.
   --  A null value is returned if the bean does not exist or is not of the good type.
   generic
      type Element_Type is new Util.Beans.Basic.Readonly_Bean with private;
      type Element_Access is access all Element_Type'Class;
   function Get_Request_Bean (Request : in ASF.Requests.Request'Class;
                              Name    : in String) return Element_Access;

end ASF.Helpers.Beans;
