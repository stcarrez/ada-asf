-----------------------------------------------------------------------
--  components-utils-beans -- Bean component utility
--  Copyright (C) 2011, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Beans.Objects;
package body ASF.Components.Utils.Beans is

   --  ------------------------------
   --  Evaluate the <b>value</b> attribute and set it in the value expression
   --  referred to by the <b>var</b> attribute.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UISetBean;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Name : constant String := Util.Beans.Objects.To_String (UI.Get_Attribute (Context, "var"));
   begin
      if Name'Length > 0 then
         Context.Set_Attribute (Name, UI.Get_Attribute (Context => Context, Name => "value"));
      else
         declare
            VE : constant EL.Expressions.Value_Expression := UI.Get_Value_Expression ("var");
         begin
            if VE.Is_Null then
               UI.Log_Error ("Invalid value expression for 'var'");
               return;
            end if;
            VE.Set_Value (Context => Context.Get_ELContext.all,
                          Value   => UI.Get_Attribute (Context => Context, Name => "value"));
         end;
      end if;
   end Encode_Begin;

end ASF.Components.Utils.Beans;
