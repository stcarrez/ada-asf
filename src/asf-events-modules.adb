-----------------------------------------------------------------------
--  asf-events -- ASF Events
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body ASF.Events.Modules is

   --  ------------------------------
   --  Set a parameter on the message.
   --  ------------------------------
   procedure Set_Parameter (Message : in out Module_Event;
                            Name    : in String;
                            Value   : in String) is
   begin
      Message.Props.Set (Name, Value);
   end Set_Parameter;

   --  ------------------------------
   --  Get the parameter with the given name.
   --  ------------------------------
   function Get_Parameter (Message : in Module_Event;
                           Name    : String) return String is
   begin
      return Message.Props.Get (Name, "");
   end Get_Parameter;

   --  ------------------------------
   --  Get the value that corresponds to the parameter with the given name.
   --  ------------------------------
   overriding
   function Get_Value (Message : in Module_Event;
                       Name    : in String) return Util.Beans.Objects.Object is
   begin
      if Message.Props.Exists (Name) then
         return Util.Beans.Objects.To_Object (Message.Get_Parameter (Name));
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

end ASF.Events.Modules;
