-----------------------------------------------------------------------
--  asf-events -- ASF Events
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Applications;

with Util.Events;
with Util.Beans.Basic;
with Util.Beans.Objects;
package ASF.Events.Modules is

   type Module_Event is new Util.Events.Event and Util.Beans.Basic.Readonly_Bean with private;

   --  Set a parameter on the message.
   procedure Set_Parameter (Message : in out Module_Event;
                            Name    : in String;
                            Value   : in String);

   --  Get the parameter with the given name.
   function Get_Parameter (Message : in Module_Event;
                           Name    : String) return String;

   --  Get the value that corresponds to the parameter with the given name.
   overriding
   function Get_Value (Message : in Module_Event;
                       Name    : in String) return Util.Beans.Objects.Object;

private

   type Module_Event is new Util.Events.Event and Util.Beans.Basic.Readonly_Bean with record
      Props : ASF.Applications.Config;
   end record;

end ASF.Events.Modules;
