-----------------------------------------------------------------------
--  asf-events -- ASF Events
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
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
