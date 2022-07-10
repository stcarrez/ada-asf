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
