-----------------------------------------------------------------------
--  contexts-facelets-flash -- Flash context
--  Copyright (C) 2012 Stephane Carrez
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

package body ASF.Contexts.Flash is

   --  ------------------------------
   --  Set the attribute having given name with the value.
   --  ------------------------------
   procedure Set_Attribute (Context : in out Flash_Context;
                            Name    : in String;
                            Value   : in Util.Beans.Objects.Object) is
      Flash : Flash_Bean_Access;
   begin
      Context.Get_Active_Flash (Flash);
      if Util.Beans.Objects.Is_Null (Value) then
         Flash.Attributes.Delete (Name);
      else
         Flash.Attributes.Include (Name, Value);
      end if;
   end Set_Attribute;

   --  ------------------------------
   --  Set the attribute having given name with the value.
   --  ------------------------------
   procedure Set_Attribute (Context : in out Flash_Context;
                            Name    : in Unbounded_String;
                            Value   : in Util.Beans.Objects.Object) is
   begin
      Context.Set_Attribute (To_String (Name), Value);
   end Set_Attribute;

   --  Keep in the flash context the request attribute identified by the name <b>Name</b>.
   procedure Keep (Context : in out Flash_Context;
                   Name    : in String) is
   begin
      null;
   end Keep;

   --  Returns True if the <b>Redirect</b> property was set on the previous flash instance.
   function Is_Redirect (Context : in Flash_Context) return Boolean is
   begin
      return False;
   end Is_Redirect;

   --  Set this property to True to indicate to the next request on this session will be
   --  a redirect.  After this call, the next request will return the <b>Redirect</b> value
   --  when the <b>Is_Redirect</b> function will be called.
   procedure Set_Redirect (Context  : in out Flash_Context;
                           Redirect : in Boolean) is
   begin
      null;
   end Set_Redirect;

   --  Perform any specific action before processing the phase referenced by <b>Phase</b>.
   --  This operation is used to restore the flash context for a new request.
   procedure Do_Pre_Phase_Actions (Context  : in out Flash_Context;
                                   Phase    : in ASF.Events.Phases.Phase_Type) is
   begin
      null;
   end Do_Pre_Phase_Actions;

   --  Perform any specific action after processing the phase referenced by <b>Phase</b>.
   --  This operation is used to save the flash context
   procedure Do_Post_Phase_Actions (Context  : in out Flash_Context;
                                    Phase    : in ASF.Events.Phases.Phase_Type) is
   begin
      null;
   end Do_Post_Phase_Actions;

   procedure Get_Active_Flash (Context : in out Flash_Context;
                               Result  : out Flash_Bean_Access) is
   begin
      if Context.Previous = null then
         null;
      end if;
      Result := Context.Previous;
   end Get_Active_Flash;

   procedure Get_Execute_Flash (Context : in out Flash_Context;
                                Result  : out Flash_Bean_Access) is
   begin
      if Context.Next = null then
         null;
      end if;
      Result := Context.Next;
   end Get_Execute_Flash;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Value (From : in Flash_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

end ASF.Contexts.Flash;
