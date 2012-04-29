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

with ASF.Sessions;
package body ASF.Contexts.Flash is

   --  ------------------------------
   --  Set the attribute having given name with the value.
   --  ------------------------------
   procedure Set_Attribute (Flash   : in out Flash_Context;
                            Name    : in String;
                            Value   : in Util.Beans.Objects.Object) is
      Instance : Flash_Bean_Access;
   begin
      Flash.Get_Execute_Flash (Instance);
      if Util.Beans.Objects.Is_Null (Value) then
         Instance.Attributes.Delete (Name);
      else
         Instance.Attributes.Include (Name, Value);
      end if;
   end Set_Attribute;

   --  ------------------------------
   --  Set the attribute having given name with the value.
   --  ------------------------------
   procedure Set_Attribute (Flash   : in out Flash_Context;
                            Name    : in Unbounded_String;
                            Value   : in Util.Beans.Objects.Object) is
   begin
      Flash.Set_Attribute (To_String (Name), Value);
   end Set_Attribute;

   --  Keep in the flash context the request attribute identified by the name <b>Name</b>.
   procedure Keep (Flash   : in out Flash_Context;
                   Name    : in String) is
   begin
      null;
   end Keep;

   --  Returns True if the <b>Redirect</b> property was set on the previous flash instance.
   function Is_Redirect (Flash : in Flash_Context) return Boolean is
   begin
      return False;
   end Is_Redirect;

   --  Set this property to True to indicate to the next request on this session will be
   --  a redirect.  After this call, the next request will return the <b>Redirect</b> value
   --  when the <b>Is_Redirect</b> function will be called.
   procedure Set_Redirect (Flash    : in out Flash_Context;
                           Redirect : in Boolean) is
   begin
      null;
   end Set_Redirect;

   --  ------------------------------
   --  Perform any specific action before processing the phase referenced by <b>Phase</b>.
   --  This operation is used to restore the flash context for a new request.
   --  ------------------------------
   procedure Do_Pre_Phase_Actions (Flash   : in out Flash_Context;
                                   Phase   : in ASF.Events.Phases.Phase_Type;
                                   Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use type ASF.Events.Phases.Phase_Type;
      use type Util.Beans.Basic.Readonly_Bean_Access;
   begin
      --  Restore the flash bean instance from the session if there is one.
      if Phase = ASF.Events.Phases.RESTORE_VIEW then
         declare
            S : constant ASF.Sessions.Session := Context.Get_Session;
            B : access Util.Beans.Basic.Readonly_Bean'Class;
         begin
            if S.Is_Valid then
               Flash.Object := S.Get_Attribute ("asf.flash.bean");
               B := Util.Beans.Objects.To_Bean (Flash.Object);
               if B /= null and then B.all in Flash_Bean'Class then
                  Flash.Previous := Flash_Bean'Class (B.all)'Unchecked_Access;
               end if;
            end if;
         end;
      end if;
   end Do_Pre_Phase_Actions;

   --  Perform any specific action after processing the phase referenced by <b>Phase</b>.
   --  This operation is used to save the flash context
   procedure Do_Post_Phase_Actions (Flash   : in out Flash_Context;
                                    Phase   : in ASF.Events.Phases.Phase_Type;
                                    Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use type ASF.Events.Phases.Phase_Type;
   begin
      if Phase = ASF.Events.Phases.INVOKE_APPLICATION then
         Flash.Do_Last_Phase_Actions (Context);
      end if;
   end Do_Post_Phase_Actions;


   --  Perform the last actions that must be made to save the flash context in the session.
   procedure Do_Last_Phase_Actions (Flash   : in out Flash_Context;
                                    Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      S : ASF.Sessions.Session := Context.Get_Session;
   begin
      if S.Is_Valid then
         S.Set_Attribute ("asf.flash.bean", Util.Beans.Objects.Null_Object);
      elsif Flash.Next /= null then
         S := Context.Get_Session (Create => True);
      end if;

      if Flash.Next /= null then
         S.Set_Attribute ("asf.flash.bean", Util.Beans.Objects.To_Object (Flash.Next.all'Access));
      end if;
   end Do_Last_Phase_Actions;

   procedure Get_Active_Flash (Flash   : in out Flash_Context;
                               Result  : out Flash_Bean_Access) is
   begin
      if Flash.Previous = null then
         null;
      end if;
      Result := Flash.Previous;
   end Get_Active_Flash;

   procedure Get_Execute_Flash (Flash   : in out Flash_Context;
                                Result  : out Flash_Bean_Access) is
   begin
      if Flash.Next = null then
         Flash.Next := new Flash_Bean;
      end if;
      Result := Flash.Next;
   end Get_Execute_Flash;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   function Get_Value (From : in Flash_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

end ASF.Contexts.Flash;
