-----------------------------------------------------------------------
--  asf-models-selects -- Data model for UISelectOne and UISelectMany
--  Copyright (C) 2011 Stephane Carrez
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

package body ASF.Models.Selects is

   --  ------------------------------
   --  Return an Object from the select item record.
   --  Returns a NULL object if the item is empty.
   --  ------------------------------
   function To_Object (Item : in Select_Item) return Util.Beans.Objects.Object is
   begin
      if Item.Item.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         declare
            Bean : constant Select_Item_Access := new Select_Item;
         begin
            Bean.all := Item;
            return Util.Beans.Objects.To_Object (Bean.all'Access);
         end;
      end if;
   end To_Object;

   --  ------------------------------
   --  Return the <b>Select_Item</b> instance from a generic bean object.
   --  Returns an empty item if the object does not hold a <b>Select_Item</b>.
   --  ------------------------------
   function To_Select_Item (Object : in Util.Beans.Objects.Object) return Select_Item is
      Bean   : constant access Util.Beans.Basic.Readonly_Bean'Class
        := Util.Beans.Objects.To_Bean (Object);
      Result : Select_Item;
   begin
      if Bean = null then
         return Result;
      end if;
      if not (Bean.all in Select_Item'Class) then
         return Result;
      end if;
      Result := Select_Item (Bean.all);
      return Result;
   end To_Select_Item;

   --  ------------------------------
   --  Creates a <b>Select_Item</b> with the specified label and value.
   --  ------------------------------
   function Create_Select_Item (Label       : in Wide_Wide_String;
                                Value       : in String;
                                Description : in Wide_Wide_String := "";
                                Disabled    : in Boolean := False;
                                Escaped     : in Boolean := True) return Select_Item is
      Result : Select_Item;
   begin
      Result.Item := Select_Item_Refs.Create;
      declare
         Item : constant Select_Item_Record_Access := Result.Item.Value;
      begin
         Item.Label       := To_Unbounded_Wide_Wide_String (Label);
         --  Item.Value       := To_Unbounded_Wide_Wide_String (Value);
         Item.Description := To_Unbounded_Wide_Wide_String (Description);
         Item.Disabled    := Disabled;
         Item.Escape      := Escaped;
      end;
      return Result;
   end Create_Select_Item;

   --  ------------------------------
   --  Creates a <b>Select_Item</b> with the specified label and value.
   --  ------------------------------
   function Create_Select_Item (Label       : in Wide_Wide_String;
                                Value       : in Wide_Wide_String;
                                Description : in Wide_Wide_String;
                                Disabled    : in Boolean := False;
                                Escaped     : in Boolean := True) return Select_Item is
      Result : Select_Item;
   begin
      Result.Item := Select_Item_Refs.Create;
      declare
         Item : constant Select_Item_Record_Access := Result.Item.Value;
      begin
         Item.Label       := To_Unbounded_Wide_Wide_String (Label);
         Item.Value       := To_Unbounded_Wide_Wide_String (Value);
         Item.Description := To_Unbounded_Wide_Wide_String (Description);
         Item.Disabled    := Disabled;
         Item.Escape      := Escaped;
      end;
      return Result;
   end Create_Select_Item;

   --  ------------------------------
   --  Get the item label.
   --  ------------------------------
   function Get_Label (Item : in Select_Item) return Wide_Wide_String is
   begin
      if Item.Item.Is_Null then
         return "";
      else
         return To_Wide_Wide_String (Item.Item.Value.Label);
      end if;
   end Get_Label;

   --  ------------------------------
   --  Get the item value.
   --  ------------------------------
   function Get_Value (Item : in Select_Item) return Wide_Wide_String is
   begin
      if Item.Item.Is_Null then
         return "";
      else
         return To_Wide_Wide_String (Item.Item.Value.Value);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the item description.
   --  ------------------------------
   function Get_Description (Item : in Select_Item) return Wide_Wide_String is
   begin
      if Item.Item.Is_Null then
         return "";
      else
         return To_Wide_Wide_String (Item.Item.Value.Description);
      end if;
   end Get_Description;

   --  ------------------------------
   --  Returns true if the item is disabled.
   --  ------------------------------
   function Is_Disabled (Item : in Select_Item) return Boolean is
   begin
      if Item.Item.Is_Null then
         return False;
      else
         return Item.Item.Value.Disabled;
      end if;
   end Is_Disabled;

   --  ------------------------------
   --  Returns true if the
   --  ------------------------------
   function Is_Escaped (Item : in Select_Item) return Boolean is
   begin
      if Item.Item.Is_Null then
         return False;
      else
         return Item.Item.Value.Escape;
      end if;
   end Is_Escaped;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Select_Item;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if From.Item.Is_Null then
         return Util.Beans.Objects.Null_Object;
      end if;
      declare
         Item : constant Select_Item_Record_Access := From.Item.Value;
      begin
         if Name = "name" then
            return Util.Beans.Objects.To_Object (Item.Label);
         elsif Name = "value" then
            return Util.Beans.Objects.To_Object (Item.Value);
         elsif Name = "description" then
            return Util.Beans.Objects.To_Object (Item.Description);
         elsif Name = "disabled" then
            return Util.Beans.Objects.To_Object (Item.Disabled);
         elsif Name = "escaped" then
            return Util.Beans.Objects.To_Object (Item.Escape);
         else
            return Util.Beans.Objects.Null_Object;
         end if;
      end;
   end Get_Value;

   --  ------------------------------
   --  Select Item List
   --  ------------------------------

   --  Return an Object from the select item list.
   --  Returns a NULL object if the list is empty.
   function To_Object (Item : in Select_Item_List) return Util.Beans.Objects.Object is
   begin
      if Item.List.Is_Null then
         return Util.Beans.Objects.Null_Object;
      else
         declare
            Bean : constant Select_Item_List_Access := new Select_Item_List;
         begin
            Bean.all := Item;
            return Util.Beans.Objects.To_Object (Bean.all'Access);
         end;
      end if;
   end To_Object;

   --  Return the <b>Select_Item_List</b> instance from a generic bean object.
   --  Returns an empty list if the object does not hold a <b>Select_Item_List</b>.
   function To_Select_Item_List (Object : in Util.Beans.Objects.Object) return Select_Item_List is
      Bean : constant access Util.Beans.Basic.Readonly_Bean'Class
        := Util.Beans.Objects.To_Bean (Object);
      Result : Select_Item_List;
   begin
      if Bean = null then
         return Result;
      end if;
      if not (Bean.all in Select_Item_List'Class) then
         return Result;
      end if;
      Result := Select_Item_List (Bean.all);
      return Result;
   end To_Select_Item_List;

   --  ------------------------------
   --  Get the number of items in the list.
   --  ------------------------------
   function Length (List : in Select_Item_List) return Natural is
   begin
      if List.List.Is_Null then
         return 0;
      else
         return Natural (List.List.Value.List.Length);
      end if;
   end Length;


   --  Add the item at the end of the list.
   procedure Append (List : in out Select_Item_List;
                     Item : in Select_Item'Class) is
   begin
      null;
   end Append;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Select_Item_List;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

end ASF.Models.Selects;
