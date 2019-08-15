-----------------------------------------------------------------------
--  asf-models-selects -- Data model for UISelectOne and UISelectMany
--  Copyright (C) 2011, 2012, 2013, 2017, 2019 Stephane Carrez
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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
package body ASF.Models.Selects is

   function UTF8_Decode (S : in String) return Wide_Wide_String
      renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode;

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
   function Create_Select_Item (Label       : in String;
                                Value       : in String;
                                Description : in String := "";
                                Disabled    : in Boolean := False;
                                Escaped     : in Boolean := True) return Select_Item is
      Result : Select_Item;
   begin
      Result.Item := Select_Item_Refs.Create;
      declare
         Item : constant Select_Item_Record_Accessor := Result.Item.Value;
      begin
         Item.Label       := To_Unbounded_Wide_Wide_String (UTF8_Decode (Label));
         Item.Value       := To_Unbounded_Wide_Wide_String (UTF8_Decode (Value));
         Item.Description := To_Unbounded_Wide_Wide_String (UTF8_Decode (Description));
         Item.Disabled    := Disabled;
         Item.Escape      := Escaped;
      end;
      return Result;
   end Create_Select_Item;

   --  ------------------------------
   --  Creates a <b>Select_Item</b> with the specified label and value.
   --  ------------------------------
   function Create_Select_Item_Wide (Label       : in Wide_Wide_String;
                                     Value       : in Wide_Wide_String;
                                     Description : in Wide_Wide_String := "";
                                     Disabled    : in Boolean := False;
                                     Escaped     : in Boolean := True) return Select_Item is
      Result : Select_Item;
   begin
      Result.Item := Select_Item_Refs.Create;
      declare
         Item : constant Select_Item_Record_Accessor := Result.Item.Value;
      begin
         Item.Label       := To_Unbounded_Wide_Wide_String (Label);
         Item.Value       := To_Unbounded_Wide_Wide_String (Value);
         Item.Description := To_Unbounded_Wide_Wide_String (Description);
         Item.Disabled    := Disabled;
         Item.Escape      := Escaped;
      end;
      return Result;
   end Create_Select_Item_Wide;

   --  ------------------------------
   --  Creates a <b>Select_Item</b> with the specified label, value and description.
   --  The objects are converted to a wide wide string.  The empty string is used if they
   --  are null.
   --  ------------------------------
   function Create_Select_Item (Label       : in Util.Beans.Objects.Object;
                                Value       : in Util.Beans.Objects.Object;
                                Description : in Util.Beans.Objects.Object;
                                Disabled    : in Boolean := False;
                                Escaped     : in Boolean := True) return Select_Item is
      use Util.Beans.Objects;

      Result : Select_Item;
   begin
      Result.Item := Select_Item_Refs.Create;
      declare
         Item : constant Select_Item_Record_Accessor := Result.Item.Value;
      begin
         if not Is_Null (Label) then
            Item.Label       := To_Unbounded_Wide_Wide_String (Label);
         end if;
         if not Is_Null (Value) then
            Item.Value       := To_Unbounded_Wide_Wide_String (Value);
         end if;
         if not Is_Null (Description) then
            Item.Description := To_Unbounded_Wide_Wide_String (Description);
         end if;
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
   --  Returns true if the label must be escaped using HTML escape rules.
   --  ------------------------------
   function Is_Escaped (Item : in Select_Item) return Boolean is
   begin
      if Item.Item.Is_Null then
         return False;
      else
         return Item.Item.Value.Escape;
      end if;
   end Is_Escaped;

   --  ------------------------------
   --  Returns true if the select item component is empty.
   --  ------------------------------
   function Is_Empty (Item : in Select_Item) return Boolean is
   begin
      return Item.Item.Is_Null;
   end Is_Empty;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Select_Item;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if From.Item.Is_Null then
         return Util.Beans.Objects.Null_Object;
      end if;
      declare
         Item : constant Select_Item_Record_Accessor := From.Item.Value;
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

   --  ------------------------------
   --  Return an Object from the select item list.
   --  Returns a NULL object if the list is empty.
   --  ------------------------------
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

   --  ------------------------------
   --  Return the <b>Select_Item_List</b> instance from a generic bean object.
   --  Returns an empty list if the object does not hold a <b>Select_Item_List</b>.
   --  ------------------------------
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
   --  Get the number of elements in the list.
   --  ------------------------------
   overriding
   function Get_Count (From : in Select_Item_List) return Natural is
   begin
      return From.Length;
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   overriding
   procedure Set_Row_Index (From  : in out Select_Item_List;
                            Index : in Natural) is
   begin
      From.Current := From.Get_Select_Item (Index);
      From.Row     := Util.Beans.Objects.To_Object (From.Current'Unchecked_Access,
                                                    Util.Beans.Objects.STATIC);
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   overriding
   function Get_Row (From  : in Select_Item_List) return Util.Beans.Objects.Object is
   begin
      return From.Row;
   end Get_Row;

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

   --  ------------------------------
   --  Get the select item from the list
   --  ------------------------------
   function Get_Select_Item (List : in Select_Item_List'Class;
                             Pos  : in Positive) return Select_Item is
   begin
      if List.List.Is_Null then
         raise Constraint_Error with "Select item list is empty";
      end if;
      return List.List.Value.List.Element (Pos);
   end Get_Select_Item;

   --  ------------------------------
   --  Add the item at the end of the list.
   --  ------------------------------
   procedure Append (List : in out Select_Item_List;
                     Item : in Select_Item'Class) is
   begin
      if List.List.Is_Null then
         List.List := Select_Item_Vector_Refs.Create;
      end if;
      List.List.Value.List.Append (Select_Item (Item));
   end Append;

   --  ------------------------------
   --  Add the item at the end of the list.  This is a shortcut for
   --  Append (Create_List_Item (Label, Value))
   --  ------------------------------
   procedure Append (List  : in out Select_Item_List;
                     Label : in String;
                     Value : in String) is
   begin
      List.Append (Create_Select_Item (Label, Value));
   end Append;

   --  ------------------------------
   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   --  ------------------------------
   overriding
   function Get_Value (From : in Select_Item_List;
                       Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (From, Name);
   begin
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

end ASF.Models.Selects;
