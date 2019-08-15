-----------------------------------------------------------------------
--  asf-models-selects -- Data model for UISelectOne and UISelectMany
--  Copyright (C) 2011, 2012, 2013, 2019 Stephane Carrez
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

with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;
private with Util.Refs;

package ASF.Models.Selects is

   --  ------------------------------
   --  Select Item
   --  ------------------------------
   --  The <b>Select_Item</b> type describes a single option of a list of options
   --  used by the <b>UISelectOne</b> or <b>UISelectMany</b> components.
   --  The select item contains:
   --  <ul>
   --    <li>A label
   --    <li>A value
   --    <li>A description
   --    <li>Whether the select item is disabled or not
   --    <li>Whether the label is escaped or not
   --  </ul>
   --  An application creates the <b>Select_Item</b> instances and passes them
   --  to the ASF components through an <b>Util.Beans.Objects.Object</b> value.
   type Select_Item is new Util.Beans.Basic.Readonly_Bean with private;
   type Select_Item_Access is access all Select_Item;

   --  Return an Object from the select item record.
   --  Returns a NULL object if the item is empty.
   function To_Object (Item : in Select_Item) return Util.Beans.Objects.Object;

   --  Return the <b>Select_Item</b> instance from a generic bean object.
   --  Returns an empty item if the object does not hold a <b>Select_Item</b>.
   function To_Select_Item (Object : in Util.Beans.Objects.Object) return Select_Item;

   --  Creates a <b>Select_Item</b> with the specified label and value.
   function Create_Select_Item (Label       : in String;
                                Value       : in String;
                                Description : in String := "";
                                Disabled    : in Boolean := False;
                                Escaped     : in Boolean := True) return Select_Item;

   --  Creates a <b>Select_Item</b> with the specified label and value.
   function Create_Select_Item_Wide (Label       : in Wide_Wide_String;
                                     Value       : in Wide_Wide_String;
                                     Description : in Wide_Wide_String := "";
                                     Disabled    : in Boolean := False;
                                     Escaped     : in Boolean := True) return Select_Item;

   --  Creates a <b>Select_Item</b> with the specified label, value and description.
   --  The objects are converted to a wide wide string.  The empty string is used if they
   --  are null.
   function Create_Select_Item (Label       : in Util.Beans.Objects.Object;
                                Value       : in Util.Beans.Objects.Object;
                                Description : in Util.Beans.Objects.Object;
                                Disabled    : in Boolean := False;
                                Escaped     : in Boolean := True) return Select_Item;

   --  Get the item label.
   function Get_Label (Item : in Select_Item) return Wide_Wide_String;

   --  Get the item value.
   function Get_Value (Item : in Select_Item) return Wide_Wide_String;

   --  Get the item description.
   function Get_Description (Item : in Select_Item) return Wide_Wide_String;

   --  Returns true if the item is disabled.
   function Is_Disabled (Item : in Select_Item) return Boolean;

   --  Returns true if the label must be escaped using HTML escape rules.
   function Is_Escaped (Item : in Select_Item) return Boolean;

   --  Returns true if the select item component is empty.
   function Is_Empty (Item : in Select_Item) return Boolean;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Select_Item;
                       Name : in String) return Util.Beans.Objects.Object;

   --  ------------------------------
   --  Select Item List
   --  ------------------------------
   --  The <b>Select_Item_List</b> type holds a list of <b>Select_Item</b>.
   --  Similar to <b>Select_Item</b>, an application builds the list items and gives it
   --  to the ASF components through an <b>Util.Beans.Objects.Object</b> instance.
   type Select_Item_List is new Util.Beans.Basic.List_Bean with private;
   type Select_Item_List_Access is access all Select_Item_List;

   --  Return an Object from the select item list.
   --  Returns a NULL object if the list is empty.
   function To_Object (Item : in Select_Item_List) return Util.Beans.Objects.Object;

   --  Return the <b>Select_Item_List</b> instance from a generic bean object.
   --  Returns an empty list if the object does not hold a <b>Select_Item_List</b>.
   function To_Select_Item_List (Object : in Util.Beans.Objects.Object) return Select_Item_List;

   --  Get the number of elements in the list.
   overriding
   function Get_Count (From : in Select_Item_List) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   overriding
   procedure Set_Row_Index (From  : in out Select_Item_List;
                            Index : in Natural);

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : in Select_Item_List) return Util.Beans.Objects.Object;

   --  Get the number of items in the list.
   function Length (List : in Select_Item_List) return Natural;

   --  Get the select item from the list
   function Get_Select_Item (List : in Select_Item_List'Class;
                             Pos  : in Positive) return Select_Item;

   --  Add the item at the end of the list.
   procedure Append (List : in out Select_Item_List;
                     Item : in Select_Item'Class);

   --  Add the item at the end of the list.  This is a shortcut for
   --  Append (Create_List_Item (Label, Value))
   procedure Append (List  : in out Select_Item_List;
                     Label : in String;
                     Value : in String);

private

   use Ada.Strings.Wide_Wide_Unbounded;

   type Select_Item_Record is limited record
      Label       : Unbounded_Wide_Wide_String;
      Value       : Unbounded_Wide_Wide_String;
      Description : Unbounded_Wide_Wide_String;
      Disabled    : Boolean := False;
      Escape      : Boolean := False;
   end record;
   type Select_Item_Record_Access is access all Select_Item_Record;

   package Select_Item_Refs is
      new Util.Refs.General_References (Element_Type   => Select_Item_Record);
   subtype Select_Item_Record_Accessor is Select_Item_Refs.Element_Accessor;

   type Select_Item is new Util.Beans.Basic.Readonly_Bean with record
      Item : Select_Item_Refs.Ref;
   end record;

   package Select_Item_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Select_Item);

   type Select_Item_Vector is new Util.Refs.Ref_Entity with record
      List  : Select_Item_Vectors.Vector;
   end record;
   type Select_Item_Vector_Access is access all Select_Item_Vector;

   package Select_Item_Vector_Refs is
      new Util.Refs.References (Element_Type   => Select_Item_Vector,
                                Element_Access => Select_Item_Vector_Access);

   type Select_Item_List is new Util.Beans.Basic.List_Bean with record
      List    : Select_Item_Vector_Refs.Ref;
      Current : aliased Select_Item;
      Row     : Util.Beans.Objects.Object;
   end record;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Select_Item_List;
                       Name : in String) return Util.Beans.Objects.Object;

end ASF.Models.Selects;
