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

with Ada.Finalization;
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Refs;

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
   function Create_Select_Item (Label       : in Wide_Wide_String;
                                Value       : in String;
                                Description : in Wide_Wide_String := "";
                                Disabled    : in Boolean := False;
                                Escaped     : in Boolean := True) return Select_Item;

   --  Creates a <b>Select_Item</b> with the specified label and value.
   function Create_Select_Item (Label       : in Wide_Wide_String;
                                Value       : in Wide_Wide_String;
                                Description : in Wide_Wide_String;
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

   --  Returns true if the
   function Is_Escaped (Item : in Select_Item) return Boolean;

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
   type Select_Item_List is private;
   type Select_Item_List_Access is access all Select_Item_List;

   --  Return an Object from the select item list.
   --  Returns a NULL object if the list is empty.
   function To_Object (Item : in Select_Item_List) return Util.Beans.Objects.Object;

   --  Return the <b>Select_Item_List</b> instance from a generic bean object.
   --  Returns an empty list if the object does not hold a <b>Select_Item_List</b>.
   function To_Select_Item_List (Object : in Util.Beans.Objects.Object) return Select_Item_List;

   --  Get the number of items in the list.
   function Length (List : in Select_Item_List) return Natural;

   --  Add the item at the end of the list.
   procedure Append (List : in out Select_Item_List;
                     Item : in Select_Item'Class);

private

   use Ada.Strings.Wide_Wide_Unbounded;

   type Select_Item_Record is new Util.Refs.Ref_Entity with record
      Label       : Unbounded_Wide_Wide_String;
      Value       : Unbounded_Wide_Wide_String;
      Description : Unbounded_Wide_Wide_String;
      Disabled    : Boolean := False;
      Escape      : Boolean := False;
   end record;
   type Select_Item_Record_Access is access all Select_Item_Record;

   package Select_Item_Refs is
      new Util.Refs.References (Element_Type   => Select_Item_Record,
                                Element_Access => Select_Item_Record_Access);

   type Select_Item is new Util.Beans.Basic.Readonly_Bean with record
      Item : Select_Item_Refs.Ref;
   end record;

   package Select_Item_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Select_Item);

   type Select_Item_Vector is new Util.Refs.Ref_Entity with record
      List : Select_Item_Vectors.Vector;
   end record;
   type Select_Item_Vector_Access is access all Select_Item_Vector;

   package Select_Item_Vector_Refs is
      new Util.Refs.References (Element_Type   => Select_Item_Vector,
                                Element_Access => Select_Item_Vector_Access);

   type Select_Item_List is new Util.Beans.Basic.Readonly_Bean with record
      List : Select_Item_Vector_Refs.Ref;
   end record;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Select_Item_List;
                       Name : in String) return Util.Beans.Objects.Object;

end ASF.Models.Selects;
