-----------------------------------------------------------------------
--  html-selects -- ASF HTML UISelectOne and UISelectMany components
--  Copyright (C) 2011, 2013, 2014 Stephane Carrez
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

with ASF.Components.Base;
with ASF.Components.Html.Forms;
with ASF.Models.Selects;

with Util.Beans.Objects;

--  The <b>Selects</b> package implements various components used in forms to select
--  one or many choices from a list.
--
--  See JSR 314 - JavaServer Faces Specification 4.1.14 UISelectItem
--  See JSR 314 - JavaServer Faces Specification 4.1.15 UISelectItems
--  See JSR 314 - JavaServer Faces Specification 4.1.16 UISelectMany
--  See JSR 314 - JavaServer Faces Specification 4.1.17 UISelectOne
package ASF.Components.Html.Selects is

   --  ------------------------------
   --  UISelectBoolean Component
   --  ------------------------------
   --  The <b>UISelectBoolean</b> is a component that represents a single boolean value.
   type UISelectBoolean is new Forms.UIInput with private;
   type UISelectBoolean_Access is access all UISelectBoolean'Class;

   --  Render the checkbox element.
   overriding
   procedure Render_Input (UI       : in UISelectBoolean;
                           Context  : in out Faces_Context'Class;
                           Write_Id : in Boolean := True);

   --  Convert the string into a value.  If a converter is specified on the component,
   --  use it to convert the value.  Make sure the result is a boolean.
   overriding
   function Convert_Value (UI      : in UISelectBoolean;
                           Value   : in String;
                           Context : in Faces_Context'Class) return EL.Objects.Object;

   --  ------------------------------
   --  UISelectItem Component
   --  ------------------------------
   --  The <b>UISelectItem</b> is a component that may be nested inside a <b>UISelectMany</b>
   --  or <b>UISelectOne</b> component and represents exactly one <b>Select_Item</b>.
   type UISelectItem is new Core.UILeaf with private;
   type UISelectItem_Access is access all UISelectItem'Class;

   --  Get the <b>Select_Item</b> represented by the component.
   function Get_Select_Item (From    : in UISelectItem;
                             Context : in Faces_Context'Class)
                             return ASF.Models.Selects.Select_Item;

   --  ------------------------------
   --  UISelectItems Component
   --  ------------------------------
   --  The <b>UISelectItems</b> is a component that may be nested inside a <b>UISelectMany</b>
   --  or <b>UISelectOne</b> component and represents zero or more <b>Select_Item</b>.
   type UISelectItems is new Core.UILeaf with private;
   type UISelectItems_Access is access all UISelectItems'Class;

   --  Get the <b>Select_Item</b> represented by the component.
   function Get_Select_Item_List (From    : in UISelectItems;
                                  Context : in Faces_Context'Class)
                                  return ASF.Models.Selects.Select_Item_List;

   --  ------------------------------
   --  SelectOne Component
   --  ------------------------------
   --  The <b>UISelectOne</b> is a component that represents zero or one selection from
   --  a list of available options.
   type UISelectOne is new Forms.UIInput with private;
   type UISelectOne_Access is access all UISelectOne'Class;

   --  Render the <b>select</b> element.
   overriding
   procedure Encode_Begin (UI      : in UISelectOne;
                           Context : in out Faces_Context'Class);

   --  Renders the <b>select</b> element.  This is called by <b>Encode_Begin</b> if
   --  the component is rendered.
   procedure Render_Select (UI      : in UISelectOne;
                            Context : in out Faces_Context'Class);

   --  Renders the <b>option</b> element.  This is called by <b>Render_Select</b> to
   --  generate the component options.
   procedure Render_Options (UI      : in UISelectOne;
                             Value   : in Util.Beans.Objects.Object;
                             Context : in out Faces_Context'Class);

   --  ------------------------------
   --  SelectOneRadio Component
   --  ------------------------------
   --  The <b>UISelectOneRadio</b> is a component that represents zero or one selection from
   --  a list of available options.
   type UISelectOneRadio is new UISelectOne with private;
   type UISelectOneRadio_Access is access all UISelectOneRadio'Class;

   --  Returns True if the radio options must be rendered vertically.
   function Is_Vertical (UI      : in UISelectOneRadio;
                         Context : in Faces_Context'Class) return Boolean;

   --  Renders the <b>select</b> element.  This is called by <b>Encode_Begin</b> if
   --  the component is rendered.
   overriding
   procedure Render_Select (UI      : in UISelectOneRadio;
                            Context : in out Faces_Context'Class);

   --  ------------------------------
   --  Iterator over the Select_Item elements
   --  ------------------------------
   type Cursor is limited private;

   --  Get an iterator to scan the component children.
   --  SCz 2011-11-07: due to a bug in GNAT, we cannot use a function for First because
   --  the inner member 'List' is not copied correctly if the 'Cursor' is a limited type.
   --  Fallback to the Ada95 way.
   procedure First (UI       : in UISelectOne'Class;
                    Context  : in Faces_Context'Class;
                    Iterator : out Cursor);

   --  Returns True if the iterator points to a valid child.
   function Has_Element (Pos : in Cursor) return Boolean;

   --  Get the child component pointed to by the iterator.
   function Element (Pos     : in Cursor;
                     Context : in Faces_Context'Class) return ASF.Models.Selects.Select_Item;

   --  Move to the next child.
   procedure Next (Pos     : in out Cursor;
                   Context : in Faces_Context'Class);

private

   type UISelectBoolean is new Forms.UIInput with null record;

   type Cursor is limited record
      List      : ASF.Models.Selects.Select_Item_List;
      Component : ASF.Components.Base.Cursor;
      Current   : ASF.Components.Base.UIComponent_Access := null;
      Pos       : Natural := 0;
      Last      : Natural := 0;
   end record;

   type UISelectItem is new Core.UILeaf with null record;

   type UISelectItems is new Core.UILeaf with null record;

   type UISelectOne is new Forms.UIInput with null record;

   type UISelectOneRadio is new UISelectOne with null record;

end ASF.Components.Html.Selects;
