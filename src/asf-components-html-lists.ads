-----------------------------------------------------------------------
--  html.lists -- List of items
--  Copyright (C) 2009, 2010, 2014, 2022 Stephane Carrez
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
package ASF.Components.Html.Lists is

   --  The attribute that defines the layout of the list.
   LAYOUT_ATTR_NAME : constant String := "layout";

   --  The attribute that defines the CSS style to apply on the list.
   STYLE_CLASS_ATTR_NAME  : constant String := "styleClass";

   --  The attribute representing the CSS to be applied on each item of the list.
   ITEM_STYLE_CLASS_ATTR_NAME : constant String := "itemStyleClass";

   type UIList is new UIHtmlComponent with private;

   --  Get the list layout to use.  The default is to use no layout or a div if some CSS style
   --  is applied on the list or some specific list ID must be generated.  Possible layout values
   --  include:
   --    "simple"        : the list is rendered as is or as a div with each children as is,
   --    "unorderedList" : the list is rendered as an HTML ul/li list,
   --    "orderedList"   : the list is rendered as an HTML ol/li list.
   function Get_Layout (UI      : in UIList;
                        Class   : in String;
                        Context : in Faces_Context'Class) return String;

   --  Get the value to write on the output.
   function Get_Value (UI    : in UIList) return EL.Objects.Object;

   --  Set the value to write on the output.
   procedure Set_Value (UI    : in out UIList;
                        Value : in EL.Objects.Object);

   --  Get the variable name
   function Get_Var (UI : in UIList) return String;

   --  Encode an item of the list with the given item layout and item class.
   procedure Encode_Item (UI          : in UIList;
                          Item_Layout : in String;
                          Item_Class  : in String;
                          Context     : in out Faces_Context'Class);

   overriding
   procedure Encode_Children (UI      : in UIList;
                              Context : in out Faces_Context'Class);

private

   type UIList is new UIHtmlComponent with record
      Value : EL.Objects.Object;
   end record;

end ASF.Components.Html.Lists;
