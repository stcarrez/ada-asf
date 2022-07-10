-----------------------------------------------------------------------
--  html.lists -- List of items
--  Copyright (C) 2009, 2010, 2013, 2014, 2022 Stephane Carrez
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
with Util.Log.Loggers;
with Util.Beans.Basic;
with ASF.Components.Base;
package body ASF.Components.Html.Lists is

   use type EL.Objects.Data_Type;

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Components.Html.Lists");

   function Get_Item_Layout (List_Layout : in String;
                             Item_Class  : in String) return String;

   --  ------------------------------
   --  Get the list layout to use.  The default is to use no layout or a div if some CSS style
   --  is applied on the list or some specific list ID must be generated.  Possible layout values
   --  include:
   --    "simple"        : the list is rendered as is or as a div with each children as is,
   --    "unorderedList" : the list is rendered as an HTML ul/li list,
   --    "orderedList"   : the list is rendered as an HTML ol/li list.
   --  ------------------------------
   function Get_Layout (UI      : in UIList;
                        Class   : in String;
                        Context : in Faces_Context'Class) return String is
      Value  : constant EL.Objects.Object := UI.Get_Attribute (Context, "layout");
      Layout : constant String := EL.Objects.To_String (Value);
   begin
      if Layout = "orderedList" or else Layout = "ordered" then
         return "ol";
      elsif Layout = "unorderedList" or else Layout = "unordered" then
         return "ul";
      elsif Class'Length > 0 or else not UI.Is_Generated_Id then
         return "div";
      else
         return "";
      end if;
   end Get_Layout;

   --  ------------------------------
   --  Get the item layout according to the list layout and the item class (if any).
   --  ------------------------------
   function Get_Item_Layout (List_Layout : in String;
                             Item_Class  : in String) return String is
   begin
      if List_Layout'Length = 2 then
         return "li";
      elsif Item_Class'Length > 0 then
         return "div";
      else
         return "";
      end if;
   end Get_Item_Layout;

   --  ------------------------------
   --  Get the value to write on the output.
   --  ------------------------------
   function Get_Value (UI : in UIList) return EL.Objects.Object is
   begin
      return UI.Get_Attribute (UI.Get_Context.all, "value");
   end Get_Value;

   --  ------------------------------
   --  Set the value to write on the output.
   --  ------------------------------
   procedure Set_Value (UI    : in out UIList;
                        Value : in EL.Objects.Object) is
   begin
      null;
   end Set_Value;

   --  ------------------------------
   --  Get the variable name
   --  ------------------------------
   function Get_Var (UI : in UIList) return String is
      Var : constant EL.Objects.Object := UI.Get_Attribute (UI.Get_Context.all, "var");
   begin
      return EL.Objects.To_String (Var);
   end Get_Var;

   --  ------------------------------
   --  Encode an item of the list with the given item layout and item class.
   --  ------------------------------
   procedure Encode_Item (UI          : in UIList;
                          Item_Layout : in String;
                          Item_Class  : in String;
                          Context     : in out Faces_Context'Class) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if Item_Layout'Length > 0 then
         Writer.Start_Element (Item_Layout);
      end if;
      if Item_Class'Length > 0 then
         Writer.Write_Attribute ("class", Item_Class);
      end if;
      Base.UIComponent (UI).Encode_Children (Context);
      if Item_Layout'Length > 0 then
         Writer.End_Element (Item_Layout);
      end if;
   end Encode_Item;

   overriding
   procedure Encode_Children (UI      : in UIList;
                              Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      declare
         Value      : EL.Objects.Object := Get_Value (UI);
         Kind       : constant EL.Objects.Data_Type := EL.Objects.Get_Type (Value);
         Name       : constant String := UI.Get_Var;
         Bean       : access Util.Beans.Basic.Readonly_Bean'Class;
         Is_Reverse : constant Boolean := UI.Get_Attribute ("reverse", Context, False);
         List       : Util.Beans.Basic.List_Bean_Access;
         Count      : Natural;
      begin
         --  Check that we have a List_Bean but do not complain if we have a null value.
         if Kind /= EL.Objects.TYPE_BEAN then
            if Kind /= EL.Objects.TYPE_NULL then
               ASF.Components.Base.Log_Error (UI, "Invalid list bean (found a {0})",
                                              EL.Objects.Get_Type_Name (Value));
            end if;
            return;
         end if;

         Bean := EL.Objects.To_Bean (Value);
         if Bean = null or else not (Bean.all in Util.Beans.Basic.List_Bean'Class) then
            ASF.Components.Base.Log_Error (UI, "Invalid list bean: "
                                           & "it does not implement 'List_Bean' interface");
            return;
         end if;

         List := Util.Beans.Basic.List_Bean'Class (Bean.all)'Unchecked_Access;
         Count := List.Get_Count;
         if Count /= 0 then
            declare
               Class       : constant String := UI.Get_Attribute (STYLE_CLASS_ATTR_NAME,
                                                                  Context, "");
               Item_Class  : constant String := UI.Get_Attribute (ITEM_STYLE_CLASS_ATTR_NAME,
                                                                  Context, "");
               Layout      : constant String := UI.Get_Layout (Class, Context);
               Writer      : constant Response_Writer_Access := Context.Get_Response_Writer;
               Item_Layout : constant String := Get_Item_Layout (Layout, Item_Class);
            begin
               if Layout'Length > 0 then
                  Writer.Start_Element (Layout);
               end if;
               if not UI.Is_Generated_Id then
                  Writer.Write_Attribute ("id", UI.Get_Client_Id);
               end if;
               if Class'Length > 0 then
                  Writer.Write_Attribute ("class", Class);
               end if;
               if Is_Reverse then
                  for I in reverse 1 .. Count loop
                     List.Set_Row_Index (I);
                     Value := List.Get_Row;

                     Context.Set_Attribute (Name, Value);
                     Log.Debug ("Set variable {0}", Name);
                     UI.Encode_Item (Item_Layout, Item_Class, Context);
                  end loop;
               else
                  for I in 1 .. Count loop
                     List.Set_Row_Index (I);
                     Value := List.Get_Row;

                     Context.Set_Attribute (Name, Value);
                     Log.Debug ("Set variable {0}", Name);
                     UI.Encode_Item (Item_Layout, Item_Class, Context);
                  end loop;
               end if;
               if Layout'Length > 0 then
                  Writer.End_Element (Layout);
               end if;
            end;
         end if;
      end;
   end Encode_Children;

end ASF.Components.Html.Lists;
