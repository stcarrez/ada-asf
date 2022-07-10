-----------------------------------------------------------------------
--  html-selects -- ASF HTML UISelectOne and UISelectMany components
--  Copyright (C) 2011, 2013, 2014, 2015, 2022 Stephane Carrez
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

with Util.Strings;
with ASF.Utils;
package body ASF.Components.Html.Selects is

   --  ------------------------------
   --  UISelectItem Component
   --  ------------------------------

   ITEM_LABEL_NAME       : constant String := "itemLabel";
   ITEM_VALUE_NAME       : constant String := "itemValue";
   ITEM_DESCRIPTION_NAME : constant String := "itemDescription";
   ITEM_DISABLED_NAME    : constant String := "itemDisabled";

   SELECT_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   --  ------------------------------
   --  UISelectBoolean Component
   --  ------------------------------
   --  Render the checkbox element.
   overriding
   procedure Render_Input (UI       : in UISelectBoolean;
                           Context  : in out Faces_Context'Class;
                           Write_Id : in Boolean := True) is
      use ASF.Components.Html.Forms;

      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
      Value  : constant EL.Objects.Object := UIInput'Class (UI).Get_Value;
   begin
      Writer.Start_Element ("input");
      Writer.Write_Attribute (Name => "type", Value => "checkbox");
      UI.Render_Attributes (Context, SELECT_ATTRIBUTE_NAMES, Writer, Write_Id);
      Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
      if not EL.Objects.Is_Null (Value) and then EL.Objects.To_Boolean (Value) then
         Writer.Write_Attribute (Name => "checked", Value => "true");
      end if;
      Writer.End_Element ("input");
   end Render_Input;

   --  ------------------------------
   --  Convert the string into a value.  If a converter is specified on the component,
   --  use it to convert the value.  Make sure the result is a boolean.
   --  ------------------------------
   overriding
   function Convert_Value (UI      : in UISelectBoolean;
                           Value   : in String;
                           Context : in Faces_Context'Class) return EL.Objects.Object is
      use type EL.Objects.Data_Type;

      Result : constant EL.Objects.Object := Forms.UIInput (UI).Convert_Value (Value, Context);
   begin
      case EL.Objects.Get_Type (Result) is
         when EL.Objects.TYPE_BOOLEAN =>
            return Result;

         when EL.Objects.TYPE_INTEGER =>
            return EL.Objects.To_Object (EL.Objects.To_Boolean (Result));

         when others =>
            if Value = "on" then
               return EL.Objects.To_Object (True);
            else
               return EL.Objects.To_Object (False);
            end if;

      end case;
   end Convert_Value;

   --  ------------------------------
   --  Iterator over the Select_Item elements
   --  ------------------------------

   --  ------------------------------
   --  Get an iterator to scan the component children.
   --  ------------------------------
   procedure First (UI       : in UISelectOne'Class;
                    Context  : in Faces_Context'Class;
                    Iterator : out Cursor) is
   begin
      Iterator.Component := UI.First;
      Iterator.Pos       := 0;
      Iterator.Last      := 0;
      while ASF.Components.Base.Has_Element (Iterator.Component) loop
         Iterator.Current := ASF.Components.Base.Element (Iterator.Component);
         if Iterator.Current.all in UISelectItem'Class then
            return;
         end if;
         if Iterator.Current.all in UISelectItems'Class then
            Iterator.List := UISelectItems'Class (Iterator.Current.all)
              .Get_Select_Item_List (Context);
            Iterator.Last := Iterator.List.Length;
            Iterator.Pos  := 1;
            if Iterator.Last > 0 then
               return;
            end if;
         end if;
         ASF.Components.Base.Next (Iterator.Component);
      end loop;
      Iterator.Pos := 0;
      Iterator.Current := null;
   end First;

   --  ------------------------------
   --  Returns True if the iterator points to a valid child.
   --  ------------------------------
   function Has_Element (Pos : in Cursor) return Boolean is
      use type ASF.Components.Base.UIComponent_Access;
   begin
      if Pos.Pos > 0 and then Pos.Pos <= Pos.Last then
         return True;
      else
         return Pos.Current /= null;
      end if;
   end Has_Element;

   --  ------------------------------
   --  Get the child component pointed to by the iterator.
   --  ------------------------------
   function Element (Pos     : in Cursor;
                     Context : in Faces_Context'Class) return ASF.Models.Selects.Select_Item is
   begin
      if Pos.Pos > 0 and then Pos.Pos <= Pos.Last then
         return Pos.List.Get_Select_Item (Pos.Pos);
      else
         return UISelectItem'Class (Pos.Current.all).Get_Select_Item (Context);
      end if;
   end Element;

   --  ------------------------------
   --  Move to the next child.
   --  ------------------------------
   procedure Next (Pos     : in out Cursor;
                   Context : in Faces_Context'Class) is
   begin
      if Pos.Pos > 0 and then Pos.Pos < Pos.Last then
         Pos.Pos := Pos.Pos + 1;
      else
         Pos.Pos := 0;
         loop
            Pos.Current := null;
            ASF.Components.Base.Next (Pos.Component);
            exit when not ASF.Components.Base.Has_Element (Pos.Component);
            Pos.Current := ASF.Components.Base.Element (Pos.Component);
            exit when Pos.Current.all in UISelectItem'Class;
            if Pos.Current.all in UISelectItems'Class then
               Pos.List := UISelectItems'Class (Pos.Current.all).Get_Select_Item_List (Context);
               Pos.Last := Pos.List.Length;
               Pos.Pos  := 1;
               exit when Pos.Last > 0;
               Pos.Pos := 0;
            end if;
         end loop;
      end if;
   end Next;

   --  ------------------------------
   --  Get the <b>Select_Item</b> represented by the component.
   --  ------------------------------
   function Get_Select_Item (From    : in UISelectItem;
                             Context : in Faces_Context'Class)
                             return ASF.Models.Selects.Select_Item is
      use Util.Beans.Objects;

      Val : constant Object := From.Get_Attribute (Name    => VALUE_NAME,
                                                   Context => Context);
   begin
      if not Is_Null (Val) then
         return ASF.Models.Selects.To_Select_Item (Val);
      end if;
      declare
         Label       : constant Object := From.Get_Attribute (Name    => ITEM_LABEL_NAME,
                                                              Context => Context);
         Value       : constant Object := From.Get_Attribute (Name    => ITEM_VALUE_NAME,
                                                              Context => Context);
         Description : constant Object := From.Get_Attribute (Name    => ITEM_DESCRIPTION_NAME,
                                                              Context => Context);
         Disabled    : constant Boolean := From.Get_Attribute (Name   => ITEM_DISABLED_NAME,
                                                               Context => Context);
      begin
         if Is_Null (Label) then
            return ASF.Models.Selects.Create_Select_Item (Value, Value, Description, Disabled);
         else
            return ASF.Models.Selects.Create_Select_Item (Label, Value, Description, Disabled);
         end if;
      end;
   end Get_Select_Item;

   --  ------------------------------
   --  UISelectItems Component
   --  ------------------------------

   --  ------------------------------
   --  Get the <b>Select_Item</b> represented by the component.
   --  ------------------------------
   function Get_Select_Item_List (From : in UISelectItems;
                                  Context : in Faces_Context'Class)
                                     return ASF.Models.Selects.Select_Item_List is
      use Util.Beans.Objects;

      Value : constant Object := From.Get_Attribute (Name    => VALUE_NAME,
                                                     Context => Context);
   begin
      return ASF.Models.Selects.To_Select_Item_List (Value);
   end Get_Select_Item_List;

   --  ------------------------------
   --  SelectOne Component
   --  ------------------------------

   --  ------------------------------
   --  Render the <b>select</b> element.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UISelectOne;
                           Context : in out Faces_Context'Class) is
   begin
      if UI.Is_Rendered (Context) then
         UISelectOne'Class (UI).Render_Select (Context);
      end if;
   end Encode_Begin;

   --  ------------------------------
   --  Renders the <b>select</b> element.  This is called by <b>Encode_Begin</b> if
   --  the component is rendered.
   --  ------------------------------
   procedure Render_Select (UI      : in UISelectOne;
                            Context : in out Faces_Context'Class) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
      Value  : constant EL.Objects.Object := UISelectOne'Class (UI).Get_Value;
   begin
      Writer.Start_Element ("select");
      Writer.Write_Attribute (Name => "name", Value => UI.Get_Client_Id);
      UI.Render_Attributes (Context, SELECT_ATTRIBUTE_NAMES, Writer);

      UISelectOne'Class (UI).Render_Options (Value, Context);
      Writer.End_Element ("select");
   end Render_Select;

   --  ------------------------------
   --  Renders the <b>option</b> element.  This is called by <b>Render_Select</b> to
   --  generate the component options.
   --  ------------------------------
   procedure Render_Options (UI      : in UISelectOne;
                             Value   : in Util.Beans.Objects.Object;
                             Context : in out Faces_Context'Class) is
      Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
      Selected : constant Wide_Wide_String := Util.Beans.Objects.To_Wide_Wide_String (Value);
      Iter     : Cursor;
   begin
      UI.First (Context, Iter);
      while Has_Element (Iter) loop
         declare
            Item       : constant ASF.Models.Selects.Select_Item := Element (Iter, Context);
            Item_Value : constant Wide_Wide_String := Item.Get_Value;
         begin
            Writer.Start_Element ("option");
            Writer.Write_Wide_Attribute ("value", Item_Value);
            if Item_Value = Selected then
               Writer.Write_Attribute ("selected", "selected");
            end if;
            if Item.Is_Escaped then
               Writer.Write_Wide_Text (Item.Get_Label);
            else
               Writer.Write_Wide_Text (Item.Get_Label);
            end if;
            Writer.End_Element ("option");
            Next (Iter, Context);
         end;
      end loop;
   end Render_Options;

   --  ------------------------------
   --  Returns True if the radio options must be rendered vertically.
   --  ------------------------------
   function Is_Vertical (UI      : in UISelectOneRadio;
                         Context : in Faces_Context'Class) return Boolean is
      Dir    : constant String := UI.Get_Attribute (Context => Context,
                                                    Name    => "layout",
                                                    Default => "");
   begin
      return Dir = "pageDirection";
   end Is_Vertical;

   --  ------------------------------
   --  Renders the <b>select</b> element.  This is called by <b>Encode_Begin</b> if
   --  the component is rendered.
   --  ------------------------------
   overriding
   procedure Render_Select (UI      : in UISelectOneRadio;
                            Context : in out Faces_Context'Class) is
      Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
      Value    : constant EL.Objects.Object := UISelectOne'Class (UI).Get_Value;
      Vertical : constant Boolean := UI.Is_Vertical (Context);
      Selected : constant Wide_Wide_String := Util.Beans.Objects.To_Wide_Wide_String (Value);
      Iter     : Cursor;
      Id       : constant String := To_String (UI.Get_Client_Id);
      N        : Natural := 0;

      Disabled_Class : constant EL.Objects.Object := UI.Get_Attribute (Context => Context,
                                                                       Name => "disabledClass");
      Enabled_Class  : constant EL.Objects.Object := UI.Get_Attribute (Context => Context,
                                                                       Name => "enabledClass");
   begin
      Writer.Start_Element ("table");
      UI.Render_Attributes (Context, Writer);

      if not Vertical then
         Writer.Start_Element ("tr");
      end if;

      UI.First (Context, Iter);
      while Has_Element (Iter) loop
         declare
            Item       : constant ASF.Models.Selects.Select_Item := Element (Iter, Context);
            Item_Value : constant Wide_Wide_String := Item.Get_Value;
         begin
            if Vertical then
               Writer.Start_Element ("tr");
            end if;
            Writer.Start_Element ("td");

            --  Render the input radio checkbox.
            Writer.Start_Element ("input");
            Writer.Write_Attribute ("type", "radio");
            Writer.Write_Attribute ("name", Id);
            if Item.Is_Disabled then
               Writer.Write_Attribute ("disabled", "disabled");
            end if;
            Writer.Write_Attribute ("id", Id & "_" & Util.Strings.Image (N));
            Writer.Write_Wide_Attribute ("value", Item_Value);
            if Item_Value = Selected then
               Writer.Write_Attribute ("checked", "checked");
            end if;
            Writer.End_Element ("input");

            --  Render the label associated with the checkbox.
            Writer.Start_Element ("label");
            if Item.Is_Disabled then
               if not Util.Beans.Objects.Is_Null (Disabled_Class) then
                  Writer.Write_Attribute ("class", Disabled_Class);
               end if;
            else
               if not Util.Beans.Objects.Is_Null (Enabled_Class) then
                  Writer.Write_Attribute ("class", Enabled_Class);
               end if;
            end if;

            Writer.Write_Attribute ("for", Id & "_" & Util.Strings.Image (N));
            if Item.Is_Escaped then
               Writer.Write_Wide_Text (Item.Get_Label);
            else
               Writer.Write_Wide_Text (Item.Get_Label);
            end if;
            Writer.End_Element ("label");

            Writer.End_Element ("td");
            if Vertical then
               Writer.End_Element ("tr");
            end if;

            Next (Iter, Context);
            N := N + 1;
         end;
      end loop;

      if not Vertical then
         Writer.End_Element ("tr");
      end if;
      Writer.End_Element ("table");
   end Render_Select;

begin
   ASF.Utils.Set_Text_Attributes (SELECT_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Interactive_Attributes (SELECT_ATTRIBUTE_NAMES);
end ASF.Components.Html.Selects;
