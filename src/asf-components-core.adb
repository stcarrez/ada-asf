-----------------------------------------------------------------------
--  components-core -- ASF Core Components
--  Copyright (C) 2009, 2010 Stephane Carrez
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

package body ASF.Components.Core is

   use ASF;
   use EL.Objects;

   --  ------------------------------
   --  Return a client-side identifier for this component, generating
   --  one if necessary.
   --  ------------------------------
   function Get_Client_Id (UI : UIComponentBase) return Unbounded_String is
      Id : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute ("id");
   begin
      if Id /= null then
         return To_Unbounded_String (Views.Nodes.Get_Value (Id.all, UI.Get_Context.all));
      end if;
      return UI.Id;
   end Get_Client_Id;

   --  ------------------------------
   --  Get the attribute tag
   --  ------------------------------
   overriding
   function Get_Attribute (UI      : UIComponentBase;
                           Name    : String)
                           return access ASF.Views.Nodes.Tag_Attribute is
   begin
      if UI.Tag = null then
         return null;
      else
         return UI.Tag.Get_Attribute (Name);
      end if;
   end Get_Attribute;

   overriding
   function Get_Attribute (UI      : UIComponentBase;
                           Context : Faces_Context'Class;
                           Name    : String) return EL.Objects.Object is
      Attr : access ASF.Views.Nodes.Tag_Attribute;
   begin
      if UI.Tag = null then
         return EL.Objects.Null_Object;
      end if;
      Attr := UI.Tag.Get_Attribute (Name);
      if Attr = null then
         return EL.Objects.Null_Object;
      end if;
      return ASF.Views.Nodes.Get_Value (Attr.all, Context);
   end Get_Attribute;

   procedure Encode_Begin (UI      : in UIText;
                           Context : in out Faces_Context'Class) is
   begin
      UI.Text.Encode_All (Context);
   end Encode_Begin;

   function Create_UIText (Tag : ASF.Views.Nodes.Text_Tag_Node_Access)
                           return UIComponent_Access is
      Result : constant UIText_Access := new UIText;
   begin
      Result.Text := Tag;
      return Result.all'Access;
   end Create_UIText;

   --  ------------------------------
   --  Get the root node of the view.
   --  ------------------------------
   function Get_Root (UI : UIViewRoot) return UIComponent_Access is
   begin
      return UI.Root;
   end Get_Root;

   --  ------------------------------
   --  Set the root node of the view.
   --  ------------------------------
   procedure Set_Root (UI   : in out UIViewRoot;
                       Root : in UIComponent_Access) is
   begin
      if UI.Root /= null then
         Delete (UI.Root);
      end if;
      UI.Root := Root;
   end Set_Root;

   procedure Finalize (Object : in out UIViewRoot) is
   begin
      if Object.Root /= null then
         Delete (Object.Root);
      end if;
   end Finalize;

   overriding
   procedure Encode_Begin (UI      : in UIView;
                           Context : in out Faces_Context'Class) is
   begin
      null;
   end Encode_Begin;

   --  ------------------------------
   --  Component Parameter
   --  ------------------------------

   --  ------------------------------
   --  Get the parameter name
   --  ------------------------------
   function Get_Name (UI      : UIParameter;
                      Context : Faces_Context'Class) return String is
   begin
      return EL.Objects.To_String (UI.Get_Attribute (Name => "name", Context => Context));
   end Get_Name;

   --  ------------------------------
   --  Get the parameter value
   --  ------------------------------
   function Get_Value (UI      : UIParameter;
                       Context : Faces_Context'Class) return EL.Objects.Object is
   begin
      return UI.Get_Attribute (Name => "value", Context => Context);
   end Get_Value;

   overriding
   procedure Encode_Children (UI      : in UIParameter;
                              Context : in out Faces_Context'Class) is
   begin
      null;
   end Encode_Children;

end ASF.Components.Core;
