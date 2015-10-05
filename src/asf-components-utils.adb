-----------------------------------------------------------------------
--  components-util -- ASF Util Components
--  Copyright (C) 2009, 2010, 2011, 2013, 2015 Stephane Carrez
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
with Util.Beans.Objects;
with ASF.Views;
with ASF.Views.Nodes;
package body ASF.Components.Utils is

   function Get_Line_Info (UI : in Base.UIComponent'Class) return Views.Line_Info;

   --  ------------------------------
   --  Get the line information where the component is defined.
   --  ------------------------------
   function Get_Line_Info (UI : in Base.UIComponent'Class) return Views.Line_Info is
      Tag : constant access ASF.Views.Nodes.Tag_Node'Class := UI.Get_Tag;
   begin
      if Tag /= null then
         return Tag.Get_Line_Info;
      else
         return Tag.Get_Line_Info;
      end if;
   end Get_Line_Info;
   pragma Unreferenced (Get_Line_Info);

   --  ------------------------------
   --  Get the line information where the component is defined.
   --  ------------------------------
   function Get_Line_Info (UI : in Base.UIComponent'Class) return String is
      Tag : constant access ASF.Views.Nodes.Tag_Node'Class := UI.Get_Tag;
   begin
      if Tag /= null then
         return Tag.Get_Line_Info;
      else
         return "?";
      end if;
   end Get_Line_Info;

   --  ------------------------------
   --  Get the component attribute that implements the <tt>List_Bean</tt> interface.
   --  Returns null if the attribute is not found or does not implement the interface.
   --  ------------------------------
   function Get_List_Bean (UI      : in Base.UIComponent'Class;
                           Name    : in String;
                           Context : in ASF.Contexts.Faces.Faces_Context'Class)
                           return Util.Beans.Basic.List_Bean_Access is
      use type Util.Beans.Objects.Data_Type;

      Value : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, Name);
      Kind  : constant Util.Beans.Objects.Data_Type := Util.Beans.Objects.Get_Type (Value);
      Bean  : access Util.Beans.Basic.Readonly_Bean'Class;
      List  : Util.Beans.Basic.List_Bean_Access;
   begin
      --  Check that we have a List_Bean but do not complain if we have a null value.
      if Kind /= Util.Beans.Objects.TYPE_BEAN then
         if Kind /= Util.Beans.Objects.TYPE_NULL then
            ASF.Components.Base.Log_Error (UI, "Invalid list bean (found a {0})",
                                           Util.Beans.Objects.Get_Type_Name (Value));
         end if;
         return null;
      end if;

      Bean := Util.Beans.Objects.To_Bean (Value);
      if Bean = null or else not (Bean.all in Util.Beans.Basic.List_Bean'Class) then
         ASF.Components.Base.Log_Error (UI, "Invalid list bean: it does not implement"
                                        & " 'List_Bean' interface");
         return null;
      end if;
      List := Util.Beans.Basic.List_Bean'Class (Bean.all)'Unchecked_Access;
      return List;
   end Get_List_Bean;

end ASF.Components.Utils;
