-----------------------------------------------------------------------
--  components -- Component tree
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

with EL.Contexts.Default;
with Ada.Unchecked_Deallocation;
package body ASF.Components is

   --  ------------------------------
   --  Get the parent component.
   --  Returns null if the node is the root element.
   --  ------------------------------
   function Get_Parent (UI : UIComponent) return UIComponent_Access is
   begin
      return UI.Parent;
   end Get_Parent;

   --  ------------------------------
   --  Return a client-side identifier for this component, generating
   --  one if necessary.
   --  ------------------------------
   function Get_Client_Id (UI : UIComponent) return Unbounded_String is
   begin
      return UI.Id;
   end Get_Client_Id;

   --  ------------------------------
   --  Get the list of children.
   --  ------------------------------
   function Get_Children (UI : UIComponent) return UIComponent_List is
      Result : UIComponent_List;
   begin
      Result.Child := UI.First_Child;
      return Result;
   end Get_Children;

   function Create_UIComponent (Parent : UIComponent_Access;
                                Tag    : access ASF.Views.Nodes.Tag_Node'Class)
                                return UIComponent_Access is
      Result : UIComponent_Access := new UIComponent;
   begin
      Append (Parent, Result, Tag);
      return Result;
   end Create_UIComponent;

   procedure Append (UI    : in UIComponent_Access;
                     Child : in UIComponent_Access;
                     Tag   : access ASF.Views.Nodes.Tag_Node'Class) is
   begin
      Child.Tag    := Tag;
      Child.Parent := UI;
      Child.Next   := null;
      if UI.Last_Child = null then
         UI.First_Child := Child;
      else
         UI.Last_Child.Next := Child;
      end if;
      UI.Last_Child := Child;
   end Append;

   --  ------------------------------
   --  Search for and return the {@link UIComponent} with an <code>id</code>
   --  that matches the specified search expression (if any), according to
   --  the algorithm described below.
   --  ------------------------------
   function Find (UI : UIComponent;
                  Name : String) return UIComponent_Access is
   begin
      return null;
   end Find;

   function Get_Context (UI : in UIComponent)
                         return ASF.Contexts.Faces.Faces_Context_Access is
   begin
      return ASF.Contexts.Faces.Current;
   end Get_Context;

   --  ------------------------------
   --  Check whether the component and its children must be rendered.
   --  ------------------------------
   function Is_Rendered (UI : UIComponent;
                         Context : Faces_Context'Class) return Boolean is
      Attr : constant EL.Objects.Object := UI.Get_Attribute (Context, "rendered");
   begin
      return EL.Objects.To_Boolean (Attr);
   end Is_Rendered;

   --  ------------------------------
   --  Set whether the component is rendered.
   --  ------------------------------
   procedure Set_Rendered (UI       : in out UIComponent;
                           Rendered : in Boolean) is
   begin
      null;
   end Set_Rendered;


   function Get_Attribute (UI      : UIComponent;
                           Context : Faces_Context'Class;
                           Name    : String) return EL.Objects.Object is
      N : Unbounded_String;
   begin
--        N := ASF.Views.Nodes.Get_Name (UI.Tag.all);
      if UI.Tag = null then
         return EL.Objects.Null_Object;
      end if;
--        Attr := ASF.Views.Nodes.Get_Attribute (UI.Tag.all, Name);
      raise Constraint_Error;
   end Get_Attribute;

   --  ------------------------------
   --  Get the attribute tag
   --  ------------------------------
   function Get_Attribute (UI      : UIComponent;
                           Name    : String)
                           return access ASF.Views.Nodes.Tag_Attribute is
   begin
      return null;
   end Get_Attribute;

   procedure Set_Attribute (UI    : in out UIComponent;
                            Name  : in String;
                            Value : in EL.Objects.Object) is
   begin
      null;
   end Set_Attribute;

   procedure Encode_Begin (UI      : in UIComponent;
                           Context : in out Faces_Context'Class) is
   begin
      null;
   end Encode_Begin;

   procedure Encode_Children (UI      : in UIComponent;
                              Context : in out Faces_Context'Class) is
      Child : UIComponent_Access := UI.First_Child;
   begin
      while Child /= null loop
         Child.Encode_All (Context);
         Child := Child.Next;
      end loop;
   end Encode_Children;

   procedure Encode_End (UI      : in UIComponent;
                         Context : in out Faces_Context'Class) is
   begin
      null;
   end Encode_End;

   procedure Encode_All (UI      : in UIComponent'Class;
                         Context : in out Faces_Context'Class) is
   begin
      UI.Encode_Begin (Context);
      UI.Encode_Children (Context);
      UI.Encode_End (Context);
   end Encode_All;

   function Get_Value (Attr : UIAttribute;
                       UI   : UIComponent'Class) return EL.Objects.Object is
   begin
      return EL.Objects.Null_Object;
--        if not Is_Null (Attr.Object) then
--           return Attr.Object;
--        else
--           return Attr.Definition.Binding.Get_Value (UI.Get_Context);
--        end if;
   end Get_Value;

   procedure Free_Component is
     new Ada.Unchecked_Deallocation (Object => UIComponent'Class,
                                     Name   => UIComponent_Access);

   --  ------------------------------
   --  Delete the component tree recursively.
   --  ------------------------------
   procedure Delete (UI : in out UIComponent_Access) is
   begin
      if UI /= null then
         declare
            C : UIComponent_Access := UI.First_Child;
         begin
            while C /= null loop
               UI.First_Child := C.Next;
               Delete (C);
               C := UI.First_Child;
            end loop;
         end;
         Free_Component (UI);
      end if;
   end Delete;

end ASF.Components;
