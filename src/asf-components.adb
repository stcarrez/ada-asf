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

with Ada.Unchecked_Deallocation;
with ASF.Views.Nodes;
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

   --  ------------------------------
   --  Get the number of children.
   --  ------------------------------
   function Get_Children_Count (UI : UIComponent) return Natural is
      Result : Natural := 0;
      Child  : UIComponent_Access := UI.First_Child;
   begin
      while Child /= null loop
         Result := Result + 1;
         Child := Child.Next;
      end loop;
      return Result;
   end Get_Children_Count;

   function Create_UIComponent (Parent : UIComponent_Access;
                                Tag    : access ASF.Views.Nodes.Tag_Node'Class)
                                return UIComponent_Access is
      Result : constant UIComponent_Access := new UIComponent;
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
      if EL.Objects.Is_Null (Attr) then
         return True;
      end if;
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
      pragma Unreferenced (Context);

      Attribute : UIAttribute_Access := UI.Attributes;
   begin
      while Attribute /= null loop
         declare
            Attr_Name : constant String := ASF.Views.Nodes.Get_Name (Attribute.Definition.all);
         begin
            if Attr_Name = Name then
               return Attribute.Value;
            end if;
         end;
         Attribute := Attribute.Next_Attr;
      end loop;
      declare
         Attr : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute (Name);
      begin
         if Attr = null then
            return EL.Objects.Null_Object;
         end if;
         return ASF.Views.Nodes.Get_Value (Attr.all, UI);
      end;
   end Get_Attribute;

   --  ------------------------------
   --  Get the attribute tag
   --  ------------------------------
   function Get_Attribute (UI      : UIComponent;
                           Name    : String)
                           return access ASF.Views.Nodes.Tag_Attribute is
   begin
      if UI.Tag = null then
         return null;
      else
         return UI.Tag.Get_Attribute (Name);
      end if;
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
      Child : UIComponent_Access;
   begin
      --  Do not render the children if the component is not rendered.
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      Child := UI.First_Child;
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

   --  ------------------------------
   --  Iterate over the children of the component and execute
   --  the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate (UI : in UIComponent'Class) is
      Child : UIComponent_Access := UI.First_Child;
   begin
      while Child /= null loop
         Process (Child);
         Child := Child.Next;
      end loop;
   end Iterate;

   --  ------------------------------
   --  Iterate over the attributes defined on the component and
   --  execute the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate_Attributes (UI : in UIComponent'Class) is
      Attribute : UIAttribute_Access := UI.Attributes;

      procedure Process_Tag_Attribute (Attr : in ASF.Views.Nodes.Tag_Attribute_Access) is
         A : UIAttribute;
      begin
         A.Definition := Attr;
         Process (ASF.Views.Nodes.Get_Name (Attr.all), A);
      end Process_Tag_Attribute;

      procedure Iterate_Tag_Attributes is new
        ASF.Views.Nodes.Iterate_Attributes (Process_Tag_Attribute);

   begin
      --  Iterate first over the component modified attributes.
      while Attribute /= null loop
         Process (ASF.Views.Nodes.Get_Name (Attribute.Definition.all), Attribute.all);
         Attribute := Attribute.Next_Attr;
      end loop;

      Iterate_Tag_Attributes (UI.Tag.all);
   end Iterate_Attributes;

   --  ------------------------------
   --  Get the attribute value.
   --  ------------------------------
   function Get_Value (Attr : UIAttribute;
                       UI   : UIComponent'Class) return EL.Objects.Object is
   begin
      if not EL.Objects.Is_Null (Attr.Value) then
         return Attr.Value;
      else
         return ASF.Views.Nodes.Get_Value (Attr.Definition.all, UI);
      end if;
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
