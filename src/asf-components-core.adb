-----------------------------------------------------------------------
--  components-core -- ASF Core Components
--  Copyright (C) 2009, 2010, 2011, 2012, 2018, 2022 Stephane Carrez
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
package body ASF.Components.Core is

   use EL.Objects;

   --  ------------------------------
   --  Return a client-side identifier for this component, generating
   --  one if necessary.
   --  ------------------------------
   overriding
   function Get_Client_Id (UI : UIComponentBase) return Unbounded_String is
      Id : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute ("id");
   begin
      if Id /= null then
         return To_Unbounded_String (Views.Nodes.Get_Value (Id.all, UI.Get_Context.all));
      end if;
      --        return UI.Id;
      return Base.UIComponent (UI).Get_Client_Id;
   end Get_Client_Id;

   --  ------------------------------
   --  Renders the UIText evaluating the EL expressions it may contain.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIText;
                           Context : in out Faces_Context'Class) is
   begin
      UI.Text.Encode_All (UI.Expr_Table, Context);
   end Encode_Begin;

   --  ------------------------------
   --  Set the expression array that contains reduced expressions.
   --  ------------------------------
   procedure Set_Expression_Table (UI         : in out UIText;
                                   Expr_Table : in Views.Nodes.Expression_Access_Array_Access) is
      use type ASF.Views.Nodes.Expression_Access_Array_Access;
   begin
      if UI.Expr_Table /= null then
         UI.Log_Error ("Expression table already initialized");
         raise Program_Error with "Expression table already initialized";
      end if;
      UI.Expr_Table := Expr_Table;
   end Set_Expression_Table;

   --  ------------------------------
   --  Finalize the object.
   --  ------------------------------
   overriding
   procedure Finalize (UI : in out UIText) is
      use type ASF.Views.Nodes.Expression_Access_Array_Access;

      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Expressions.Expression'Class,
                                        EL.Expressions.Expression_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (ASF.Views.Nodes.Expression_Access_Array,
                                        ASF.Views.Nodes.Expression_Access_Array_Access);
   begin
      if UI.Expr_Table /= null then
         for I in UI.Expr_Table'Range loop
            Free (UI.Expr_Table (I));
         end loop;
         Free (UI.Expr_Table);
      end if;
      Base.UIComponent (UI).Finalize;
   end Finalize;

   function Create_UIText (Tag : ASF.Views.Nodes.Text_Tag_Node_Access)
                           return UIText_Access is
      Result : constant UIText_Access := new UIText;
   begin
      Result.Text := Tag;
      return Result;
   end Create_UIText;

   --  ------------------------------
   --  Get the parameter name
   --  ------------------------------
   function Get_Name (UI      : UIParameter;
                      Context : Faces_Context'Class) return String is
      Name : constant EL.Objects.Object := UI.Get_Attribute (Name    => "name",
                                                             Context => Context);
   begin
      return EL.Objects.To_String (Name);
   end Get_Name;

   --  ------------------------------
   --  Get the parameter value
   --  ------------------------------
   function Get_Value (UI      : UIParameter;
                       Context : Faces_Context'Class) return EL.Objects.Object is
   begin
      return UI.Get_Attribute (Name => "value", Context => Context);
   end Get_Value;

   --  ------------------------------
   --  Get the list of parameters associated with a component.
   --  ------------------------------
   function Get_Parameters (UI : Base.UIComponent'Class) return UIParameter_Access_Array is

      Result : UIParameter_Access_Array (1 .. UI.Get_Children_Count);
      Last   : Natural := 0;

      procedure Collect (Child : in Base.UIComponent_Access);
      pragma Inline (Collect);

      procedure Collect (Child : in Base.UIComponent_Access) is
      begin
         if Child.all in UIParameter'Class then
            Last := Last + 1;
            Result (Last) :=  UIParameter (Child.all)'Access;
         end if;
      end Collect;

      procedure Iter is new ASF.Components.Base.Iterate (Process => Collect);
      pragma Inline (Iter);

   begin
      Iter (UI);
      return Result (1 .. Last);
   end Get_Parameters;

end ASF.Components.Core;
