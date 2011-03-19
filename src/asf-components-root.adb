-----------------------------------------------------------------------
--  components-root -- ASF Root View Component
--  Copyright (C) 2010 Stephane Carrez
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
with ASF.Components.Base;
package body ASF.Components.Root is

   use ASF;
   use EL.Objects;

   --  ------------------------------
   --  Get the root node of the view.
   --  ------------------------------
   function Get_Root (UI : in UIViewRoot) return access Base.UIComponent'Class is
   begin
      if UI.Root = null then
         return null;
      else
         return UI.Root.View;
      end if;
   end Get_Root;

   --  ------------------------------
   --  Set the root node of the view.
   --  ------------------------------
   procedure Set_Root (UI   : in out UIViewRoot;
                       Root : access Base.UIComponent'Class;
                       Name : in String) is
   begin
      if UI.Root /= null then
         Finalize (UI);
      end if;

      UI.Root := new Root_Holder '(Ref_Counter => 1,
                                   View        => Root,
                                   Name        => To_Unbounded_String (Name));
   end Set_Root;

   --  ------------------------------
   --  Get the view identifier.
   --  ------------------------------
   function Get_View_Id (UI : in UIViewRoot) return String is
   begin
      return To_String (UI.Root.Name);
   end Get_View_Id;

   --  ------------------------------
   --  Increment the reference counter.
   --  ------------------------------
   overriding
   procedure Adjust (Object : in out UIViewRoot) is
   begin
      if Object.Root /= null then
         Object.Root.Ref_Counter := Object.Root.Ref_Counter + 1;
      end if;
   end Adjust;

   Empty : ASF.Components.Base.UIComponent;

   --  ------------------------------
   --  Free the memory held by the component tree.
   --  ------------------------------
   overriding
   procedure Finalize (Object : in out UIViewRoot) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Root_Holder,
                                        Name   => Root_Holder_Access);

   begin
      if Object.Root /= null then
         Object.Root.Ref_Counter := Object.Root.Ref_Counter - 1;
         if Object.Root.Ref_Counter = 0 then
            declare
               C : ASF.Components.Base.UIComponent_Access := Object.Root.View.all'Unchecked_Access;
            begin
               ASF.Components.Base.Steal_Root_Component (Empty, C);
            end;
            Free (Object.Root);
         end if;
      end if;
   end Finalize;

end ASF.Components.Root;
