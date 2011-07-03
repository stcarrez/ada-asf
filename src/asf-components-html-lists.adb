-----------------------------------------------------------------------
--  html.lists -- List of items
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
with Util.Log.Loggers;
with Util.Beans.Basic;
with ASF.Components.Base;
package body ASF.Components.Html.Lists is

   use Util.Log;
   use type EL.Objects.Data_Type;

   Log : constant Loggers.Logger := Loggers.Create ("ASF.Components.Html.Lists");

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

   procedure Encode_Children (UI      : in UIList;
                              Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      declare
         Value : EL.Objects.Object := Get_Value (UI);
         Kind  : constant EL.Objects.Data_Type := EL.Objects.Get_Type (Value);
         Name  : constant String := UI.Get_Var;
         Bean  : access Util.Beans.Basic.Readonly_Bean'Class;
         Is_Reverse : constant Boolean := UI.Get_Attribute ("reverse", Context, False);
         List  : Util.Beans.Basic.List_Bean_Access;
         Count : Natural;
      begin
         --  Check that we have a List_Bean but do not complain if we have a null value.
         if Kind /= EL.Objects.TYPE_BEAN then
            if Kind /= EL.Objects.TYPE_NULL then
               Log.Error ("Invalid list bean (found a {0})", EL.Objects.Get_Type_Name (Value));
            end if;
            return;
         end if;

         Bean := EL.Objects.To_Bean (Value);
         if Bean = null or else not (Bean.all in Util.Beans.Basic.List_Bean'Class) then
            Log.Error ("Invalid list bean: it does not implement 'List_Bean' interface");
            return;
         end if;

         List := Util.Beans.Basic.List_Bean'Class (Bean.all)'Unchecked_Access;
         Count := List.Get_Count;
         if Is_Reverse then
            for I in reverse 1 .. Count loop
               List.Set_Row_Index (I);
               Value := List.Get_Row;

               Context.Set_Attribute (Name, Value);
               Log.Debug ("Set variable {0}", Name);
               Base.UIComponent (UI).Encode_Children (Context);
            end loop;
         else
            for I in 1 .. Count loop
               List.Set_Row_Index (I);
               Value := List.Get_Row;

               Context.Set_Attribute (Name, Value);
               Log.Debug ("Set variable {0}", Name);
               Base.UIComponent (UI).Encode_Children (Context);
            end loop;
         end if;
      end;
   end Encode_Children;

end ASF.Components.Html.Lists;
