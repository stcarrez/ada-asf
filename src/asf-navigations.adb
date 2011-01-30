-----------------------------------------------------------------------
--  asf-navigations -- Navigations
--  Copyright (C) 2010, 2011 Stephane Carrez
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
with ASF.Components.Root;
with Util.Strings;
with Util.Beans.Objects;
package body ASF.Navigations is

   --  ------------------------------
   --  Navigation Case
   --  ------------------------------

   --  ------------------------------
   --  Check if the navigator specific condition matches the current execution context.
   --  ------------------------------
   function Matches (Navigator : in Navigation_Case;
                     Action    : in String;
                     Outcome   : in String;
                     Context   : in ASF.Contexts.Faces.Faces_Context'Class) return Boolean is
   begin
      --  outcome must match
      if Navigator.Outcome /= null and then Navigator.Outcome.all /= Outcome then
         return False;
      end if;

      --  action must match
      if Navigator.Action /= null and then Navigator.Action.all /= Action then
         return False;
      end if;

      --  condition must be true
      if not Navigator.Condition.Is_Constant then
         declare
            Value : constant Util.Beans.Objects.Object
              := Navigator.Condition.Get_Value (Context.Get_ELContext.all);
         begin
            if not Util.Beans.Objects.To_Boolean (Value) then
               return False;
            end if;
         end;
      end if;

      return True;
   end Matches;

   --  ------------------------------
   --  Navigation Rule
   --  ------------------------------

   --  ------------------------------
   --  Search for the navigator that matches the current action, outcome and context.
   --  Returns the navigator or null if there was no match.
   --  ------------------------------
   function Find_Navigation (Controller : in Rule;
                             Action     : in String;
                             Outcome    : in String;
                             Context    : in Contexts.Faces.Faces_Context'Class)
                             return Navigation_Access is
      Iter      : Navigator_Vector.Cursor := Controller.Navigators.First;
      Navigator : Navigation_Access;
   begin
      while Navigator_Vector.Has_Element (Iter) loop
         Navigator := Navigator_Vector.Element (Iter);

         --  Check if this navigator matches the action/outcome.
         if Navigator.Matches (Action, Outcome, Context) then
            return Navigator;
         end if;
         Navigator_Vector.Next (Iter);
      end loop;
      return null;
   end Find_Navigation;

   --  ------------------------------
   --  Navigation Handler
   --  ------------------------------

   --  ------------------------------
   --  After executing an action and getting the action outcome, proceed to the navigation
   --  to the next page.
   --  ------------------------------
   procedure Handle_Navigation (Handler : in Navigation_Handler;
                                Action  : in String;
                                Outcome : in String;
                                Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Nav_Rules : constant Navigation_Rules_Access := Handler.Rules;
      View      : constant Components.Root.UIViewRoot := Context.Get_View_Root;
      Name      : constant String := Components.Root.Get_View_Id (View);

      function Find_Navigation (View : in String) return Navigation_Access is
         Pos : constant Rule_Map.Cursor := Nav_Rules.Rules.Find (To_Unbounded_String (View));
      begin
         if not Rule_Map.Has_Element (Pos) then
            return null;
         end if;

         return Rule_Map.Element (Pos).Find_Navigation (Action, Outcome, Context);
      end Find_Navigation;

      Navigator : Navigation_Access;
   begin
      --  Find an exact match
      Navigator := Find_Navigation (Name);

      --  Find a wildcard match
      if Navigator /= null then
         declare
            Last : Natural := Name'Last;
            N    : Natural;
         begin
            loop
               N := Util.Strings.Rindex (Name, '/', Last);
               exit when N = 0;

               Navigator := Find_Navigation (Name (Name'First .. N) & "*");
               exit when Navigator /= null or N = Name'First;
               Last := Name'First - 1;
            end loop;
         end;
      end if;

      --  Execute the navigation action.
      if Navigator /= null then
         Navigator.Navigate (Context);
      end if;
   end Handle_Navigation;

   --  ------------------------------
   --  Free the storage used by the navigation handler.
   --  ------------------------------
   overriding
   procedure Finalize (Handler : in out Navigation_Handler) is
   begin
      null;
   end Finalize;

end ASF.Navigations;
