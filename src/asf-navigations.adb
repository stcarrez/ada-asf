-----------------------------------------------------------------------
--  asf-navigations -- Navigations
--  Copyright (C) 2010, 2011, 2012, 2013, 2018, 2021, 2022 Stephane Carrez
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
with Util.Log.Loggers;

with Ada.Unchecked_Deallocation;
with ASF.Navigations.Render;
package body ASF.Navigations is

   --  ------------------------------
   --  Navigation Case
   --  ------------------------------

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Navigations");

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
   --  Clear the navigation rules.
   --  ------------------------------
   procedure Clear (Controller : in out Rule) is
      procedure Free is new Ada.Unchecked_Deallocation (Navigation_Case'Class,
                                                        Navigation_Access);
   begin
      while not Controller.Navigators.Is_Empty loop
         declare
            Iter      : Navigator_Vector.Cursor := Controller.Navigators.Last;
            Navigator : Navigation_Access := Navigator_Vector.Element (Iter);
         begin
            Free (Navigator.Outcome);
            Free (Navigator.Action);
            Free (Navigator);
            Controller.Navigators.Delete (Iter);
         end;
      end loop;
   end Clear;

   --  ------------------------------
   --  Clear the navigation rules.
   --  ------------------------------
   procedure Clear (Controller : in out Navigation_Rules) is
      procedure Free is new Ada.Unchecked_Deallocation (Rule'Class, Rule_Access);
   begin
      while not Controller.Rules.Is_Empty loop
         declare
            Iter : Rule_Map.Cursor := Controller.Rules.First;
            Rule : Rule_Access := Rule_Map.Element (Iter);
         begin
            Rule.Clear;
            Free (Rule);
            Controller.Rules.Delete (Iter);
         end;
      end loop;
   end Clear;

   --  ------------------------------
   --  Navigation Handler
   --  ------------------------------

   --  ------------------------------
   --  Provide a default navigation rules for the view and the outcome when no application
   --  navigation was found.  The default looks for an XHTML file in the same directory as
   --  the view and which has the base name defined by <b>Outcome</b>.
   --  ------------------------------
   procedure Handle_Default_Navigation (Handler : in Navigation_Handler;
                                        View    : in String;
                                        Outcome : in String;
                                        Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Pos          : constant Natural := Util.Strings.Rindex (View, '/');
      Root         : Components.Root.UIViewRoot;
   begin
      if Pos > 0 then
         declare
            Name : constant String := View (View'First .. Pos) & Outcome;
         begin
            Log.Debug ("Using default navigation from view {0} to {1}", View, Name);

            Handler.View_Handler.Create_View (Name, Context, Root, Ignore => True);
         end;
      else
         Log.Debug ("Using default navigation from view {0} to {1}", View, View);

         Handler.View_Handler.Create_View (Outcome, Context, Root, Ignore => True);
      end if;

      --  If the 'outcome' refers to a real view, use it.  Otherwise keep the current view.
      if Components.Root.Get_Root (Root) /= null then
         Context.Set_View_Root (Root);
      end if;

      exception
         when others =>
         Log.Debug ("No suitable navigation rule to navigate from view {0}: {1}",
                    View, Outcome);
         raise;

   end Handle_Default_Navigation;

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

      function Find_Navigation (View : in String) return Navigation_Access;

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
      Log.Info ("Navigate from view {0} and action {1} with outcome {2}", Name, Action, Outcome);

      --  Find an exact match
      Navigator := Find_Navigation (Name);

      --  Find a wildcard match
      if Navigator = null then
         declare
            Last : Natural := Name'Last;
            N    : Natural;
         begin
            loop
               N := Util.Strings.Rindex (Name, '/', Last);
               exit when N = 0;

               Navigator := Find_Navigation (Name (Name'First .. N) & "*");
               exit when Navigator /= null or else N = Name'First;
               Last := N - 1;
            end loop;
         end;
      end if;

      --  Execute the navigation action.
      if Navigator /= null then
         Navigator.Navigate (Context);
      else
         Log.Debug ("No navigation rule found for view {0}, action {1} and outcome {2}",
                    Name, Action, Outcome);

         Navigation_Handler'Class (Handler).Handle_Default_Navigation (Name, Outcome, Context);
      end if;
   end Handle_Navigation;

   --  ------------------------------
   --  Initialize the the lifecycle handler.
   --  ------------------------------
   procedure Initialize (Handler : in out Navigation_Handler;
                         Views      : ASF.Applications.Views.View_Handler_Access) is
   begin
      Handler.Rules := new Navigation_Rules;
      Handler.View_Handler := Views;
   end Initialize;

   --  ------------------------------
   --  Free the storage used by the navigation handler.
   --  ------------------------------
   overriding
   procedure Finalize (Handler : in out Navigation_Handler) is
      procedure Free is new Ada.Unchecked_Deallocation (Navigation_Rules, Navigation_Rules_Access);
   begin
      if Handler.Rules /= null then
         Clear (Handler.Rules.all);
         Free (Handler.Rules);
      end if;
   end Finalize;

   --  ------------------------------
   --  Add a navigation case to navigate from the view identifier by <b>From</b>
   --  to the result view identified by <b>To</b>.  Some optional conditions are evaluated
   --  The <b>Outcome</b> must match unless it is empty.
   --  The <b>Action</b> must match unless it is empty.
   --  The <b>Condition</b> expression must evaluate to True.
   --  ------------------------------
   procedure Add_Navigation_Case (Handler   : in out Navigation_Handler;
                                  From      : in String;
                                  To        : in String;
                                  Outcome   : in String := "";
                                  Action    : in String := "";
                                  Condition : in String := "";
                                  Context   : in EL.Contexts.ELContext'Class) is
      C : constant Navigation_Access := Render.Create_Render_Navigator (To, 0);
   begin
      Handler.Add_Navigation_Case (C, From, Outcome, Action, Condition, Context);
   end Add_Navigation_Case;

   --  ------------------------------
   --  Add a navigation case to navigate from the view identifier by <b>From</b>
   --  by using the navigation rule defined by <b>Navigator</b>.
   --  Some optional conditions are evaluated:
   --  The <b>Outcome</b> must match unless it is empty.
   --  The <b>Action</b> must match unless it is empty.
   --  The <b>Condition</b> expression must evaluate to True.
   --  ------------------------------
   procedure Add_Navigation_Case (Handler   : in out Navigation_Handler'Class;
                                  Navigator : in Navigation_Access;
                                  From      : in String;
                                  Outcome   : in String := "";
                                  Action    : in String := "";
                                  Condition : in String := "";
                                  Context   : in EL.Contexts.ELContext'Class) is
   begin
      Log.Info ("Add navigation from {0} with outcome {1}", From, Outcome);

      if Outcome'Length > 0 then
         Navigator.Outcome := new String '(Outcome);
      end if;
      if Action'Length > 0 then
         Navigator.Action := new String '(Action);
      end if;

      --  if Handler.View_Handler = null then
      --   Handler.View_Handler := Handler.Application.Get_View_Handler;
      --  end if;

      if Condition'Length > 0 then
         Navigator.Condition := EL.Expressions.Create_Expression (Condition, Context);
      end if;

      Navigator.View_Handler := Handler.View_Handler;
      declare
         View : constant Unbounded_String := To_Unbounded_String (From);
         Pos  : constant Rule_Map.Cursor := Handler.Rules.Rules.Find (View);
         R    : Rule_Access;
      begin
         if not Rule_Map.Has_Element (Pos) then
            R := new Rule;
            Handler.Rules.Rules.Include (Key      => View,
                                         New_Item => R);
         else
            R := Rule_Map.Element (Pos);
         end if;

         R.Navigators.Append (Navigator);
      end;
   end Add_Navigation_Case;

end ASF.Navigations;
