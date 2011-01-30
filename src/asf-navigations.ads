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

with ASF.Contexts.Faces;
with EL.Expressions;
with Ada.Finalization;
with ASF.Applications.Views;
private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded;
private with Ada.Strings.Unbounded.Hash;

--  The <b>ASF.Navigations</b> package is responsible for calculating the view to be
--  rendered after an action is processed.  The navigation handler contains some rules
--  (defined in XML files) and it uses the current view id, the action outcome and
--  the faces context to decide which view must be rendered.
--
--  See JSR 314 - JavaServer Faces Specification 7.4 NavigationHandler
package ASF.Navigations is

   --  ------------------------------
   --  Navigation Handler
   --  ------------------------------
   type Navigation_Handler is new Ada.Finalization.Limited_Controlled with private;

   --  After executing an action and getting the action outcome, proceed to the navigation
   --  to the next page.
   procedure Handle_Navigation (Handler : in Navigation_Handler;
                                Action  : in String;
                                Outcome : in String;
                                Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Free the storage used by the navigation handler.
   overriding
   procedure Finalize (Handler : in out Navigation_Handler);

   --  ------------------------------
   --  Navigation Case
   --  ------------------------------
   --  The <b>Navigation_Case</b> contains a condition and a navigation action.
   --  The condition must be matched to execute the navigation action.
   type Navigation_Case is abstract tagged limited private;
   type Navigation_Access is access all Navigation_Case'Class;

   --  Check if the navigator specific condition matches the current execution context.
   function Matches (Navigator : in Navigation_Case;
                     Action    : in String;
                     Outcome   : in String;
                     Context   : in ASF.Contexts.Faces.Faces_Context'Class) return Boolean;

   --  Navigate to the next page or action according to the controller's navigator.
   --  A navigator controller could redirect the user to another page, render a specific
   --  view or return some raw content.
   procedure Navigate (Navigator : in Navigation_Case;
                       Context   : in out ASF.Contexts.Faces.Faces_Context'Class) is abstract;


private

   use Ada.Strings.Unbounded;

   type Navigation_Case is abstract tagged limited record
      --  When not empty, the condition that must be verified to follow this navigator.
      Condition    : EL.Expressions.Expression;
      View_Handler : access ASF.Applications.Views.View_Handler'Class;
      Outcome      : String_Access;
      Action       : String_Access;
   end record;

   package Navigator_Vector is new Ada.Containers.Vectors (Index_Type   => Natural,
                                                           Element_Type => Navigation_Access);

   --  ------------------------------
   --  Navigation Rule
   --  ------------------------------
   type Rule is tagged limited record
      Navigators : Navigator_Vector.Vector;
   end record;

   --  Search for the navigator that matches the current action, outcome and context.
   --  Returns the navigator or null if there was no match.
   function Find_Navigation (Controller : in Rule;
                             Action     : in String;
                             Outcome    : in String;
                             Context    : in Contexts.Faces.Faces_Context'Class)
                             return Navigation_Access;

   type Rule_Access is access all Rule'Class;

   package Rule_Map is new Ada.Containers.Hashed_Maps (Key_Type     => Unbounded_String,
                                                       Element_Type => Rule_Access,
                                                       Hash         => Hash,
                                                       Equivalent_Keys => "=");

   type Navigation_Rules is limited record
      --  Exact match rules
      Rules : Rule_Map.Map;
   end record;

   type Navigation_Rules_Access is access all Navigation_Rules;

   type Navigation_Handler is new Ada.Finalization.Limited_Controlled with record
      Rules : Navigation_Rules_Access;
   end record;

end ASF.Navigations;
