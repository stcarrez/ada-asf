-----------------------------------------------------------------------
--  asf-navigations-render -- Navigator to render a page
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
with Ada.Exceptions;
with Util.Log.Loggers;
with ASF.Components.Root;

package body ASF.Navigations.Render is

   use Ada.Exceptions;
   use Util.Log;
   use ASF.Applications;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Navigations.Render");

   --  ------------------------------
   --  Navigate to the next page or action according to the controller's navigator.
   --  A navigator controller could redirect the user to another page, render a specific
   --  view or return some raw content.
   --  ------------------------------
   overriding
   procedure Navigate (Controller : in Render_Navigator;
                       Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Name : constant String := To_String (Controller.View_Name);
      View : Components.Root.UIViewRoot;
   begin
      Log.Debug ("Navigate to view {0}", Name);

      Controller.View_Handler.Create_View (Name, Context, View);

      Context.Set_View_Root (View);

   exception
      when E : others =>
         Log.Error ("Error when navigating to view {0}: {1}: {2}", Name,
                    Exception_Name (E), Exception_Message (E));
         raise;

   end Navigate;

   --  ------------------------------
   --  Create a navigation case to render a view.
   --  ------------------------------
   function Create_Render_Navigator (To_View : in String) return Navigation_Access is
      Result : Render_Navigator_Access := new Render_Navigator;
   begin
      Result.View_Name := To_Unbounded_String (To_View);
      return Result.all'Access;
   end Create_Render_Navigator;

end ASF.Navigations.Render;
