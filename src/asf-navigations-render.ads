-----------------------------------------------------------------------
--  asf-navigations-render -- Navigator to render a page
--  Copyright (C) 2010, 2011, 2018 Stephane Carrez
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
package ASF.Navigations.Render is

   --  ------------------------------
   --  Render page navigator
   --  ------------------------------
   --  The <b>Render_Navigator</b> defines the page that must be rendered for the response.
   type Render_Navigator (Len : Natural) is new Navigation_Case with private;
   type Render_Navigator_Access is access all Render_Navigator'Class;

   --  Navigate to the next page or action according to the controller's navigator.
   --  A navigator controller could redirect the user to another page, render a specific
   --  view or return some raw content.
   overriding
   procedure Navigate (Controller : in Render_Navigator;
                       Context    : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Create a navigation case to render a view.
   function Create_Render_Navigator (To_View : in String;
                                     Status  : in Natural) return Navigation_Access;

private

   type Render_Navigator (Len : Natural) is new Navigation_Case with record
      Status       : Natural := 0;
      View_Name    : String (1 .. Len);
   end record;

end ASF.Navigations.Render;
