-----------------------------------------------------------------------
--  asf-navigations-redirect -- Navigator to redirect to another page
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
package ASF.Navigations.Redirect is

   --  ------------------------------
   --  Redirect page navigator
   --  ------------------------------
   --  The <b>Redirect_Navigator</b> handles the redirection rule for the navigation.
   type Redirect_Navigator is new Navigation_Case with private;
   type Redirect_Navigator_Access is access all Redirect_Navigator'Class;

   --  Navigate to the next page or action according to the controller's navigator.
   --  The navigator controller redirects the user to another page.
   overriding
   procedure Navigate (Controller : in Redirect_Navigator;
                       Context    : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Create a navigation case to redirect to another page.
   function Create_Redirect_Navigator (To_View : in String) return Navigation_Access;

private

   type Redirect_Navigator is new Navigation_Case with record
      View_Name    : Unbounded_String;
   end record;

end ASF.Navigations.Redirect;
