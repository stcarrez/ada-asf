-----------------------------------------------------------------------
--  components-widgets-selects -- Select component with jQuery Chosen
--  Copyright (C) 2015 Stephane Carrez
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

with ASF.Components.Html.Selects;
with ASF.Contexts.Faces;
package ASF.Components.Widgets.Selects is

   OPTIONS_FACET_NAME : constant String := "options";
   EVENTS_FACET_NAME  : constant String := "events";

   --  ------------------------------
   --  UIChosen
   --  ------------------------------
   --  The <b>UIChosen</b> component displays a HTML select component.
   type UIChosen is new ASF.Components.Html.Selects.UISelectOne with null record;

   --  Render the chosen configuration script.
   overriding
   procedure Encode_End (UI      : in UIChosen;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Components.Widgets.Selects;
