-----------------------------------------------------------------------
--  components-widgets-progress -- Simple progress bar
--  Copyright (C) 2021 Stephane Carrez
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

with ASF.Components.Html;
with ASF.Contexts.Faces;
package ASF.Components.Widgets.Progress is

   VALUE_ATTR_NAME      : constant String := "value";
   MIN_VALUE_ATTR_NAME  : constant String := "minValue";
   MAX_VALUE_ATTR_NAME  : constant String := "maxValue";
   DIRECTION_ATTR_NAME  : constant String := "direction";

   type Progress_Type is delta 0.1 digits 4 range 0.0 .. 100.0;

   --  ------------------------------
   --  UIProgressBar
   --  ------------------------------
   --  The <b>UIProgressBar</b> component displays a simple progress bar component.
   type UIProgressBar is new ASF.Components.Html.UIHtmlComponent with null record;

   --  Get the progress value
   function Get_Progress (UI      : in UIProgressBar;
                          Context : in ASF.Contexts.Faces.Faces_Context'Class)
                          return Progress_Type;

   --  Render the tab start.
   overriding
   procedure Encode_Begin (UI      : in UIProgressBar;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the tab close.
   overriding
   procedure Encode_End (UI      : in UIProgressBar;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Components.Widgets.Progress;
