-----------------------------------------------------------------------
--  components-widgets-progress -- Simple progress bar
--  Copyright (C) 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
