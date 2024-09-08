-----------------------------------------------------------------------
--  components-widgets-selects -- Select component with jQuery Chosen
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
