-----------------------------------------------------------------------
--  components-widgets-tabs -- Tab views, tabs and accordion
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Components.Html;
with ASF.Contexts.Faces;
package ASF.Components.Widgets.Tabs is

   COLLAPSIBLE_ATTR_NAME   : constant String := "collapsible";
   EFFECT_ATTR_NAME        : constant String := "effect";
   DURATION_ATTR_NAME      : constant String := "duration";

   --  ------------------------------
   --  UITab
   --  ------------------------------
   --  The <b>UITab</b> component displays a tab component within a tab view.
   type UITab is new ASF.Components.Html.UIHtmlComponent with null record;

   --  Render the tab start.
   overriding
   procedure Encode_Begin (UI      : in UITab;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the tab close.
   overriding
   procedure Encode_End (UI      : in UITab;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  ------------------------------
   --  UITabView
   --  ------------------------------
   --  The <b>UITabView</b> component displays a tab selection panel.
   type UITabView is new ASF.Components.Html.UIHtmlComponent with null record;

   --  Render the tab list and prepare to render the tab contents.
   overriding
   procedure Encode_Begin (UI      : in UITabView;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the tab view close.
   overriding
   procedure Encode_End (UI      : in UITabView;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  ------------------------------
   --  UIAccordion
   --  ------------------------------
   --  The <b>UIAccordion</b> component displays a tab selection panel.
   type UIAccordion is new ASF.Components.Html.UIHtmlComponent with null record;

   --  Render the accordion list and prepare to render the tab contents.
   overriding
   procedure Encode_Children (UI      : in UIAccordion;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Components.Widgets.Tabs;
