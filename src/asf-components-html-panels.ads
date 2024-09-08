-----------------------------------------------------------------------
--  html.panels -- Layout panels
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package ASF.Components.Html.Panels is

   type UIPanelGroup is new UIHtmlComponent with private;

   function Get_Layout (UI      : UIPanelGroup;
                        Context : in Faces_Context'Class) return String;

   overriding
   procedure Encode_Begin (UI      : in UIPanelGroup;
                           Context : in out Faces_Context'Class);

   overriding
   procedure Encode_End (UI      : in UIPanelGroup;
                         Context : in out Faces_Context'Class);

private

   type UIPanelGroup is new UIHtmlComponent with null record;

end ASF.Components.Html.Panels;
