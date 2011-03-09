-----------------------------------------------------------------------
--  html.panels -- Layout panels
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
package ASF.Components.Html.Panels is

   type UIPanelGroup is new UIHtmlComponent with private;

   function Get_Layout (UI      : UIPanelGroup;
                        Context : in Faces_Context'Class) return String;

   procedure Encode_Begin (UI      : in UIPanelGroup;
                           Context : in out Faces_Context'Class);

   procedure Encode_End (UI      : in UIPanelGroup;
                         Context : in out Faces_Context'Class);

private

   type UIPanelGroup is new UIHtmlComponent with null record;

end ASF.Components.Html.Panels;
