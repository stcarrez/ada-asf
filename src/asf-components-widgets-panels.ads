-----------------------------------------------------------------------
--  components-widgets-panels -- Collapsible panels
--  Copyright (C) 2013 Stephane Carrez
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
with ASF.Contexts.Writer;
package ASF.Components.Widgets.Panels is

   HEADER_FACET_NAME    : constant String := "header";
   FOOTER_FACET_NAME    : constant String := "footer";

   HEADER_ATTR_NAME     : constant String := "header";
   FOOTER_ATTR_NAME     : constant String := "footer";
   TOGGLEABLE_ATTR_NAME : constant String := "toggleable";
   CLOSABLE_ATTR_NAME   : constant String := "closable";

   --  ------------------------------
   --  UIPanel
   --  ------------------------------
   --  The <b>UIPanel</b> component displays a <tt>div</tt> panel with a header, a body
   --  and a footer.  The panel header can contain some actions to collapse or expand the
   --  panel content.
   type UIPanel is new ASF.Components.Html.UIHtmlComponent with null record;

   --  Render the panel header.
   procedure Render_Header (UI      : in UIPanel;
                            Writer  : in out ASF.Contexts.Writer.Response_Writer'Class;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the panel footer.
   procedure Render_Footer (UI      : in UIPanel;
                            Writer  : in out ASF.Contexts.Writer.Response_Writer'Class;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the panel header and prepare for the panel content.
   overriding
   procedure Encode_Begin (UI      : in UIPanel;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the panel footer.
   overriding
   procedure Encode_End (UI      : in UIPanel;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Components.Widgets.Panels;
