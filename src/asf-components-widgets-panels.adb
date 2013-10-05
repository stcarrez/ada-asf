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
with Util.Beans.Objects;
with ASF.Components.Base;
package body ASF.Components.Widgets.Panels is

   --  ------------------------------
   --  Render the panel header.
   --  ------------------------------
   procedure Render_Header (UI      : in UIPanel;
                            Writer  : in out ASF.Contexts.Writer.Response_Writer'Class;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use type ASF.Components.Base.UIComponent_Access;

      Header       : Util.Beans.Objects.Object;
      Header_Facet : ASF.Components.Base.UIComponent_Access;
   begin
      Writer.Start_Element ("div");
      Writer.Write_Attribute ("class", "ui-panel-header ui-widget-header");
      Header := UI.Get_Attribute (Name => HEADER_ATTR_NAME, Context => Context);
      if not Util.Beans.Objects.Is_Empty (Header) then
         Writer.Start_Element ("span");
         Writer.Write_Text (Header);
         Writer.End_Element ("span");
         Writer.End_Element ("div");
      end if;

      --  If there is a header facet, render it now.
      Header_Facet := UI.Get_Facet (HEADER_FACET_NAME);
      if Header_Facet /= null then
         Header_Facet.Encode_All (Context);
      end if;
      Writer.End_Element ("div");

      null;
   end Render_Header;

   --  ------------------------------
   --  Render the panel footer.
   --  ------------------------------
   procedure Render_Footer (UI      : in UIPanel;
                            Writer  : in out ASF.Contexts.Writer.Response_Writer'Class;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use type ASF.Components.Base.UIComponent_Access;

      Footer       : Util.Beans.Objects.Object;
      Footer_Facet : ASF.Components.Base.UIComponent_Access;
      Has_Footer   : Boolean;
   begin
      Footer_Facet := UI.Get_Facet (FOOTER_FACET_NAME);
      Footer := UI.Get_Attribute (Name => FOOTER_ATTR_NAME, Context => Context);

      Has_Footer := Footer_Facet /= null or else not Util.Beans.Objects.Is_Empty (Footer);
      if Has_Footer then
         Writer.Start_Element ("div");
         Writer.Write_Attribute ("class", "ui-panel-footer ui-widget-footer");
      end if;
      if not Util.Beans.Objects.Is_Empty (Footer) then
         Writer.Write_Text (Footer);
      end if;

      --  If there is a footer facet, render it now.
      if Footer_Facet /= null then
         Footer_Facet.Encode_All (Context);
      end if;
      if Has_Footer then
         Writer.End_Element ("div");
      end if;
   end Render_Footer;

   --  ------------------------------
   --  Render the panel header and prepare for the panel content.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIPanel;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if UI.Is_Rendered (Context) then
         Writer.Start_Element ("div");
         Writer.Write_Attribute ("class", "ui-panel ui-widget ui-corner-all");
         UIPanel'Class (UI).Render_Header (Writer.all, Context);
         Writer.Start_Element ("div");
         Writer.Write_Attribute ("class", "ui-panel-content ui-widget-content");
      end if;
   end Encode_Begin;

   --  ------------------------------
   --  Render the panel footer.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIPanel;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if UI.Is_Rendered (Context) then
         Writer.End_Element ("div");
         UIPanel'Class (UI).Render_Footer (Writer.all, Context);
         Writer.End_Element ("div");
      end if;
   end Encode_End;

end ASF.Components.Widgets.Panels;
