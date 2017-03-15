-----------------------------------------------------------------------
--  components-widgets-panels -- Collapsible panels
--  Copyright (C) 2013, 2017 Stephane Carrez
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

   procedure Render_Action_Icon (Writer  : in out ASF.Contexts.Writer.Response_Writer'Class;
                                 Name    : in String);

   procedure Render_Action_Icon (Writer  : in out ASF.Contexts.Writer.Response_Writer'Class;
                                 Name    : in String) is
   begin
      Writer.Start_Element ("a");
      Writer.Write_Attribute ("href", "#");
      Writer.Write_Attribute ("class", "ui-panel-icon ui-corner-all ui-state-default");
      Writer.Start_Element ("span");
      Writer.Write_Attribute ("class", Name);
      Writer.End_Element ("span");
      Writer.End_Element ("a");
   end Render_Action_Icon;

   --  ------------------------------
   --  Render the panel header.
   --  ------------------------------
   procedure Render_Header (UI      : in UIPanel;
                            Writer  : in out ASF.Contexts.Writer.Response_Writer'Class;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use type ASF.Components.Base.UIComponent_Access;

      Header       : Util.Beans.Objects.Object;
      Header_Facet : ASF.Components.Base.UIComponent_Access;
      Closable     : constant Boolean := UI.Get_Attribute (CLOSABLE_ATTR_NAME, Context);
      Toggleable   : constant Boolean := UI.Get_Attribute (TOGGLEABLE_ATTR_NAME, Context);
   begin
      Writer.Start_Element ("div");
      Writer.Write_Attribute ("class", "ui-panel-header ui-widget-header");
      Header := UI.Get_Attribute (Name => HEADER_ATTR_NAME, Context => Context);
      if not Util.Beans.Objects.Is_Empty (Header) then
         Writer.Start_Element ("span");
         Writer.Write_Text (Header);
         Writer.End_Element ("span");
      end if;

      --  If there is a header facet, render it now.
      Header_Facet := UI.Get_Facet (HEADER_FACET_NAME);
      if Header_Facet /= null then
         Header_Facet.Encode_All (Context);
      end if;

      if Closable then
         Render_Action_Icon (Writer, "ui-icon ui-icon-closethick");
      end if;

      if Toggleable then
         Render_Action_Icon (Writer, "ui-icon ui-icon-minusthick");
      end if;
      Writer.End_Element ("div");

      --  Write the javascript to support the close and toggle actions.
      if Closable or Toggleable then
         Writer.Queue_Script ("$(""#");
         Writer.Queue_Script (UI.Get_Client_Id);
         Writer.Queue_Script (""").panel();");
      end if;
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
         Writer.Write_Attribute ("id", UI.Get_Client_Id);
         declare
            use Util.Beans.Objects;

            Style   : constant Object := UI.Get_Attribute (Context, "style");
            Class   : constant Object := UI.Get_Attribute (Context, "styleClass");
         begin
            if not Util.Beans.Objects.Is_Null (Class) then
               Writer.Write_Attribute ("class", To_String (Class)
                                       & " ui-panel ui-widget ui-corner-all");
            else
               Writer.Write_Attribute ("class", "ui-panel ui-widget ui-corner-all");
            end if;
            if not Is_Null (Style) then
               Writer.Write_Attribute ("style", Style);
            end if;
         end;
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
