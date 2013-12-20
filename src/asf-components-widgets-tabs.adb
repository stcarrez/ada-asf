-----------------------------------------------------------------------
--  components-widgets-tabs -- Tab views and tabs
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
with ASF.Contexts.Writer;
package body ASF.Components.Widgets.Tabs is

   --  ------------------------------
   --  Render the tab start.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UITab;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if UI.Is_Rendered (Context) then
         Writer.Start_Element ("div");
         Writer.Write_Attribute ("id", UI.Get_Client_Id);
      end if;
   end Encode_Begin;

   --  ------------------------------
   --  Render the tab close.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UITab;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if UI.Is_Rendered (Context) then
         Writer.End_Element ("div");
      end if;
   end Encode_End;

   --  ------------------------------
   --  Render the tab list and prepare to render the tab contents.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UITabView;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is

      procedure Render_Tab (T : in Components.Base.UIComponent_Access);

      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;

      procedure Render_Tab (T : in Components.Base.UIComponent_Access) is
         Id : constant Unbounded_String := T.Get_Client_Id;
      begin
         if T.all in UITab'Class then
            Writer.Start_Element ("li");
            Writer.Start_Element ("a");
            Writer.Write_Attribute ("href", "#" & To_String (Id));
            Writer.Write_Text (T.Get_Attribute ("title", Context));
            Writer.End_Element ("a");
            Writer.End_Element ("li");
         end if;
      end Render_Tab;

      procedure Render_Tabs is
        new ASF.Components.Base.Iterate (Process => Render_Tab);

   begin
      if UI.Is_Rendered (Context) then
         declare
            use Util.Beans.Objects;

            Id       : constant Unbounded_String := UI.Get_Client_Id;
            Effect   : constant Object := UI.Get_Attribute (Context, Name => EFFECT_ATTR_NAME);
            Collapse : constant Boolean := UI.Get_Attribute (COLLAPSIBLE_ATTR_NAME, Context);
         begin
            Writer.Start_Element ("div");
            Writer.Write_Attribute ("id", Id);
            Writer.Start_Element ("ul");
            Render_Tabs (UI);
            Writer.End_Element ("ul");
            Writer.Queue_Script ("$(""#");
            Writer.Queue_Script (Id);
            Writer.Queue_Script (""").tabs({");
            if Collapse then
               Writer.Queue_Script ("collapsible: true");
            end if;
            if not Is_Empty (Effect) then
               if Collapse then
                  Writer.Queue_Script (",");
               end if;
               Writer.Queue_Script ("show:{effect:""");
               Writer.Queue_Script (Effect);
               Writer.Queue_Script (""",duration:");
               Writer.Queue_Script (UI.Get_Attribute (DURATION_ATTR_NAME, Context, "500"));
               Writer.Queue_Script ("}");
            end if;
            Writer.Queue_Script ("});");
         end;
      end if;
   end Encode_Begin;

   --  ------------------------------
   --  Render the tab view close.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UITabView;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if UI.Is_Rendered (Context) then
         Writer.End_Element ("div");
      end if;
   end Encode_End;

   --  ------------------------------
   --  Render the accordion list and prepare to render the tab contents.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UIAccordion;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is

      procedure Render_Tab (T : in Components.Base.UIComponent_Access);

      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;

      procedure Render_Tab (T : in Components.Base.UIComponent_Access) is
      begin
         if T.all in UITab'Class then
            Writer.Start_Element ("h3");
            Writer.Write_Text (T.Get_Attribute ("title", Context));
            Writer.End_Element ("h3");

            T.Encode_All (Context);
         end if;
      end Render_Tab;

      procedure Render_Tabs is
        new ASF.Components.Base.Iterate (Process => Render_Tab);
   begin
      if UI.Is_Rendered (Context) then
         declare
            use Util.Beans.Objects;

            Id       : constant Unbounded_String := UI.Get_Client_Id;
            Effect   : constant Object := UI.Get_Attribute (Context, Name => EFFECT_ATTR_NAME);
            Collapse : constant Boolean := UI.Get_Attribute (COLLAPSIBLE_ATTR_NAME, Context);
         begin
            Writer.Start_Element ("div");
            Writer.Write_Attribute ("id", Id);
            Render_Tabs (UI);
            Writer.End_Element ("div");
            Writer.Queue_Script ("$(""#");
            Writer.Queue_Script (Id);
            Writer.Queue_Script (""").accordion({");
            if Collapse then
               Writer.Queue_Script ("collapsible: true");
            end if;
            if not Is_Empty (Effect) then
               if Collapse then
                  Writer.Queue_Script (",");
               end if;
               Writer.Queue_Script ("show:{effect:""");
               Writer.Queue_Script (Effect);
               Writer.Queue_Script (""",duration:");
               Writer.Queue_Script (UI.Get_Attribute (DURATION_ATTR_NAME, Context, "500"));
               Writer.Queue_Script ("}");
            end if;
            Writer.Queue_Script ("});");
         end;
      end if;
   end Encode_Children;

end ASF.Components.Widgets.Tabs;
