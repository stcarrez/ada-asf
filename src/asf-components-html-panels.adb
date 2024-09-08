-----------------------------------------------------------------------
--  html.panels -- Layout panels
--  Copyright (C) 2009, 2010, 2012, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Objects;
package body ASF.Components.Html.Panels is

   function Get_Layout (UI : UIPanelGroup;
                        Context : in Faces_Context'Class) return String is
      Value  : constant EL.Objects.Object := UI.Get_Attribute (Context, "layout");
      Layout : constant String := EL.Objects.To_String (Value);
   begin
      if Layout = "div" or else Layout = "block" then
         return "div";
      elsif Layout = "none" then
         return "";
      else
         return "span";
      end if;
   end Get_Layout;

   overriding
   procedure Encode_Begin (UI      : in UIPanelGroup;
                           Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
         Tag    : constant String := UI.Get_Layout (Context);
      begin
         if Tag'Length > 0 then
            if Tag = "span" then
               Writer.Start_Optional_Element (Tag);
            else
               Writer.Start_Element (Tag);
            end if;
            UI.Render_Attributes (Context, Writer);
         end if;
      end;
   end Encode_Begin;

   overriding
   procedure Encode_End (UI      : in UIPanelGroup;
                         Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
         Tag    : constant String := UI.Get_Layout (Context);
      begin
         if Tag'Length > 0 then
            if Tag = "span" then
               Writer.End_Optional_Element (Tag);
            else
               Writer.End_Element (Tag);
            end if;
         end if;
      end;
   end Encode_End;

end ASF.Components.Html.Panels;
