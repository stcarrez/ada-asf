-----------------------------------------------------------------------
--  html-pages -- HTML Page Components
--  Copyright (C) 2011, 2014, 2019 Stephane Carrez
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

with Util.Strings;
with Util.Beans.Objects;

with ASF.Utils;
with ASF.Requests;

--  The <b>Pages</b> package implements various components used when building an HTML page.
--
package body ASF.Components.Html.Pages is

   BODY_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   HEAD_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   --  ------------------------------
   --  Head Component
   --  ------------------------------

   --  ------------------------------
   --  Encode the HTML head element.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIHead;
                           Context : in out Contexts.Faces.Faces_Context'Class) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
   begin
      Writer.Start_Element ("head");
      UI.Render_Attributes (Context, HEAD_ATTRIBUTE_NAMES, Writer);
   end Encode_Begin;

   --  ------------------------------
   --  Terminate the HTML head element.  Before closing the head, generate the resource
   --  links that have been queued for the head generation.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIHead;
                         Context : in out Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (UI);

      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
   begin
      Writer.Write_Scripts;
      Writer.End_Element ("head");
   end Encode_End;

   --  ------------------------------
   --  Encode the HTML body element.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIBody;
                           Context : in out Contexts.Faces.Faces_Context'Class) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
   begin
      Writer.Start_Element ("body");
      UI.Render_Attributes (Context, BODY_ATTRIBUTE_NAMES, Writer);
   end Encode_Begin;

   --  ------------------------------
   --  Terminate the HTML body element.  Before closing the body, generate the inclusion
   --  of differed resources (pending javascript, inclusion of javascript files)
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIBody;
                         Context : in out Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (UI);

      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
   begin
      Writer.Write_Scripts;
      Writer.End_Element ("body");
   end Encode_End;

   --  ------------------------------
   --  Get the link to be rendered in the <b>href</b> attribute.
   --  ------------------------------
   function Get_Link (UI      : in UIOutputStylesheet;
                      Context : in Faces_Context'Class) return String is
      Req   : constant ASF.Requests.Request_Access := Context.Get_Request;
      Name  : constant String := UI.Get_Attribute ("name", Context,  "");
      Lib   : constant String := UI.Get_Attribute ("library", Context,  "");
      Ctx   : constant String := Req.Get_Context_Path;
   begin
      if Lib'Length > 0 then
         return Ctx & "/resources/" & Lib & "/" & Name;
      else
         return Ctx & "/resources/" & Name;
      end if;
   end Get_Link;

   --  ------------------------------
   --  Terminate the HTML body element.  Before closing the body, generate the inclusion
   --  of differed resources (pending javascript, inclusion of javascript files)
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIOutputStylesheet;
                         Context : in out Contexts.Faces.Faces_Context'Class) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      Writer.Start_Element ("link");
      Writer.Write_Attribute ("type", "text/css");
      Writer.Write_Attribute ("rel", "stylesheet");
      Writer.Write_Attribute ("href", UI.Get_Link (Context));
      Writer.End_Element ("link");
   end Encode_End;

   --  ------------------------------
   --  Encode the DOCTYPE element.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIDoctype;
                           Context : in out Contexts.Faces.Faces_Context'Class) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if UI.Is_Rendered (Context) then
         Writer.Write ("<!DOCTYPE ");
         Writer.Write (UI.Get_Attribute ("rootElement", Context, ""));
         declare
            Public : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "public");
            System : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "system");
         begin
            if not Util.Beans.Objects.Is_Null (Public) then
               Writer.Write (" PUBLIC """);
               Writer.Write (Public);
               Writer.Write ('"');
            end if;
            if not Util.Beans.Objects.Is_Null (System) then
               Writer.Write (" """);
               Writer.Write (System);
               Writer.Write ('"');
            end if;
         end;
         Writer.Write ('>');
         Writer.Write (ASCII.LF);
      end if;
   end Encode_Begin;

begin
   Utils.Set_Head_Attributes (HEAD_ATTRIBUTE_NAMES);
   Utils.Set_Text_Attributes (BODY_ATTRIBUTE_NAMES);
   Utils.Set_Body_Attributes (BODY_ATTRIBUTE_NAMES);
end ASF.Components.Html.Pages;
