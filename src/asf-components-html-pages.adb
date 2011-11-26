-----------------------------------------------------------------------
--  html-pages -- HTML Page Components
--  Copyright (C) 2011 Stephane Carrez
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
with ASF.Utils;

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

begin
   Utils.Set_Head_Attributes (HEAD_ATTRIBUTE_NAMES);
   Utils.Set_Text_Attributes (BODY_ATTRIBUTE_NAMES);
   Utils.Set_Body_Attributes (BODY_ATTRIBUTE_NAMES);
end ASF.Components.Html.Pages;
