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

--  The <b>Pages</b> package implements various components used when building an HTML page.
--
package ASF.Components.Html.Pages is

   --  ------------------------------
   --  Head Component
   --  ------------------------------
   type UIHead is new UIHtmlComponent with private;

   --  Encode the HTML head element.
   overriding
   procedure Encode_Begin (UI      : in UIHead;
                           Context : in out Contexts.Faces.Faces_Context'Class);

   --  Terminate the HTML head element.  Before closing the head, generate the resource
   --  links that have been queued for the head generation.
   overriding
   procedure Encode_End (UI      : in UIHead;
                         Context : in out Contexts.Faces.Faces_Context'Class);

   --  ------------------------------
   --  Body Component
   --  ------------------------------
   type UIBody is new UIHtmlComponent with private;

   --  Encode the HTML body element.
   overriding
   procedure Encode_Begin (UI      : in UIBody;
                           Context : in out Contexts.Faces.Faces_Context'Class);

   --  Terminate the HTML body element.  Before closing the body, generate the inclusion
   --  of differed resources (pending javascript, inclusion of javascript files)
   overriding
   procedure Encode_End (UI      : in UIBody;
                         Context : in out Contexts.Faces.Faces_Context'Class);

   --  ------------------------------
   --  Stylesheet Component
   --  ------------------------------
   type UIOutputStylesheet is new UIHtmlComponent with private;

   --  Encode the HTML link element.
   overriding
   procedure Encode_End (UI      : in UIOutputStylesheet;
                         Context : in out Contexts.Faces.Faces_Context'Class);

   function Get_Link (UI      : in UIOutputStylesheet;
                      Context : in Faces_Context'Class) return String;

   --  ------------------------------
   --  Doctype Component
   --  ------------------------------
   type UIDoctype is new UIHtmlComponent with private;

   --  Encode the DOCTYPE element.
   overriding
   procedure Encode_Begin (UI      : in UIDoctype;
                           Context : in out Contexts.Faces.Faces_Context'Class);

private

   type UIHead is new UIHtmlComponent with null record;

   type UIBody is new UIHtmlComponent with record
      Value     : EL.Objects.Object;
   end record;

   type UIOutputStylesheet is new UIHtmlComponent with null record;

   type UIDoctype is new UIHtmlComponent with null record;

end ASF.Components.Html.Pages;
