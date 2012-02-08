-----------------------------------------------------------------------
--  html-links -- ASF HTML Links Components
--  Copyright (C) 2009, 2010, 2012 Stephane Carrez
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
package ASF.Components.Html.Links is

   --  ------------------------------
   --  Output Link Component
   --  ------------------------------
   type UIOutputLink is new UIHtmlComponent with private;

   --  Get the link to be rendered in the <b>href</b> attribute.
   function Get_Link (UI      : in UIOutputLink;
                      Context : in Faces_Context'Class) return String;

   --  Get the value to write on the output.
   function Get_Value (UI    : in UIOutputLink) return EL.Objects.Object;

   --  Set the value to write on the output.
   procedure Set_Value (UI    : in out UIOutputLink;
                        Value : in EL.Objects.Object);

   --  Encode the begining of the link.
   procedure Encode_Begin (UI      : in UIOutputLink;
                           Context : in out Faces_Context'Class);

   --  Encode the end of the link.
   procedure Encode_End (UI      : in UIOutputLink;
                         Context : in out Faces_Context'Class);

private

   type UIOutputLink is new UIHtmlComponent with record
      Value : EL.Objects.Object;
   end record;

end ASF.Components.Html.Links;
