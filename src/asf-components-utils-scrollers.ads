-----------------------------------------------------------------------
--  components-utils-scrollers -- Data scrollers
--  Copyright (C) 2013, 2015 Stephane Carrez
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
with ASF.Contexts.Faces;
with ASF.Components.Html;
with ASF.Components.Holders;
package ASF.Components.Utils.Scrollers is

   --  ------------------------------
   --  UIScroller
   --  ------------------------------
   --
   type UIScroller is new ASF.Components.Html.UIHtmlComponent with private;

   --  Get the list value holder that the scroller is controlling.
   function Get_List (UI      : in UIScroller;
                      Context : in ASF.Contexts.Faces.Faces_Context'Class)
                      return Holders.List_Holder_Access;

   procedure Render_Page (UI      : in UIScroller;
                          Name    : in String;
                          Page    : in Positive;
                          Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Encode the data scroller.
   overriding
   procedure Encode_Children (UI      : in UIScroller;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

private

   type UIScroller is new ASF.Components.Html.UIHtmlComponent with null record;

end ASF.Components.Utils.Scrollers;
