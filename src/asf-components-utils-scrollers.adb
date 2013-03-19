-----------------------------------------------------------------------
--  components-utils-scrollers -- Data scrollers
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
with ASF.Contexts.Faces;
with ASF.Contexts.Writer;
with ASF.Components.Html;
package body ASF.Components.Utils.Scrollers is

   --  ------------------------------
   --  UIScroller
   --  ------------------------------

   --  Encode the data scroller.
   overriding
   procedure Encode_Children (UI      : in UIScroller;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      null;
   end Encode_Children;

end ASF.Components.Utils.Scrollers;
