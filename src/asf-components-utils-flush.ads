-----------------------------------------------------------------------
--  components-utils-flush -- Flush javascript queue and response
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
with ASF.Components.Core;
with ASF.Contexts.Faces;
package ASF.Components.Utils.Flush is

   --  ------------------------------
   --  UIFlush
   --  ------------------------------
   --  The <b>UIFlush</b> component is intended to be used to flush the javascript
   --  queue at well defined positions in the HTML document.
   type UIFlush is new ASF.Components.Core.UILeaf with null record;

   --  Flush the javascript queue
   overriding
   procedure Encode_Begin (UI      : in UIFlush;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Components.Utils.Flush;
