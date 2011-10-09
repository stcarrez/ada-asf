-----------------------------------------------------------------------
--  components-utils-scripts -- Javascript queue
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
with ASF.Contexts.Faces;
with ASF.Contexts.Writer;
with ASF.Components.Utils.Escapes;
package ASF.Components.Utils.Scripts is

   --  ------------------------------
   --  UIScript
   --  ------------------------------
   --  The <b>UIScript</b> component allows to write Javascript code in the javascript
   --  queue in order to collect all the Javascript code of a page in a well defined order
   --  and in a single place.  The javascript code is queued and will be flushed either
   --  by the <b>UIFlush</b> component or before flushing the response.
   type UIScript is new ASF.Components.Utils.Escapes.UIEscape with private;

   --  Write the content that was collected by rendering the inner children.
   --  Write the content in the Javascript queue.
   overriding
   procedure Write_Content (UI      : in UIScript;
                            Writer  : in out Contexts.Writer.Response_Writer'Class;
                            Content : in Ada.Strings.Unbounded.Unbounded_String;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class);

private

   type UIScript is new ASF.Components.Utils.Escapes.UIEscape with null record;

end ASF.Components.Utils.Scripts;
