-----------------------------------------------------------------------
--  components-utils-scripts -- Javascript queue
--  Copyright (C) 2011, 2012, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
                            Content : in String;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  If the component provides a src attribute, render the <script src='xxx'></script>
   --  tag with an optional async attribute.
   overriding
   procedure Encode_Begin (UI      : in UIScript;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

private

   type UIScript is new ASF.Components.Utils.Escapes.UIEscape with null record;

end ASF.Components.Utils.Scripts;
