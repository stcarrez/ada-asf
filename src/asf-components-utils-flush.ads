-----------------------------------------------------------------------
--  components-utils-flush -- Flush javascript queue and response
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
