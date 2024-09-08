-----------------------------------------------------------------------
--  components-utils-beans -- Bean component utility
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Components.Core;
with ASF.Contexts.Faces;
package ASF.Components.Utils.Beans is

   --  ------------------------------
   --  UISetBean
   --  ------------------------------
   --  The <b>UISetBean</b> component is a helper bean used to evaluate an EL expression
   --  and save the result in another bean or variable.
   type UISetBean is new ASF.Components.Core.UILeaf with null record;

   --  Evaluate the <b>value</b> attribute and set it in the value expression
   --  referred to by the <b>var</b> attribute.
   overriding
   procedure Encode_Begin (UI      : in UISetBean;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Components.Utils.Beans;
