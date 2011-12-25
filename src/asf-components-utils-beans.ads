-----------------------------------------------------------------------
--  components-utils-beans -- Bean component utility
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
