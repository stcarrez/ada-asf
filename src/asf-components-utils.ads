-----------------------------------------------------------------------
--  asf-components-utils -- ASF Util Components
--  Copyright (C) 2009 - 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Beans.Basic;

with ASF.Components.Base;
with ASF.Contexts.Faces;

--  = Util Components =
--  The `util` components are specific to Ada Server Faces and they are provided to help in
--  writing server faces pages.  The component is defined in the following namespace:
--
--  ```
--  xmlns:util="http://code.google.com/p/ada-asf/util"
--  ```
--
--  @include-doc docs/comp-util/*.txt
package ASF.Components.Utils is

   --  Get the line information where the component is defined.
   function Get_Line_Info (UI : in Base.UIComponent'Class) return String;

   --  Get the component attribute that implements the <tt>List_Bean</tt> interface.
   --  Returns null if the attribute is not found or does not implement the interface.
   function Get_List_Bean (UI      : in Base.UIComponent'Class;
                           Name    : in String;
                           Context : in ASF.Contexts.Faces.Faces_Context'Class)
                           return Util.Beans.Basic.List_Bean_Access;

end ASF.Components.Utils;
