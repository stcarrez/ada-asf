-----------------------------------------------------------------------
--  asf-components-html -- ASF HTML Components
--  Copyright (C) 2009 - 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Contexts.Writer;
with ASF.Contexts.Faces;
with ASF.Components.Core;
with Util.Strings;

--  = HTML Components =
--  The `html` components provide the HTML components.
--
--  ```
--  xmlns:h="http://java.sun.com/jsf/html"
--  ```
--
--  @include-doc docs/comp-jsf-html/*.txt
package ASF.Components.Html is

   use ASF.Contexts.Writer;
   use ASF.Contexts.Faces;

   type UIHtmlComponent is new ASF.Components.Core.UIComponentBase with private;

   procedure Render_Attributes (UI      : in UIHtmlComponent;
                                Context : in out Faces_Context'Class;
                                Writer  : in Response_Writer_Access);

   --  Render the attributes which are defined on the component and which are
   --  in the list specified by <b>names</b>.
   procedure Render_Attributes (UI       : in UIHtmlComponent;
                                Context  : in out Faces_Context'Class;
                                Names    : in Util.Strings.String_Set.Set;
                                Writer   : in Response_Writer_Access;
                                Write_Id : in Boolean := True);

private

   type UIHtmlComponent is new ASF.Components.Core.UIComponentBase with record
      Value1 : EL.Objects.Object;
   end record;

end ASF.Components.Html;
