-----------------------------------------------------------------------
--  components-ajax-includes -- AJAX Include component
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Components.Html;
with ASF.Contexts.Faces;
package ASF.Components.Ajax.Includes is

   --  @attribute
   --  Defines whether the inclusion is asynchronous or not.
   --  When true, an AJAX call will be made by the client browser to fetch the content.
   --  When false, the view is included in the current component tree.
   ASYNC_ATTR_NAME  : constant String := "async";

   --  @attribute
   --  Defines the HTML container element that will contain the included view.
   LAYOUT_ATTR_NAME : constant String := "layout";

   --  @attribute
   --  Defines the view name to include.
   SRC_ATTR_NAME    : constant String := "src";

   --  @tag include
   --  The <b>ajax:include</b> component allows to include
   type UIInclude is new ASF.Components.Html.UIHtmlComponent with private;

   --  Get the HTML layout that must be used for the include container.
   --  The default layout is a "div".
   --  Returns "div", "span", "pre", "b".
   function Get_Layout (UI      : in UIInclude;
                        Context : in ASF.Contexts.Faces.Faces_Context'Class) return String;

   --  The included XHTML file is rendered according to the <b>async</b> attribute:
   --
   --  When <b>async</b> is false, render the specified XHTML file in such a way that inner
   --  forms will be posted on the included view.
   --
   --  When <b>async</b> is true, trigger an AJAX call to include the specified
   --  XHTML view when the page is loaded.
   --
   --
   overriding
   procedure Encode_Children (UI      : in UIInclude;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

private

   type UIInclude is new ASF.Components.Html.UIHtmlComponent with null record;

end ASF.Components.Ajax.Includes;
