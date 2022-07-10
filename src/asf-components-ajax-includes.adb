-----------------------------------------------------------------------
--  components-ajax-includes -- AJAX Include component
--  Copyright (C) 2011, 2022 Stephane Carrez
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

with ASF.Applications.Main;
with ASF.Applications.Views;
with ASF.Components.Root;
with ASF.Contexts.Writer;

package body ASF.Components.Ajax.Includes is

   --  ------------------------------
   --  Get the HTML layout that must be used for the include container.
   --  The default layout is a "div".
   --  Returns "div", "span", "pre", "b".
   --  ------------------------------
   function Get_Layout (UI      : in UIInclude;
                        Context : in ASF.Contexts.Faces.Faces_Context'Class) return String is
      Layout : constant String := UI.Get_Attribute (Name    => LAYOUT_ATTR_NAME,
                                                    Context => Context,
                                                    Default => "div");
   begin
      if Layout = "div" or else Layout = "span" or else Layout = "pre" or else Layout = "b" then
         return Layout;
      else
         return "div";
      end if;
   end Get_Layout;

   --  ------------------------------
   --  The included XHTML file is rendered according to the <b>async</b> attribute:
   --
   --  When <b>async</b> is false, render the specified XHTML file in such a way that inner
   --  forms will be posted on the included view.
   --
   --  When <b>async</b> is true, trigger an AJAX call to include the specified
   --  XHTML view when the page is loaded.
   --
   --
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UIInclude;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         App    : constant ASF.Contexts.Faces.Application_Access := Context.Get_Application;
         View_Handler : constant access ASF.Applications.Views.View_Handler'Class
           := App.Get_View_Handler;
         Id     : constant Ada.Strings.Unbounded.Unbounded_String := UI.Get_Client_Id;
         Layout : constant String := UIInclude'Class (UI).Get_Layout (Context);
         Async  : constant Boolean := UI.Get_Attribute (Name    => ASYNC_ATTR_NAME,
                                                        Context => Context,
                                                        Default => False);
         Page   : constant String := UI.Get_Attribute (Name    => SRC_ATTR_NAME,
                                                       Context => Context,
                                                       Default => "");
         Writer : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
      begin
         Writer.Start_Element (Layout);
         UI.Render_Attributes (Context, Writer);

         --  In Async mode, generate the javascript code to trigger the async update of
         --  the generated div/span.
         if Async then
            Writer.Write_Attribute ("id", Id);
            Writer.Queue_Script ("ASF.Update(null,""");
            Writer.Queue_Script (View_Handler.Get_Action_URL (Context, Page));
            Writer.Queue_Script (""", ""#");
            Writer.Queue_Script (Id);
            Writer.Queue_Script (""");");
         else
            --  Include the view content as if the user fetched the patch.  This has almost the
            --  same final result except that the inner content is returned now and not by
            --  another async http GET request.
            declare
               View         : constant ASF.Components.Root.UIViewRoot := Context.Get_View_Root;
               Include_View : ASF.Components.Root.UIViewRoot;
               Is_Ajax      : constant Boolean := Context.Is_Ajax_Request;
               Content_Type : constant String := Context.Get_Response.Get_Content_Type;
            begin
               Context.Set_Ajax_Request (True);
               View_Handler.Restore_View (Page, Context, Include_View);
               Context.Set_View_Root (Include_View);
               View_Handler.Render_View (Context, Include_View);
               Context.Get_Response.Set_Content_Type (Content_Type);
               Context.Set_View_Root (View);
               Context.Set_Ajax_Request (Is_Ajax);

            exception
               when others =>
                  Context.Get_Response.Set_Content_Type (Content_Type);
                  Context.Set_View_Root (View);
                  Context.Set_Ajax_Request (Is_Ajax);
                  raise;
            end;
         end if;
         Writer.End_Element (Layout);
      end;
   end Encode_Children;

end ASF.Components.Ajax.Includes;
