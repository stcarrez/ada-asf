-----------------------------------------------------------------------
--  components-utils-scrollers -- Data scrollers
--  Copyright (C) 2013, 2015 Stephane Carrez
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

with ASF.Contexts.Writer;
package body ASF.Components.Utils.Scrollers is

   --  ------------------------------
   --  Get the list value holder that the scroller is controlling.
   --  ------------------------------
   function Get_List (UI      : in UIScroller;
                      Context : in ASF.Contexts.Faces.Faces_Context'Class)
                      return Holders.List_Holder_Access is
      use type Base.UIComponent_Access;

      Id : constant String := UI.Get_Attribute ("for", Context, "");
      C  : Base.UIComponent_Access;
   begin
      C := UI.Find (Id);
      if C = null then
         return null;
      elsif not (C.all in Holders.List_Holder'Class) then
         return null;
      else
         return Holders.List_Holder'Class (C.all)'Access;
      end if;
   end Get_List;

   --  Encode the data scroller.
   overriding
   procedure Encode_Children (UI      : in UIScroller;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use type Holders.List_Holder_Access;

      List : Holders.List_Holder_Access;
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      List := UIScroller'Class (UI).Get_List (Context);
      if List = null then
         Base.Log_Error (UI, "There is no list object associated with the scroller");
         return;
      end if;
      declare
         Id     : constant Ada.Strings.Unbounded.Unbounded_String := UI.Get_Client_Id;
         Writer : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
         Row_Count    : constant Natural := List.Get_Row_Count; --  (Context);
         Row_Per_Page : constant Natural := List.Get_Row_Per_Page; --  (Context);
         Current_Page : constant Natural := List.Get_Current_Page; --  (Context);
         Page_Count   : constant Natural := (Row_Count + Row_Per_Page - 1) / Row_Per_Page;
      begin
         Writer.Start_Element ("div");
         UI.Render_Attributes (Context, Writer);

         if Current_Page > 1 then
            UI.Render_Page ("first", 1, Context);
         end if;
         if Current_Page > 1 then
            UI.Render_Page ("previous", Current_Page - 1, Context);
         end if;

         if Current_Page < Page_Count then
            UI.Render_Page ("next", Current_Page + 1, Context);
         end if;
         if Current_Page < Page_Count then
            UI.Render_Page ("last", Page_Count, Context);
         end if;
         Writer.End_Element ("div");

         Writer.Queue_Script ("$('#" & Id & "').scroller();");
      end;
   end Encode_Children;

   procedure Render_Page (UI      : in UIScroller;
                          Name    : in String;
                          Page    : in Positive;
                          Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use type ASF.Components.Base.UIComponent_Access;

      Writer : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
      F : constant ASF.Components.Base.UIComponent_Access := UI.Get_Facet (Name);
   begin
      Writer.Start_Element ("div");
      Writer.Write_Attribute ("class", "asf-scroll-" & Name);
      if F /= null then
         F.Encode_All (Context);
      else
         Writer.Write_Text (Positive'Image (Page));
      end if;
      Writer.End_Element ("div");
   end Render_Page;

end ASF.Components.Utils.Scrollers;
