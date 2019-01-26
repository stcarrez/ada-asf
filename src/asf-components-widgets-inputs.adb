-----------------------------------------------------------------------
--  asf-components-widgets-inputs -- Input widget components
--  Copyright (C) 2013, 2015, 2017, 2019 Stephane Carrez
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

with Ada.Strings.Unbounded;
with Ada.Characters.Conversions;

with ASF.Models.Selects;
with ASF.Components.Utils;
with ASF.Components.Html.Messages;
with ASF.Events.Faces.Actions;
with ASF.Views.Nodes;
with ASF.Applications.Messages.Vectors;

with Util.Beans.Basic;
with Util.Strings.Transforms;
package body ASF.Components.Widgets.Inputs is

   --  ------------------------------
   --  Render the input field title.
   --  ------------------------------
   procedure Render_Title (UI      : in UIInput;
                           Name    : in String;
                           Writer  : in Response_Writer_Access;
                           Context : in out Faces_Context'Class) is
      Title : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "title");
   begin
      Writer.Start_Element ("dt");
      Writer.Start_Element ("label");
      Writer.Write_Attribute ("for", Name);
      Writer.Write_Text (Title);
      Writer.End_Element ("label");
      Writer.End_Element ("dt");
   end Render_Title;

   --  ------------------------------
   --  Render the input component.  Starts the DL/DD list and write the input
   --  component with the possible associated error message.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIInput;
                           Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Id       : constant String := To_String (UI.Get_Client_Id);
         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
         Messages : constant ASF.Applications.Messages.Vectors.Cursor := Context.Get_Messages (Id);
         Style    : constant String := UI.Get_Attribute ("styleClass", Context);
      begin
         Writer.Start_Element ("dl");
         Writer.Write_Attribute ("id", Id);
         if ASF.Applications.Messages.Vectors.Has_Element (Messages) then
            Writer.Write_Attribute ("class", Style & " asf-error");
         elsif Style'Length > 0 then
            Writer.Write_Attribute ("class", Style);
         end if;
         UI.Render_Title (Id, Writer, Context);
         Writer.Start_Element ("dd");
         UI.Render_Input (Context, Write_Id => False);
      end;
   end Encode_Begin;

   --  ------------------------------
   --  Render the end of the input component.  Closes the DL/DD list.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIInput;
                         Context : in out Faces_Context'Class) is
      use ASF.Components.Html.Messages;
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      --  Render the error message associated with the input field.
      declare
         Id       : constant String := To_String (UI.Get_Client_Id);
         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
         Messages : constant ASF.Applications.Messages.Vectors.Cursor := Context.Get_Messages (Id);
      begin
         if ASF.Applications.Messages.Vectors.Has_Element (Messages) then
            Write_Message (UI, ASF.Applications.Messages.Vectors.Element (Messages),
                           SPAN_NO_STYLE, False, True, Context);
         end if;
         Writer.End_Element ("dd");
         Writer.End_Element ("dl");
      end;
   end Encode_End;

   --  ------------------------------
   --  Render the end of the input component.  Closes the DL/DD list.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIComplete;
                         Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      --  Render the autocomplete script and finish the input component.
      declare
         Id       : constant String := To_String (UI.Get_Client_Id);
         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
      begin
         Writer.Queue_Script ("$('#");
         Writer.Queue_Script (Id);
         Writer.Queue_Script (" input').complete({");
         Writer.Queue_Script ("});");
         UIInput (UI).Encode_End (Context);
      end;
   end Encode_End;

   overriding
   procedure Process_Decodes (UI      : in out UIComplete;
                              Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Id  : constant String := To_String (UI.Get_Client_Id);
         Val : constant String := Context.Get_Parameter (Id & ".match");
      begin
         if Val'Length > 0 then
            UI.Match_Value := Util.Beans.Objects.To_Object (Val);
         else
            ASF.Components.Html.Forms.UIInput (UI).Process_Decodes (Context);
         end if;
      end;
   end Process_Decodes;

   procedure Render_List (UI      : in UIComplete;
                          Match   : in String;
                          Context : in out Faces_Context'Class) is
      use type Util.Beans.Basic.List_Bean_Access;

      procedure Render_Item (Label : in String);

      List       : constant Util.Beans.Basic.List_Bean_Access
        := ASF.Components.Utils.Get_List_Bean (UI, "autocompleteList", Context);
      Writer     : constant Response_Writer_Access := Context.Get_Response_Writer;
      Need_Comma : Boolean := False;
      Count      : Natural;

      procedure Render_Item (Label : in String) is
         Result : Ada.Strings.Unbounded.Unbounded_String;
      begin
         if Match'Length = 0 or else
           (Match'Length <= Label'Length
            and then Match = Label (Label'First .. Label'First + Match'Length - 1))
         then
            if Need_Comma then
               Writer.Write (",");
            end if;
            Writer.Write ('"');
            Util.Strings.Transforms.Escape_Java (Content => Label,
                                                 Into    => Result);
            Writer.Write (Result);
            Writer.Write ('"');
            Need_Comma := True;
         end if;
      end Render_Item;

   begin
      Writer.Write ('[');
      if List /= null then
         Count := List.Get_Count;
         if List.all in ASF.Models.Selects.Select_Item_List'Class then
            declare
               S : constant access ASF.Models.Selects.Select_Item_List'Class
                 := ASF.Models.Selects.Select_Item_List'Class (List.all)'Access;
            begin
               for I in 1 .. Count loop
                  declare
                     Value : constant ASF.Models.Selects.Select_Item := S.Get_Select_Item (I);
                  begin
                     Render_Item (Ada.Characters.Conversions.To_String (Value.Get_Label));
                  end;
               end loop;
            end;
         else
            for I in 1 .. Count loop
               List.Set_Row_Index (I);
               declare
                  Value : constant Util.Beans.Objects.Object := List.Get_Row;
                  Label : constant String := Util.Beans.Objects.To_String (Value);
               begin
                  Render_Item (Label);
               end;
            end loop;
         end if;
      end if;
      Writer.Write (']');
   end Render_List;

   overriding
   procedure Process_Updates (UI      : in out UIComplete;
                              Context : in out Faces_Context'Class) is
   begin
      if not Util.Beans.Objects.Is_Empty (UI.Match_Value) then
         declare
            Value : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute ("match");
            ME    : EL.Expressions.Method_Expression;
         begin
            if Value /= null then
               declare
                  VE    : constant EL.Expressions.Value_Expression
                    := ASF.Views.Nodes.Get_Value_Expression (Value.all);
               begin
                  VE.Set_Value (Value => UI.Match_Value, Context => Context.Get_ELContext.all);
               end;
            end if;

            --  Post an event on this component to trigger the rendering of the completion
            --  list as part of an application/json output.  The rendering is made by Broadcast.
            ASF.Events.Faces.Actions.Post_Event (UI     => UI,
                                                 Method => ME);
         end;
      else
         ASF.Components.Html.Forms.UIInput (UI).Process_Updates (Context);
      end if;

--     exception
--        when E : others =>
--           UI.Is_Valid := False;
--           UI.Add_Message (CONVERTER_MESSAGE_NAME, "convert", Context);
--           Log.Info (Utils.Get_Line_Info (UI)
--                     & ": Exception raised when updating value {0} for component {1}: {2}",
--                     EL.Objects.To_String (UI.Submitted_Value),
--                     To_String (UI.Get_Client_Id), Ada.Exceptions.Exception_Name (E));
   end Process_Updates;

   --  ------------------------------
   --  Broadcast the event to the event listeners installed on this component.
   --  Listeners are called in the order in which they were added.
   --  ------------------------------
   overriding
   procedure Broadcast (UI      : in out UIComplete;
                        Event   : not null access ASF.Events.Faces.Faces_Event'Class;
                        Context : in out Faces_Context'Class) is
      pragma Unreferenced (Event);

      Match  : constant String := Util.Beans.Objects.To_String (UI.Match_Value);
   begin
      Context.Get_Response.Set_Content_Type ("application/json; charset=UTF-8");
      UI.Render_List (Match, Context);
      Context.Response_Completed;
   end Broadcast;

   --  ------------------------------
   --  Render the end of the input date component.
   --  Generate the javascript code to activate the input date selector
   --  and closes the DL/DD list.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIInputDate;
                         Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      --  Render the input date script and finish the input component.
      declare
         Id       : constant String := To_String (UI.Get_Client_Id);
         Writer   : constant Response_Writer_Access := Context.Get_Response_Writer;
         Format   : constant String := UI.Get_Attribute ("dateFormat", Context);
      begin
         Writer.Queue_Script ("$('#");
         Writer.Queue_Script (Id);
         Writer.Queue_Script (" input').datepicker({");
         if Format'Length > 0 then
            Writer.Queue_Script ("dateFormat: """);
            Writer.Queue_Script (Format);
            Writer.Queue_Script ("""");
         end if;
         Writer.Queue_Script ("});");
         UIInput (UI).Encode_End (Context);
      end;
   end Encode_End;

end ASF.Components.Widgets.Inputs;
