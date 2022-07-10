-----------------------------------------------------------------------
--  asf-components-html-messages -- Faces messages
--  Copyright (C) 2011, 2012, 2013, 2017, 2022 Stephane Carrez
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

with ASF.Utils;
with ASF.Applications.Messages;
with ASF.Components.Base;

--  The <b>ASF.Components.Html.Messages</b> package implements the <b>h:message</b>
--  and <b>h:messages</b> components.
package body ASF.Components.Html.Messages is

   use ASF.Components.Base;
   use ASF.Applications.Messages;

   MSG_ATTRIBUTE_NAMES    : Util.Strings.String_Set.Set;

   FATAL_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   ERROR_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   WARN_ATTRIBUTE_NAMES   : Util.Strings.String_Set.Set;

   INFO_ATTRIBUTE_NAMES   : Util.Strings.String_Set.Set;

   --  ------------------------------
   --  Check whether the UI component whose name is given in <b>Name</b> has some messages
   --  associated with it.
   --  ------------------------------
   function Has_Message (Name : in Util.Beans.Objects.Object) return Util.Beans.Objects.Object is
      Context  : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
   begin
      if Context = null then
         return Util.Beans.Objects.To_Object (False);
      end if;
      declare
         Id   : constant String := Util.Beans.Objects.To_String (Name);
         Msgs : constant ASF.Applications.Messages.Vectors.Cursor := Context.Get_Messages (Id);
      begin
         return Util.Beans.Objects.To_Object (Applications.Messages.Vectors.Has_Element (Msgs));
      end;
   end Has_Message;

   --  ------------------------------
   --  Write a single message enclosed by the tag represented by <b>Tag</b>.
   --  ------------------------------
   procedure Write_Message (UI           : in UIHtmlComponent'Class;
                            Message      : in ASF.Applications.Messages.Message;
                            Mode         : in Message_Mode;
                            Show_Detail  : in Boolean;
                            Show_Summary : in Boolean;
                            Context      : in out Faces_Context'Class) is
      Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
   begin
      case Mode is
         when SPAN_NO_STYLE =>
            Writer.Start_Element ("span");

         when SPAN =>
            Writer.Start_Element ("span");
            UI.Render_Attributes (Context, MSG_ATTRIBUTE_NAMES, Writer);

         when LIST =>
            Writer.Start_Element ("li");

         when TABLE =>
            Writer.Start_Element ("tr");

      end case;

      case Get_Severity (Message) is
         when FATAL =>
            UI.Render_Attributes (Context, FATAL_ATTRIBUTE_NAMES, Writer,
                                  Write_Id => Mode /= SPAN_NO_STYLE);

         when ERROR =>
            UI.Render_Attributes (Context, ERROR_ATTRIBUTE_NAMES, Writer,
                                  Write_Id => Mode /= SPAN_NO_STYLE);

         when WARN =>
            UI.Render_Attributes (Context, WARN_ATTRIBUTE_NAMES, Writer,
                                  Write_Id => Mode /= SPAN_NO_STYLE);

         when INFO | NONE =>
            UI.Render_Attributes (Context, INFO_ATTRIBUTE_NAMES, Writer,
                                  Write_Id => Mode /= SPAN_NO_STYLE);

      end case;

      if Mode = TABLE then
         Writer.Start_Element ("td");
      end if;
      if Show_Summary then
         Writer.Write_Text (Get_Summary (Message));
      end if;
      if Show_Detail then
         Writer.Write_Text (Get_Detail (Message));
      end if;
      case Mode is
         when SPAN | SPAN_NO_STYLE =>
            Writer.End_Element ("span");

         when LIST =>
            Writer.End_Element ("li");

         when TABLE =>
            Writer.End_Element ("td");
            Writer.End_Element ("tr");

      end case;
   end Write_Message;

   --  ------------------------------
   --  Render a list of messages each of them being enclosed by the <b>Tag</b> element.
   --  ------------------------------
   procedure Write_Messages (UI       : in UIHtmlComponent'Class;
                             Mode     : in Message_Mode;
                             Context  : in out Faces_Context'Class;
                             Messages : in out ASF.Applications.Messages.Vectors.Cursor) is

      procedure Process_Message (Message : in ASF.Applications.Messages.Message);

      Show_Detail  : constant Boolean := UI.Get_Attribute ("showDetail", Context, False);
      Show_Summary : constant Boolean := UI.Get_Attribute ("showSummary", Context, True);

      procedure Process_Message (Message : in ASF.Applications.Messages.Message) is
      begin
         Write_Message (UI, Message, Mode, Show_Detail, Show_Summary, Context);
      end Process_Message;

   begin
      while ASF.Applications.Messages.Vectors.Has_Element (Messages) loop
         Vectors.Query_Element (Messages, Process_Message'Access);
         Vectors.Next (Messages);
      end loop;
   end Write_Messages;

   --  ------------------------------
   --  Encode the beginning of the <b>h:message</b> component.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIMessage;
                           Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Name     : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "for");
         Messages : ASF.Applications.Messages.Vectors.Cursor;
      begin
         --  No specification of 'for' attribute, render the global messages.
         if Util.Beans.Objects.Is_Null (Name) then
            Messages := Context.Get_Messages ("");
         else
            declare
               Id     : constant String := Util.Beans.Objects.To_String (Name);
               Target : constant UIComponent_Access := UI.Find (Id => Id);
            begin
               --  If the component does not exist, report an error in the logs.
               if Target = null then
                  UI.Log_Error ("Cannot find component '{0}'", Id);
               else
                  Messages := Context.Get_Messages (Id);
               end if;
            end;
         end if;

         --  If we have some message, render the first one (as specified by <h:message>).
         if ASF.Applications.Messages.Vectors.Has_Element (Messages) then
            declare
               Show_Detail  : constant Boolean := UI.Get_Attribute ("showDetail", Context, True);
               Show_Summary : constant Boolean := UI.Get_Attribute ("showSummary", Context, False);
            begin
               Write_Message (UI, ASF.Applications.Messages.Vectors.Element (Messages),
                              SPAN, Show_Detail, Show_Summary, Context);
            end;
         end if;
      end;
   end Encode_Begin;

   --  ------------------------------
   --  Encode the end of the <b>h:message</b> component.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIMessage;
                         Context : in out Faces_Context'Class) is
   begin
      null;
   end Encode_End;

   --  ------------------------------
   --  Encode the beginning of the <b>h:message</b> component.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIMessages;
                           Context : in out Faces_Context'Class) is
   begin
      if UI.Is_Rendered (Context) then
         declare
            Name     : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "for");
            Messages : ASF.Applications.Messages.Vectors.Cursor;
         begin
            --  No specification of 'for' attribute, render the global messages.
            if Util.Beans.Objects.Is_Null (Name) then
               if UI.Get_Attribute ("globalOnly", Context) then
                  Messages := Context.Get_Messages ("");
               end if;
            else
               declare
                  Id     : constant String := Util.Beans.Objects.To_String (Name);
                  Target : constant UIComponent_Access := UI.Find (Id => Id);
               begin
                  --  If the component does not exist, report an error in the logs.
                  if Target = null then
                     UI.Log_Error ("Cannot find component '{0}'", Id);
                  else
                     Messages := Context.Get_Messages (Id);
                  end if;
               end;
            end if;

            --  If we have some message, render them.
            if ASF.Applications.Messages.Vectors.Has_Element (Messages) then
               declare
                  Writer : constant Response_Writer_Access := Context.Get_Response_Writer;
                  Layout : constant String
                    := Util.Beans.Objects.To_String (UI.Get_Attribute (Context, "layout"));
               begin
                  if Layout = "table" then
                     Writer.Start_Element ("table");
                     UI.Render_Attributes (Context, MSG_ATTRIBUTE_NAMES, Writer);
                     Write_Messages (UI, TABLE, Context, Messages);
                     Writer.End_Element ("table");
                  else
                     Writer.Start_Element ("ul");
                     UI.Render_Attributes (Context, MSG_ATTRIBUTE_NAMES, Writer);
                     Write_Messages (UI, LIST, Context, Messages);
                     Writer.End_Element ("ul");
                  end if;
               end;
            end if;
         end;
      end if;
   end Encode_Begin;

   --  ------------------------------
   --  Encode the end of the <b>h:message</b> component.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIMessages;
                         Context : in out Faces_Context'Class) is
   begin
      null;
   end Encode_End;

   FATAL_CLASS_ATTR       : aliased constant String := "fatalClass";
   FATAL_STYLE_CLASS_ATTR : aliased constant String := "fatalStyle";

   ERROR_CLASS_ATTR       : aliased constant String := "errorClass";
   ERROR_STYLE_CLASS_ATTR : aliased constant String := "errorStyle";

   WARN_CLASS_ATTR        : aliased constant String := "warnClass";
   WARN_STYLE_CLASS_ATTR  : aliased constant String := "warnStyle";

   INFO_CLASS_ATTR        : aliased constant String := "infoClass";
   INFO_STYLE_CLASS_ATTR  : aliased constant String := "infoStyle";

begin
   ASF.Utils.Set_Text_Attributes (MSG_ATTRIBUTE_NAMES);
   FATAL_ATTRIBUTE_NAMES.Insert (FATAL_CLASS_ATTR'Access);
   FATAL_ATTRIBUTE_NAMES.Insert (FATAL_STYLE_CLASS_ATTR'Access);

   ERROR_ATTRIBUTE_NAMES.Insert (ERROR_CLASS_ATTR'Access);
   ERROR_ATTRIBUTE_NAMES.Insert (ERROR_STYLE_CLASS_ATTR'Access);

   WARN_ATTRIBUTE_NAMES.Insert (WARN_CLASS_ATTR'Access);
   WARN_ATTRIBUTE_NAMES.Insert (WARN_STYLE_CLASS_ATTR'Access);

   INFO_ATTRIBUTE_NAMES.Insert (INFO_CLASS_ATTR'Access);
   INFO_ATTRIBUTE_NAMES.Insert (INFO_STYLE_CLASS_ATTR'Access);
end ASF.Components.Html.Messages;
