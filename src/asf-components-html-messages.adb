-----------------------------------------------------------------------
--  html.messages -- Faces messages
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

with Util.Beans.Objects;

with ASF.Utils;
with ASF.Applications.Messages;
with ASF.Components.Base;

--  The <b>ASF.Components.Html.Messages</b> package implements the <b>h:message</b>
--  and <b>h:messages</b> components.
package body ASF.Components.Html.Messages is

   use ASF.Components.Base;
   use ASF.Applications.Messages;


   FATAL_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   ERROR_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;

   WARN_ATTRIBUTE_NAMES   : Util.Strings.String_Set.Set;

   INFO_ATTRIBUTE_NAMES   : Util.Strings.String_Set.Set;

   --  ------------------------------
   --  UIMessage component
   --  ------------------------------

   procedure Write_Messages (UI       : in UIMessage;
                             Context  : in out Faces_Context'Class;
                             Messages : in out ASF.Applications.Messages.Vectors.Cursor) is
      Writer       : constant ResponseWriter_Access := Context.Get_Response_Writer;
      Message      : ASF.Applications.Messages.Message;
      Show_Detail  : constant Boolean := UI.Get_Attribute ("showDetail", Context, True);
      Show_Summary : constant Boolean := UI.Get_Attribute ("showSummary", Context, False);
   begin
      while ASF.Applications.Messages.Vectors.Has_Element (Messages) loop
         Message := Vectors.Element (Messages);
         Writer.Start_Element ("span");

         case Get_Severity (Message) is
            when FATAL =>
               UI.Render_Attributes (Context, FATAL_ATTRIBUTE_NAMES, Writer);

            when ERROR =>
               UI.Render_Attributes (Context, ERROR_ATTRIBUTE_NAMES, Writer);

            when WARN =>
               UI.Render_Attributes (Context, WARN_ATTRIBUTE_NAMES, Writer);

            when INFO | NONE =>
               UI.Render_Attributes (Context, INFO_ATTRIBUTE_NAMES, Writer);

         end case;

         if Show_Summary then
            Writer.Write_Text (Get_Summary (Message));
         end if;
         if Show_Detail then
            Writer.Write_Text (Get_Detail (Message));
         end if;
         Writer.End_Element ("span");
         Vectors.Next (Messages);
      end loop;
   end Write_Messages;

   --  ------------------------------
   --  Encode the begining of the <b>h:message</b> component.
   --  ------------------------------
   procedure Encode_Begin (UI      : in UIMessage;
                           Context : in out Faces_Context'Class) is
   begin
      if UI.Is_Rendered (Context) then
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
                     UI.Log_Error ("Cannot find component {0}", Id);
                  else
                     Messages := Context.Get_Messages (Id);
                  end if;
               end;
            end if;

            --  If we have some message, render them.
            if ASF.Applications.Messages.Vectors.Has_Element (Messages) then
               UI.Write_Messages (Context, Messages);
            end if;
         end;
      end if;
   end Encode_Begin;

   --  Encode the end of the <b>h:message</b> component.
   procedure Encode_End (UI      : in UIMessage;
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
   ASF.Utils.Set_Text_Attributes (ERROR_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Text_Attributes (WARN_ATTRIBUTE_NAMES);
   ASF.Utils.Set_Text_Attributes (INFO_ATTRIBUTE_NAMES);

   ASF.Utils.Set_Text_Attributes (FATAL_ATTRIBUTE_NAMES);
   FATAL_ATTRIBUTE_NAMES.Insert (FATAL_CLASS_ATTR'Access);
   FATAL_ATTRIBUTE_NAMES.Insert (FATAL_STYLE_CLASS_ATTR'Access);

   ERROR_ATTRIBUTE_NAMES.Insert (ERROR_CLASS_ATTR'Access);
   ERROR_ATTRIBUTE_NAMES.Insert (ERROR_STYLE_CLASS_ATTR'Access);

   WARN_ATTRIBUTE_NAMES.Insert (WARN_CLASS_ATTR'Access);
   WARN_ATTRIBUTE_NAMES.Insert (WARN_STYLE_CLASS_ATTR'Access);

   INFO_ATTRIBUTE_NAMES.Insert (INFO_CLASS_ATTR'Access);
   INFO_ATTRIBUTE_NAMES.Insert (INFO_STYLE_CLASS_ATTR'Access);
end ASF.Components.Html.Messages;
