-----------------------------------------------------------------------
--  asf-components-html-messages -- Faces messages
--  Copyright (C) 2011, 2012, 2022 Stephane Carrez
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

with ASF.Applications.Messages.Vectors;

--  The <b>ASF.Components.Html.Messages</b> package implements the <b>h:message</b>
--  and <b>h:messages</b> components.
package ASF.Components.Html.Messages is

   --  Check whether the UI component whose name is given in <b>Name</b> has some messages
   --  associated with it.
   --  ------------------------------
   function Has_Message (Name : in Util.Beans.Objects.Object) return Util.Beans.Objects.Object;

   type Message_Mode is (SPAN_NO_STYLE, SPAN, LIST, TABLE);

   --  Write a single message enclosed by the tag represented by <b>Tag</b>.
   procedure Write_Message (UI           : in UIHtmlComponent'Class;
                            Message      : in ASF.Applications.Messages.Message;
                            Mode         : in Message_Mode;
                            Show_Detail  : in Boolean;
                            Show_Summary : in Boolean;
                            Context      : in out Faces_Context'Class);

   --  ------------------------------
   --  UIMessage component
   --  ------------------------------
   --  The <b>h:message</b> component renders a message associated with an input component.
   type UIMessage is new UIHtmlComponent with private;

   --  Encode the beginning of the <b>h:message</b> component.
   overriding
   procedure Encode_Begin (UI      : in UIMessage;
                           Context : in out Faces_Context'Class);

   --  Encode the end of the <b>h:message</b> component.
   overriding
   procedure Encode_End (UI      : in UIMessage;
                         Context : in out Faces_Context'Class);

   --  ------------------------------
   --  UIMessages component
   --  ------------------------------
   --  The <b>h:messages</b> component renders the global messages.
   type UIMessages is new UIHtmlComponent with private;

   --  Encode the beginning of the <b>h:messages</b> component.
   overriding
   procedure Encode_Begin (UI      : in UIMessages;
                           Context : in out Faces_Context'Class);

   --  Encode the end of the <b>h:messages</b> component.
   overriding
   procedure Encode_End (UI      : in UIMessages;
                         Context : in out Faces_Context'Class);

private

   --  Render a list of messages each of them being enclosed by the <b>Tag</b> element.
   procedure Write_Messages (UI       : in UIHtmlComponent'Class;
                             Mode     : in Message_Mode;
                             Context  : in out Faces_Context'Class;
                             Messages : in out ASF.Applications.Messages.Vectors.Cursor);

   type UIMessage is new UIHtmlComponent with null record;

   type UIMessages is new UIHtmlComponent with null record;

end ASF.Components.Html.Messages;
