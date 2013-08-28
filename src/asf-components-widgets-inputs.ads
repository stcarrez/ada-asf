-----------------------------------------------------------------------
--  asf-components-widgets-inputs -- Input widget components
--  Copyright (C) 2013 Stephane Carrez
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

with ASF.Components.Html.Forms;
with ASF.Contexts.Faces;
with ASF.Contexts.Writer;
with ASF.Events.Faces;

package ASF.Components.Widgets.Inputs is

   use ASF.Contexts.Faces;
   use ASF.Contexts.Writer;

   --  ------------------------------
   --  Input component
   --  ------------------------------
   --  The AWA input component overrides the ASF input component to build a compact component
   --  that displays a label, the input field and the associated error message if necessary.
   --
   --  The generated HTML looks like:
   --
   --  <dl class='... awa-error'>
   --    <dt>title <i>required</i></dt>
   --    <dd><input type='text' ...> <em/>
   --        <span class='...'>message</span>
   --    </dd>
   --  </dl>
   type UIInput is new ASF.Components.Html.Forms.UIInput with null record;
   type UIInput_Access is access all UIInput'Class;

   --  Render the input field title.
   procedure Render_Title (UI      : in UIInput;
                           Name    : in String;
                           Writer  : in Response_Writer_Access;
                           Context : in out Faces_Context'Class);

   --  Render the input component.  Starts the DL/DD list and write the input
   --  component with the possible associated error message.
   overriding
   procedure Encode_Begin (UI      : in UIInput;
                           Context : in out Faces_Context'Class);

   --  Render the end of the input component.  Closes the DL/DD list.
   overriding
   procedure Encode_End (UI      : in UIInput;
                         Context : in out Faces_Context'Class);

   --  ------------------------------
   --  The auto complete component.
   --  ------------------------------
   --
   type UIComplete is new UIInput with private;
   type UIComplete_Access is access all UIInput'Class;

   --  Render the end of the input component.  Closes the DL/DD list.
   overriding
   procedure Encode_End (UI      : in UIComplete;
                         Context : in out Faces_Context'Class);
   overriding
   procedure Process_Decodes (UI      : in out UIComplete;
                              Context : in out Faces_Context'Class);

   overriding
   procedure Process_Updates (UI      : in out UIComplete;
                              Context : in out Faces_Context'Class);

   --  Broadcast the event to the event listeners installed on this component.
   --  Listeners are called in the order in which they were added.
   overriding
   procedure Broadcast (UI      : in out UIComplete;
                        Event   : not null access ASF.Events.Faces.Faces_Event'Class;
                        Context : in out Faces_Context'Class);

   procedure Render_List (UI      : in UIComplete;
                          Match   : in String;
                          Context : in out Faces_Context'Class);

   --  ------------------------------
   --  The input date component.
   --  ------------------------------
   --
   type UIInputDate is new UIInput with null record;
   type UIInputDate_Access is access all UIInputDate'Class;

   --  Render the end of the input component.  Closes the DL/DD list.
   overriding
   procedure Encode_End (UI      : in UIInputDate;
                         Context : in out Faces_Context'Class);

private

   type UIComplete is new UIInput with record
      Match_Value : Util.Beans.Objects.Object;
   end record;

end ASF.Components.Widgets.Inputs;
