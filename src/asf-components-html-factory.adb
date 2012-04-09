-----------------------------------------------------------------------
--  html-factory -- Factory for HTML UI Components
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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

with ASF.Components.Base;
with ASF.Components.Html.Text;
with ASF.Components.Html.Lists;
with ASF.Components.Html.Links;
with ASF.Components.Html.Panels;
with ASF.Components.Html.Forms;
with ASF.Components.Html.Pages;
with ASF.Components.Html.Selects;
with ASF.Components.Html.Messages;
with ASF.Views.Nodes;
package body ASF.Components.Html.Factory is

   use ASF.Components.Base;

   function Create_Body return UIComponent_Access;
   function Create_Head return UIComponent_Access;
   function Create_Output return UIComponent_Access;
   function Create_Output_Label return UIComponent_Access;
   function Create_Output_Link return UIComponent_Access;
   function Create_Output_Format return UIComponent_Access;
   function Create_List return UIComponent_Access;
   function Create_PanelGroup return UIComponent_Access;
   function Create_Form return UIComponent_Access;
   function Create_Input_File return UIComponent_Access;
   function Create_Input_Hidden return UIComponent_Access;
   function Create_Input_Text return UIComponent_Access;
   function Create_Input_Textarea return UIComponent_Access;
   function Create_Command return UIComponent_Access;
   function Create_Message return UIComponent_Access;
   function Create_Messages return UIComponent_Access;
   function Create_SelectOne return UIComponent_Access;
   function Create_SelectOneRadio return UIComponent_Access;
   function Create_SelectBooleanCheckbox return UIComponent_Access;

   --  ------------------------------
   --  Create an UIBody component
   --  ------------------------------
   function Create_Body return UIComponent_Access is
   begin
      return new ASF.Components.Html.Pages.UIBody;
   end Create_Body;

   --  ------------------------------
   --  Create an UIHead component
   --  ------------------------------
   function Create_Head return UIComponent_Access is
   begin
      return new ASF.Components.Html.Pages.UIHead;
   end Create_Head;

   --  ------------------------------
   --  Create an UIOutput component
   --  ------------------------------
   function Create_Output return UIComponent_Access is
   begin
      return new ASF.Components.Html.Text.UIOutput;
   end Create_Output;

   --  ------------------------------
   --  Create an UIOutputLabel component
   --  ------------------------------
   function Create_Output_Label return UIComponent_Access is
   begin
      return new ASF.Components.Html.Text.UIOutputLabel;
   end Create_Output_Label;

   --  ------------------------------
   --  Create an UIOutputLink component
   --  ------------------------------
   function Create_Output_Link return UIComponent_Access is
   begin
      return new ASF.Components.Html.Links.UIOutputLink;
   end Create_Output_Link;

   --  ------------------------------
   --  Create an UIOutput component
   --  ------------------------------
   function Create_Output_Format return UIComponent_Access is
   begin
      return new ASF.Components.Html.Text.UIOutputFormat;
   end Create_Output_Format;

   --  ------------------------------
   --  Create an UIList component
   --  ------------------------------
   function Create_List return UIComponent_Access is
   begin
      return new ASF.Components.Html.Lists.UIList;
   end Create_List;

   --  ------------------------------
   --  Create an UIPanelGroup component
   --  ------------------------------
   function Create_PanelGroup return UIComponent_Access is
   begin
      return new ASF.Components.Html.Panels.UIPanelGroup;
   end Create_PanelGroup;

   --  ------------------------------
   --  Create an UIForm component
   --  ------------------------------
   function Create_Form return UIComponent_Access is
   begin
      return new ASF.Components.Html.Forms.UIForm;
   end Create_Form;

   --  ------------------------------
   --  Create an UIInput_Hidden component
   --  ------------------------------
   function Create_Input_Hidden return UIComponent_Access is
   begin
      return new ASF.Components.Html.Forms.UIInput_Hidden;
   end Create_Input_Hidden;

   --  ------------------------------
   --  Create an UIInput component
   --  ------------------------------
   function Create_Input_Text return UIComponent_Access is
   begin
      return new ASF.Components.Html.Forms.UIInput;
   end Create_Input_Text;

   --  ------------------------------
   --  Create an UIInputTextarea component
   --  ------------------------------
   function Create_Input_Textarea return UIComponent_Access is
   begin
      return new ASF.Components.Html.Forms.UIInputTextarea;
   end Create_Input_Textarea;

   --  ------------------------------
   --  Create an UIInput_File component
   --  ------------------------------
   function Create_Input_File return UIComponent_Access is
   begin
      return new ASF.Components.Html.Forms.UIInput_File;
   end Create_Input_File;

   --  ------------------------------
   --  Create an UICommand component
   --  ------------------------------
   function Create_Command return UIComponent_Access is
   begin
      return new ASF.Components.Html.Forms.UICommand;
   end Create_Command;

   --  ------------------------------
   --  Create an UIMessage component
   --  ------------------------------
   function Create_Message return UIComponent_Access is
   begin
      return new ASF.Components.Html.Messages.UIMessage;
   end Create_Message;

   --  ------------------------------
   --  Create an UIMessages component
   --  ------------------------------
   function Create_Messages return UIComponent_Access is
   begin
      return new ASF.Components.Html.Messages.UIMessages;
   end Create_Messages;

   --  ------------------------------
   --  Create an UISelectOne component
   --  ------------------------------
   function Create_SelectOne return UIComponent_Access is
   begin
      return new ASF.Components.Html.Selects.UISelectOne;
   end Create_SelectOne;

   --  ------------------------------
   --  Create an UISelectOneRadio component
   --  ------------------------------
   function Create_SelectOneRadio return UIComponent_Access is
   begin
      return new ASF.Components.Html.Selects.UISelectOneRadio;
   end Create_SelectOneRadio;

   --  ------------------------------
   --  Create an UISelectBoolean component
   --  ------------------------------
   function Create_SelectBooleanCheckbox return UIComponent_Access is
   begin
      return new ASF.Components.Html.Selects.UISelectBoolean;
   end Create_SelectBooleanCheckbox;

   use ASF.Views.Nodes;

   URI                : aliased constant String := "http://java.sun.com/jsf/html";

   BODY_TAG           : aliased constant String := "body";
   COMMAND_BUTTON_TAG : aliased constant String := "commandButton";
   FORM_TAG           : aliased constant String := "form";
   HEAD_TAG           : aliased constant String := "head";
   INPUT_FILE_TAG     : aliased constant String := "inputFile";
   INPUT_HIDDEN_TAG   : aliased constant String := "inputHidden";
   INPUT_SECRET_TAG   : aliased constant String := "inputSecret";
   INPUT_TEXT_TAG     : aliased constant String := "inputText";
   INPUT_TEXTAREA_TAG : aliased constant String := "inputTextarea";
   LIST_TAG           : aliased constant String := "list";
   MESSAGE_TAG        : aliased constant String := "message";
   MESSAGES_TAG       : aliased constant String := "messages";
   OUTPUT_FORMAT_TAG  : aliased constant String := "outputFormat";
   OUTPUT_LABEL_TAG   : aliased constant String := "outputLabel";
   OUTPUT_LINK_TAG    : aliased constant String := "outputLink";
   OUTPUT_TEXT_TAG    : aliased constant String := "outputText";
   PANEL_GROUP_TAG    : aliased constant String := "panelGroup";
   SELECT_ONE_MENU_TAG : aliased constant String := "selectOneMenu";
   SELECT_ONE_RADIO_TAG : aliased constant String := "selectOneRadio";
   SELECT_BOOLEAN_TAG  : aliased constant String := "selectBooleanCheckbox";

   Html_Bindings : aliased constant ASF.Factory.Binding_Array
     := (1 => (Name      => BODY_TAG'Access,
               Component => Create_Body'Access,
               Tag       => Create_Component_Node'Access),
         2 => (Name      => COMMAND_BUTTON_TAG'Access,
               Component => Create_Command'Access,
               Tag       => Create_Component_Node'Access),
         3 => (Name      => FORM_TAG'Access,
               Component => Create_Form'Access,
               Tag       => Create_Component_Node'Access),
         4 => (Name      => HEAD_TAG'Access,
               Component => Create_Head'Access,
               Tag       => Create_Component_Node'Access),
         5 => (Name      => INPUT_FILE_TAG'Access,
               Component => Create_Input_File'Access,
               Tag       => Create_Component_Node'Access),
         6 => (Name      => INPUT_HIDDEN_TAG'Access,
               Component => Create_Input_Hidden'Access,
               Tag       => Create_Component_Node'Access),
         7 => (Name      => INPUT_SECRET_TAG'Access,
               Component => ASF.Components.Html.Forms.Create_Input_Secret'Access,
               Tag       => Create_Component_Node'Access),
         8 => (Name      => INPUT_TEXT_TAG'Access,
               Component => Create_Input_Text'Access,
               Tag       => Create_Component_Node'Access),
         9 => (Name      => INPUT_TEXTAREA_TAG'Access,
               Component => Create_Input_Textarea'Access,
               Tag       => Create_Component_Node'Access),
        10 => (Name      => LIST_TAG'Access,
               Component => Create_List'Access,
               Tag       => Create_Component_Node'Access),
        11 => (Name      => MESSAGE_TAG'Access,
               Component => Create_Message'Access,
               Tag       => Create_Component_Node'Access),
        12 => (Name      => MESSAGES_TAG'Access,
               Component => Create_Messages'Access,
               Tag       => Create_Component_Node'Access),
        13 => (Name      => OUTPUT_FORMAT_TAG'Access,
               Component => Create_Output_Format'Access,
               Tag       => Create_Component_Node'Access),
        14 => (Name      => OUTPUT_LABEL_TAG'Access,
               Component => Create_Output_Label'Access,
               Tag       => Create_Component_Node'Access),
        15 => (Name      => OUTPUT_LINK_TAG'Access,
               Component => Create_Output_Link'Access,
               Tag       => Create_Component_Node'Access),
        16 => (Name      => OUTPUT_TEXT_TAG'Access,
               Component => Create_Output'Access,
               Tag       => Create_Component_Node'Access),
        17 => (Name      => PANEL_GROUP_TAG'Access,
               Component => Create_PanelGroup'Access,
               Tag       => Create_Component_Node'Access),
        18 => (Name      => SELECT_BOOLEAN_TAG'Access,
               Component => Create_SelectBooleanCheckbox'Access,
               Tag       => Create_Component_Node'Access),
        19 => (Name      => SELECT_ONE_MENU_TAG'Access,
               Component => Create_SelectOne'Access,
               Tag       => Create_Component_Node'Access),
        20 => (Name      => SELECT_ONE_RADIO_TAG'Access,
               Component => Create_SelectOneRadio'Access,
               Tag       => Create_Component_Node'Access)
        );

   Html_Factory : aliased constant ASF.Factory.Factory_Bindings
     := (URI => URI'Access, Bindings => Html_Bindings'Access);

   --  ------------------------------
   --  Get the HTML component factory.
   --  ------------------------------
   function Definition return ASF.Factory.Factory_Bindings_Access is
   begin
      return Html_Factory'Access;
   end Definition;

end ASF.Components.Html.Factory;
