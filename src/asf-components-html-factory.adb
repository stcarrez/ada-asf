-----------------------------------------------------------------------
--  html-factory -- Factory for HTML UI Components
--  Copyright (C) 2009, 2010, 2011, 2012, 2014, 2018, 2019 Stephane Carrez
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
   function Create_Doctype return UIComponent_Access;
   function Create_Head return UIComponent_Access;
   function Create_Output_Stylesheet return UIComponent_Access;
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
   --  Create an UIInput secret component
   function Create_Input_Secret return ASF.Components.Base.UIComponent_Access;

   --  ------------------------------
   --  Create an UIBody component
   --  ------------------------------
   function Create_Body return UIComponent_Access is
   begin
      return new ASF.Components.Html.Pages.UIBody;
   end Create_Body;

   --  ------------------------------
   --  Create an UIDoctype component
   --  ------------------------------
   function Create_Doctype return UIComponent_Access is
   begin
      return new ASF.Components.Html.Pages.UIDoctype;
   end Create_Doctype;

   --  ------------------------------
   --  Create an UIHead component
   --  ------------------------------
   function Create_Head return UIComponent_Access is
   begin
      return new ASF.Components.Html.Pages.UIHead;
   end Create_Head;

   --  ------------------------------
   --  Create an UIOutputStylesheet component
   --  ------------------------------
   function Create_Output_Stylesheet return UIComponent_Access is
   begin
      return new ASF.Components.Html.Pages.UIOutputStylesheet;
   end Create_Output_Stylesheet;

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
   --  Create an UIInput secret component
   --  ------------------------------
   function Create_Input_Secret return ASF.Components.Base.UIComponent_Access is
      Result : constant Html.Forms.UIInput_Access := new ASF.Components.Html.Forms.UIInput;
   begin
      Result.Set_Secret (True);
      return Result.all'Access;
   end Create_Input_Secret;

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
   DOCTYPE_TAG        : aliased constant String := "doctype";
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
   OUTPUT_STYLESHEET_TAG : aliased constant String := "outputStylesheet";
   OUTPUT_TEXT_TAG    : aliased constant String := "outputText";
   PANEL_GROUP_TAG    : aliased constant String := "panelGroup";
   SELECT_BOOLEAN_TAG  : aliased constant String := "selectBooleanCheckbox";
   SELECT_ONE_MENU_TAG : aliased constant String := "selectOneMenu";
   SELECT_ONE_RADIO_TAG : aliased constant String := "selectOneRadio";

   --  ------------------------------
   --  Get the HTML component factory.
   --  ------------------------------
   procedure Register (Factory : in out ASF.Factory.Component_Factory) is
   begin
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => BODY_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Body'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => COMMAND_BUTTON_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Command'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => DOCTYPE_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Doctype'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => FORM_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Form'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => HEAD_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Head'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => INPUT_FILE_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Input_File'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => INPUT_HIDDEN_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Input_Hidden'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => INPUT_SECRET_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Input_Secret'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => INPUT_TEXT_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Input_Text'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => INPUT_TEXTAREA_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Input_Textarea'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => LIST_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_List'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => MESSAGE_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Message'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => MESSAGES_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Messages'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => OUTPUT_FORMAT_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Output_Format'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => OUTPUT_LABEL_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Output_Label'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => OUTPUT_LINK_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Output_Link'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => OUTPUT_STYLESHEET_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Output_Stylesheet'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => OUTPUT_TEXT_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Output'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => PANEL_GROUP_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_PanelGroup'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => SELECT_BOOLEAN_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_SelectBooleanCheckbox'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => SELECT_ONE_MENU_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_SelectOne'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => SELECT_ONE_RADIO_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_SelectOneRadio'Access);
   end Register;

end ASF.Components.Html.Factory;
