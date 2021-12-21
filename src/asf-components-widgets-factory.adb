-----------------------------------------------------------------------
--  widgets-factory -- Factory for widget Components
--  Copyright (C) 2013, 2015, 2018, 2021 Stephane Carrez
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
with ASF.Views.Nodes;
with ASF.Components.Base;
with ASF.Components.Widgets.Inputs;
with ASF.Components.Widgets.Gravatars;
with ASF.Components.Widgets.Likes;
with ASF.Components.Widgets.Panels;
with ASF.Components.Widgets.Tabs;
with ASF.Components.Widgets.Selects;
with ASF.Components.Widgets.Progress;

package body ASF.Components.Widgets.Factory is

   use ASF.Components.Base;

   function Create_Accordion return UIComponent_Access;
   function Create_Input return UIComponent_Access;
   function Create_Input_Date return UIComponent_Access;
   function Create_Chosen return UIComponent_Access;
   function Create_Complete return UIComponent_Access;
   function Create_Gravatar return UIComponent_Access;
   function Create_Like return UIComponent_Access;
   function Create_Panel return UIComponent_Access;
   function Create_Progress return UIComponent_Access;
   function Create_TabView return UIComponent_Access;
   function Create_Tab return UIComponent_Access;

   --  ------------------------------
   --  Create a UIAccordion component
   --  ------------------------------
   function Create_Accordion return UIComponent_Access is
   begin
      return new ASF.Components.Widgets.Tabs.UIAccordion;
   end Create_Accordion;

   --  ------------------------------
   --  Create a UIInput component
   --  ------------------------------
   function Create_Input return UIComponent_Access is
   begin
      return new ASF.Components.Widgets.Inputs.UIInput;
   end Create_Input;

   --  ------------------------------
   --  Create a UIInput component
   --  ------------------------------
   function Create_Input_Date return UIComponent_Access is
   begin
      return new ASF.Components.Widgets.Inputs.UIInputDate;
   end Create_Input_Date;

   --  ------------------------------
   --  Create a UIComplete component
   --  ------------------------------
   function Create_Complete return UIComponent_Access is
   begin
      return new ASF.Components.Widgets.Inputs.UIComplete;
   end Create_Complete;

   --  ------------------------------
   --  Create a UIChoser component
   --  ------------------------------
   function Create_Chosen return UIComponent_Access is
   begin
      return new ASF.Components.Widgets.Selects.UIChosen;
   end Create_Chosen;

   --  ------------------------------
   --  Create a UIGravatar component
   --  ------------------------------
   function Create_Gravatar return UIComponent_Access is
   begin
      return new ASF.Components.Widgets.Gravatars.UIGravatar;
   end Create_Gravatar;

   --  ------------------------------
   --  Create a UILike component
   --  ------------------------------
   function Create_Like return UIComponent_Access is
   begin
      return new ASF.Components.Widgets.Likes.UILike;
   end Create_Like;

   --  ------------------------------
   --  Create a UIPanel component
   --  ------------------------------
   function Create_Panel return UIComponent_Access is
   begin
      return new ASF.Components.Widgets.Panels.UIPanel;
   end Create_Panel;

   --  ------------------------------
   --  Create a UIProgressBar component
   --  ------------------------------
   function Create_Progress return UIComponent_Access is
   begin
      return new ASF.Components.Widgets.Progress.UIProgressBar;
   end Create_Progress;

   --  ------------------------------
   --  Create a UITab component
   --  ------------------------------
   function Create_Tab return UIComponent_Access is
   begin
      return new ASF.Components.Widgets.Tabs.UITab;
   end Create_Tab;

   --  ------------------------------
   --  Create a UITabView component
   --  ------------------------------
   function Create_TabView return UIComponent_Access is
   begin
      return new ASF.Components.Widgets.Tabs.UITabView;
   end Create_TabView;

   use ASF.Views.Nodes;

   URI              : aliased constant String := "http://code.google.com/p/ada-asf/widget";
   ACCORDION_TAG    : aliased constant String := "accordion";
   AUTOCOMPLETE_TAG : aliased constant String := "autocomplete";
   CHOSEN_TAG       : aliased constant String := "chosen";
   INPUT_DATE_TAG   : aliased constant String := "inputDate";
   INPUT_TEXT_TAG   : aliased constant String := "inputText";
   GRAVATAR_TAG     : aliased constant String := "gravatar";
   LIKE_TAG         : aliased constant String := "like";
   PANEL_TAG        : aliased constant String := "panel";
   PROGRESS_TAG     : aliased constant String := "progress";
   TAB_TAG          : aliased constant String := "tab";
   TAB_VIEW_TAG     : aliased constant String := "tabView";

   --  ------------------------------
   --  Get the widget component factory.
   --  ------------------------------
   procedure Register (Factory : in out ASF.Factory.Component_Factory) is
   begin
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => ACCORDION_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Accordion'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => AUTOCOMPLETE_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Complete'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => CHOSEN_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Chosen'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => INPUT_DATE_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Input_Date'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => INPUT_TEXT_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Input'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => GRAVATAR_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Gravatar'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => LIKE_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Like'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => PANEL_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Panel'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => PROGRESS_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Progress'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => TAB_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Tab'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => TAB_VIEW_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_TabView'Access);
   end Register;

end ASF.Components.Widgets.Factory;
