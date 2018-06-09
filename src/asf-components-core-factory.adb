-----------------------------------------------------------------------
--  core-factory -- Factory for Core UI Components
--  Copyright (C) 2009, 2010, 2011, 2012, 2014, 2018 Stephane Carrez
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
with ASF.Views.Nodes;
with ASF.Views.Nodes.Jsf;
with ASF.Components.Html.Selects;
with ASF.Components.Core.Views;
package body ASF.Components.Core.Factory is

   function Create_View return Base.UIComponent_Access;
   function Create_ViewAction return Base.UIComponent_Access;
   function Create_ViewMetaData return Base.UIComponent_Access;
   function Create_ViewParameter return Base.UIComponent_Access;
   function Create_Parameter return Base.UIComponent_Access;
   function Create_SelectItem return Base.UIComponent_Access;
   function Create_SelectItems return Base.UIComponent_Access;

   --  ------------------------------
   --  Create an UIView component
   --  ------------------------------
   function Create_View return Base.UIComponent_Access is
   begin
      return new ASF.Components.Core.Views.UIView;
   end Create_View;

   --  ------------------------------
   --  Create an UIViewAction component
   --  ------------------------------
   function Create_ViewAction return Base.UIComponent_Access is
   begin
      return new ASF.Components.Core.Views.UIViewAction;
   end Create_ViewAction;

   --  ------------------------------
   --  Create an UIViewMetaData component
   --  ------------------------------
   function Create_ViewMetaData return Base.UIComponent_Access is
   begin
      return new ASF.Components.Core.Views.UIViewMetaData;
   end Create_ViewMetaData;

   --  ------------------------------
   --  Create an UIViewParameter component
   --  ------------------------------
   function Create_ViewParameter return Base.UIComponent_Access is
   begin
      return new ASF.Components.Core.Views.UIViewParameter;
   end Create_ViewParameter;

   --  ------------------------------
   --  Create an UIParameter component
   --  ------------------------------
   function Create_Parameter return Base.UIComponent_Access is
   begin
      return new ASF.Components.Core.UIParameter;
   end Create_Parameter;

   --  ------------------------------
   --  Create an UISelectItem component
   --  ------------------------------
   function Create_SelectItem return Base.UIComponent_Access is
   begin
      return new ASF.Components.Html.Selects.UISelectItem;
   end Create_SelectItem;

   --  ------------------------------
   --  Create an UISelectItems component
   --  ------------------------------
   function Create_SelectItems return Base.UIComponent_Access is
   begin
      return new ASF.Components.Html.Selects.UISelectItems;
   end Create_SelectItems;

   use ASF.Views.Nodes;

   URI                     : aliased constant String := "http://java.sun.com/jsf/core";
   ATTRIBUTE_TAG           : aliased constant String := "attribute";
   CONVERT_DATE_TIME_TAG   : aliased constant String := "convertDateTime";
   CONVERTER_TAG           : aliased constant String := "converter";
   FACET_TAG               : aliased constant String := "facet";
   METADATA_TAG            : aliased constant String := "metadata";
   PARAM_TAG               : aliased constant String := "param";
   SELECT_ITEM_TAG         : aliased constant String := "selectItem";
   SELECT_ITEMS_TAG        : aliased constant String := "selectItems";
   VALIDATE_LENGTH_TAG     : aliased constant String := "validateLength";
   VALIDATE_LONG_RANGE_TAG : aliased constant String := "validateLongRange";
   VALIDATOR_TAG           : aliased constant String := "validator";
   VIEW_TAG                : aliased constant String := "view";
   VIEW_ACTION_TAG         : aliased constant String := "viewAction";
   VIEW_PARAM_TAG          : aliased constant String := "viewParam";

   --  ------------------------------
   --  Get the HTML component factory.
   --  ------------------------------
   procedure Register (Factory : in out ASF.Factory.Component_Factory) is
   begin
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => ATTRIBUTE_TAG'Access,
                            Tag    => ASF.Views.Nodes.Jsf.Create_Attribute_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => CONVERT_DATE_TIME_TAG'Access,
                            Tag    => ASF.Views.Nodes.Jsf.Create_Convert_Date_Time_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => CONVERTER_TAG'Access,
                            Tag    => ASF.Views.Nodes.Jsf.Create_Converter_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => FACET_TAG'Access,
                            Tag    => ASF.Views.Nodes.Jsf.Create_Facet_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => METADATA_TAG'Access,
                            Tag    => ASF.Views.Nodes.Jsf.Create_Metadata_Tag_Node'Access,
                            Create => Create_ViewMetaData'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => PARAM_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Parameter'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => SELECT_ITEM_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_SelectItem'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => SELECT_ITEMS_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_SelectItems'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => VALIDATE_LENGTH_TAG'Access,
                            Tag    => ASF.Views.Nodes.Jsf.Create_Length_Validator_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => VALIDATE_LONG_RANGE_TAG'Access,
                            Tag    => ASF.Views.Nodes.Jsf.Create_Range_Validator_Tag_Node'Access,
                            Create => null);

      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => VALIDATOR_TAG'Access,
                            Tag    => ASF.Views.Nodes.Jsf.Create_Validator_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => VIEW_TAG'Access,
                            Tag    => ASF.Views.Nodes.Jsf.Create_Validator_Tag_Node'Access,
                            Create => null);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => VIEW_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_View'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => VIEW_ACTION_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_ViewAction'Access);
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => VIEW_PARAM_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_ViewParameter'Access);
   end Register;

end ASF.Components.Core.Factory;
