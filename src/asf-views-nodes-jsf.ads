-----------------------------------------------------------------------
--  asf-views-nodes-jsf -- JSF Core Tag Library
--  Copyright (C) 2010, 2011, 2012, 2013, 2014, 2018, 2020 Stephane Carrez
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
with ASF.Contexts.Facelets;
with ASF.Validators;

--  The <b>ASF.Views.Nodes.Jsf</b> package implements various JSF Core Tag
--  components which alter the component tree but don't need to create
--  new UI components in the usual way.  The following components are supported:
--
--  <f:attribute name='...' value='...'/>
--  <f:converter converterId='...'/>
--  <f:validateXXX .../>
--  <f:facet name='...'>...</f:facet>
--
--  The <b>f:attribute</b>, <b>f:converter</b> and <b>f:validateXXX</b> tags don't create any
--  component.  The <b>f:facet</b> creates components that are inserted as a facet component
--  in the component tree.
package ASF.Views.Nodes.Jsf is

   --  ------------------------------
   --  Converter Tag
   --  ------------------------------
   --  The <b>Converter_Tag_Node</b> is created in the facelet tree when
   --  the <f:converter> element is found.  When building the component tree,
   --  we have to find the <b>Converter</b> object and attach it to the
   --  parent component.  The parent component must implement the <b>Value_Holder</b>
   --  interface.
   type Converter_Tag_Node is new Views.Nodes.Tag_Node with private;
   type Converter_Tag_Node_Access is access all Converter_Tag_Node'Class;

   --  Create the Converter Tag
   function Create_Converter_Tag_Node (Binding    : in Binding_Type;
                                       Line       : in Views.Line_Info;
                                       Parent     : in Views.Nodes.Tag_Node_Access;
                                       Attributes : in Views.Nodes.Tag_Attribute_Array_Access)
                                       return Views.Nodes.Tag_Node_Access;

   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.  Get the specified converter and
   --  add it to the parent component.  This operation does not create any
   --  new UIComponent.
   overriding
   procedure Build_Components (Node    : access Converter_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class);

   --  ------------------------------
   --  Convert Date Time Tag
   --  ------------------------------
   --  The <b>Convert_Date_Time_Tag_Node</b> is created in the facelet tree when
   --  the <f:converterDateTime> element is found.  When building the component tree,
   --  we have to find the <b>Converter</b> object and attach it to the
   --  parent component.  The parent component must implement the <b>Value_Holder</b>
   --  interface.
   type Convert_Date_Time_Tag_Node is new Views.Nodes.Tag_Node with private;
   type Convert_Date_Time_Tag_Node_Access is access all Convert_Date_Time_Tag_Node'Class;

   --  Create the Converter Tag
   function Create_Convert_Date_Time_Tag_Node (Binding    : in Binding_Type;
                                               Line       : in Views.Line_Info;
                                               Parent     : in Views.Nodes.Tag_Node_Access;
                                               Attributes : in Nodes.Tag_Attribute_Array_Access)
                                               return Views.Nodes.Tag_Node_Access;

   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.  Get the specified converter and
   --  add it to the parent component.  This operation does not create any
   --  new UIComponent.
   overriding
   procedure Build_Components (Node    : access Convert_Date_Time_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class);

   --  ------------------------------
   --  Validator Tag
   --  ------------------------------
   --  The <b>Validator_Tag_Node</b> is created in the facelet tree when
   --  the <f:validateXXX> element is found.  When building the component tree,
   --  we have to find the <b>Validator</b> object and attach it to the
   --  parent component.  The parent component must implement the <b>Editable_Value_Holder</b>
   --  interface.
   type Validator_Tag_Node is new Views.Nodes.Tag_Node with private;
   type Validator_Tag_Node_Access is access all Validator_Tag_Node'Class;

   --  Create the Validator Tag
   function Create_Validator_Tag_Node (Binding    : in Binding_Type;
                                       Line       : in Views.Line_Info;
                                       Parent     : in Views.Nodes.Tag_Node_Access;
                                       Attributes : in Views.Nodes.Tag_Attribute_Array_Access)
                                       return Views.Nodes.Tag_Node_Access;

   --  Get the validator instance that corresponds to the validator tag.
   --  Returns in <b>Validator</b> the instance if it exists and indicate
   --  in <b>Shared</b> whether it must be freed or not when the component is deleted.
   procedure Get_Validator (Node      : in Validator_Tag_Node;
                            Context   : in out Contexts.Facelets.Facelet_Context'Class;
                            Validator : out Validators.Validator_Access;
                            Shared    : out Boolean);

   --  Get the specified validator and add it to the parent component.
   --  This operation does not create any new UIComponent.
   overriding
   procedure Build_Components (Node    : access Validator_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class);

   --  ------------------------------
   --  Range Validator Tag
   --  ------------------------------
   --  The <b>Range_Validator_Tag_Node</b> is created in the facelet tree when
   --  the <f:validateLongRange> element is found.
   --  The parent component must implement the <b>Editable_Value_Holder</b>
   --  interface.
   type Range_Validator_Tag_Node is new Validator_Tag_Node with private;
   type Range_Validator_Tag_Node_Access is access all Range_Validator_Tag_Node'Class;

   --  Create the Range_Validator Tag
   function Create_Range_Validator_Tag_Node (Binding    : in Binding_Type;
                                             Line       : in Views.Line_Info;
                                             Parent     : in Views.Nodes.Tag_Node_Access;
                                             Attributes : in Nodes.Tag_Attribute_Array_Access)
                                             return Views.Nodes.Tag_Node_Access;

   --  Get the validator instance that corresponds to the range validator.
   --  Returns in <b>Validator</b> the validator instance if it exists and indicate
   --  in <b>Shared</b> whether it must be freed or not when the component is deleted.
   overriding
   procedure Get_Validator (Node      : in Range_Validator_Tag_Node;
                            Context   : in out Contexts.Facelets.Facelet_Context'Class;
                            Validator : out Validators.Validator_Access;
                            Shared    : out Boolean);

   --  ------------------------------
   --  Length Validator Tag
   --  ------------------------------
   --  The <b>Length_Validator_Tag_Node</b> is created in the facelet tree when
   --  the <f:validateLength> element is found.  When building the component tree,
   --  we have to find the <b>Validator</b> object and attach it to the
   --  parent component.  The parent component must implement the <b>Editable_Value_Holder</b>
   --  interface.
   type Length_Validator_Tag_Node is new Validator_Tag_Node with private;
   type Length_Validator_Tag_Node_Access is access all Length_Validator_Tag_Node'Class;

   --  Create the Length_Validator Tag.  Verifies that the XML node defines
   --  the <b>minimum</b> or the <b>maximum</b> or both attributes.
   function Create_Length_Validator_Tag_Node (Binding    : in Binding_Type;
                                              Line       : in Views.Line_Info;
                                              Parent     : in Views.Nodes.Tag_Node_Access;
                                              Attributes : in Nodes.Tag_Attribute_Array_Access)
                                              return Views.Nodes.Tag_Node_Access;

   --  Get the validator instance that corresponds to the validator tag.
   --  Returns in <b>Validator</b> the instance if it exists and indicate
   --  in <b>Shared</b> whether it must be freed or not when the component is deleted.
   overriding
   procedure Get_Validator (Node      : in Length_Validator_Tag_Node;
                            Context   : in out Contexts.Facelets.Facelet_Context'Class;
                            Validator : out Validators.Validator_Access;
                            Shared    : out Boolean);

   --  ------------------------------
   --  Regex Validator Tag
   --  ------------------------------
   --  The <b>Regex_Validator_Tag_Node</b> is created in the facelet tree when
   --  the <f:validateRegex> element is found.  When building the component tree,
   --  we have to find the <b>Validator</b> object and attach it to the
   --  parent component.  The parent component must implement the <b>Editable_Value_Holder</b>
   --  interface.
   type Regex_Validator_Tag_Node is new Validator_Tag_Node with private;
   type Regex_Validator_Tag_Node_Access is access all Regex_Validator_Tag_Node'Class;

   --  Create the Regex_Validator Tag.  Verifies that the XML node defines
   --  the <b>pattern</b> and that it is a valid regular expression.
   function Create_Regex_Validator_Tag_Node (Binding    : in Binding_Type;
                                             Line       : in Views.Line_Info;
                                             Parent     : in Views.Nodes.Tag_Node_Access;
                                             Attributes : in Nodes.Tag_Attribute_Array_Access)
                                            return Views.Nodes.Tag_Node_Access;

   --  Get the validator instance that corresponds to the validator tag.
   --  Returns in <b>Validator</b> the instance if it exists and indicate
   --  in <b>Shared</b> whether it must be freed or not when the component is deleted.
   overriding
   procedure Get_Validator (Node      : in Regex_Validator_Tag_Node;
                            Context   : in out Contexts.Facelets.Facelet_Context'Class;
                            Validator : out Validators.Validator_Access;
                            Shared    : out Boolean);

   --  ------------------------------
   --  Attribute Tag
   --  ------------------------------
   --  The <b>Attribute_Tag_Node</b> is created in the facelet tree when
   --  the <f:attribute> element is found.  When building the component tree,
   --  an attribute is added to the parent component.
   type Attribute_Tag_Node is new Views.Nodes.Tag_Node with private;
   type Attribute_Tag_Node_Access is access all Attribute_Tag_Node'Class;

   --  Create the Attribute Tag
   function Create_Attribute_Tag_Node (Binding    : in Binding_Type;
                                       Line       : in Views.Line_Info;
                                       Parent     : in Views.Nodes.Tag_Node_Access;
                                       Attributes : in Views.Nodes.Tag_Attribute_Array_Access)
                                       return Views.Nodes.Tag_Node_Access;

   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  Adds the attribute to the component node.
   --  This operation does not create any new UIComponent.
   overriding
   procedure Build_Components (Node    : access Attribute_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class);

   --  ------------------------------
   --  Facet Tag
   --  ------------------------------
   --  The <b>Facet_Tag_Node</b> is created in the facelet tree when
   --  the <f:facet> element is found.  After building the component tree,
   --  we have to add the component as a facet element of the parent component.
   --
   type Facet_Tag_Node is new Views.Nodes.Tag_Node with private;
   type Facet_Tag_Node_Access is access all Facet_Tag_Node'Class;

   --  Create the Facet Tag
   function Create_Facet_Tag_Node (Binding    : in Binding_Type;
                                   Line       : in Views.Line_Info;
                                   Parent     : in Views.Nodes.Tag_Node_Access;
                                   Attributes : in Views.Nodes.Tag_Attribute_Array_Access)
                                   return Views.Nodes.Tag_Node_Access;

   --  Build the component tree from the tag node and attach it as
   --  the facet component of the given parent.  Calls recursively the
   --  method to create children.
   overriding
   procedure Build_Components (Node    : access Facet_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class);

   --  ------------------------------
   --  Metadata Tag
   --  ------------------------------
   --  The <b>Metadata_Tag_Node</b> is created in the facelet tree when
   --  the <f:metadata> element is found.  This special component is inserted as a special
   --  facet component on the UIView parent component.
   type Metadata_Tag_Node is new Views.Nodes.Tag_Node with private;
   type Metadata_Tag_Node_Access is access all Metadata_Tag_Node'Class;

   --  Create the Metadata Tag
   function Create_Metadata_Tag_Node (Binding    : in Binding_Type;
                                      Line       : in Views.Line_Info;
                                      Parent     : in Views.Nodes.Tag_Node_Access;
                                      Attributes : in Views.Nodes.Tag_Attribute_Array_Access)
                                      return Views.Nodes.Tag_Node_Access;

   --  Build the component tree from the tag node and attach it as a metadata information
   --  facet for the UIView parent component.
   overriding
   procedure Build_Components (Node    : access Metadata_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class);

private

   type Converter_Tag_Node is new Views.Nodes.Tag_Node with record
      Converter : EL.Objects.Object;
   end record;

   type Convert_Date_Time_Tag_Node is new Views.Nodes.Tag_Node with record
      Date_Style : Tag_Attribute_Access;
      Time_Style : Tag_Attribute_Access;
      Locale     : Tag_Attribute_Access;
      Pattern    : Tag_Attribute_Access;
      Format     : Tag_Attribute_Access;
   end record;

   type Validator_Tag_Node is new Views.Nodes.Tag_Node with record
      Validator : EL.Objects.Object;
   end record;

   type Length_Validator_Tag_Node is new Validator_Tag_Node with record
      Minimum : Tag_Attribute_Access;
      Maximum : Tag_Attribute_Access;
   end record;

   type Regex_Validator_Tag_Node is new Validator_Tag_Node with record
      Pattern : Tag_Attribute_Access;
   end record;

   type Range_Validator_Tag_Node is new Validator_Tag_Node with record
      Minimum : Tag_Attribute_Access;
      Maximum : Tag_Attribute_Access;
   end record;

   type Attribute_Tag_Node is new Views.Nodes.Tag_Node with record
      Attr       : aliased Tag_Attribute;
      Attr_Name  : Tag_Attribute_Access;
      Value      : Tag_Attribute_Access;
   end record;

   type Facet_Tag_Node is new Views.Nodes.Tag_Node with record
      Facet_Name : Tag_Attribute_Access;
   end record;

   type Metadata_Tag_Node is new Views.Nodes.Tag_Node with null record;

end ASF.Views.Nodes.Jsf;
