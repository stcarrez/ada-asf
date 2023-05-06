-----------------------------------------------------------------------
--  asf-components -- Component tree
--  Copyright (C) 2009, 2010, 2011, 2023 Stephane Carrez
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

--  == Components ==
--  The <bASF.Components</b> describes the components that form the
--  tree view.  Each component has attributes and children.  Children
--  represent sub-components and attributes control the rendering and
--  behavior of the component.
--
--  The component tree is created from the <b>ASF.Views</b> tag nodes
--  for each request.  Unlike tag nodes, the component tree is not shared.
--
with Ada.Strings.Unbounded;
with EL.Objects;
with EL.Expressions;
limited with ASF.Views.Nodes;
package ASF.Components is

   use Ada.Strings.Unbounded;

   --  Flag indicating whether or not this component should be rendered
   --  (during Render Response Phase), or processed on any subsequent form submit.
   --  The default value for this property is true.
   RENDERED_NAME          : constant String := "rendered";

   --  A localized user presentable name for the component.
   LABEL_NAME             : constant String := "label";

   --  Converter instance registered with the component.
   CONVERTER_NAME         : constant String := "converter";

   --  A ValueExpression enabled attribute that, if present, will be used as the text
   --  of the converter message, replacing any message that comes from the converter.
   CONVERTER_MESSAGE_NAME : constant String := "converterMessage";

   --  A ValueExpression enabled attribute that, if present, will be used as the text
   --  of the validator message, replacing any message that comes from the validator.
   VALIDATOR_MESSAGE_NAME : constant String := "validatorMessage";

   --  Flag indicating that the user is required to provide a submitted value for
   --  the input component.
   REQUIRED_NAME          : constant String := "required";

   --  A ValueExpression enabled attribute that, if present, will be used as the
   --  text of the validation message for the "required" facility, if the "required"
   --  facility is used.
   REQUIRED_MESSAGE_NAME  : constant String := "requiredMessage";

   --  A ValueExpression enabled attribute that, if present, will be used as the
   --  text of the expiration message when the form CSRF token has expired.
   EXPIRED_MESSAGE_NAME   : constant String := "expiredMessage";

   --  The current value of the component.
   VALUE_NAME             : constant String := "value";

   ACTION_NAME            : constant String := "action";

   --  ------------------------------
   --  Attribute of a component
   --  ------------------------------
   type UIAttribute is private;

private

   type UIAttribute_Access is access all UIAttribute;

   type UIAttribute is record
      Definition : access ASF.Views.Nodes.Tag_Attribute;
      Name       : Unbounded_String;
      Value      : EL.Objects.Object;
      Expr       : EL.Expressions.Expression;
      Next_Attr  : UIAttribute_Access;
   end record;

end ASF.Components;
