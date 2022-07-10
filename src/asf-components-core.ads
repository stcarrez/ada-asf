-----------------------------------------------------------------------
--  asf-components-core -- ASF Core Components
--  Copyright (C) 2009 - 2022 Stephane Carrez
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
with ASF.Contexts.Faces;

--  = Core Components =
--  The facelets is the default view declaration language that uses XML and XHTML.
--  It is a composition and templating framework that allows to create the component
--  tree.
--
--  The core components are defined in the following namespace:
--  ```
--  xmlns:f="http://java.sun.com/jsf/core"
--  ```
--
--  The core components are implemented by the `ASF.Components.Core`
--  package which defines the `UIComponent` that describes the various elements
--  provided by the core components.  These components are instantiated when the
--  view is created from the facelet tree that was read from the XHTML view description.
--
--  @include-doc docs/comp-jsf-core/*.txt
package ASF.Components.Core is

   use ASF.Contexts.Faces;

   type UIComponentBase is new Base.UIComponent with null record;

   --  Return a client-side identifier for this component, generating
   --  one if necessary.
   overriding
   function Get_Client_Id (UI : UIComponentBase) return Unbounded_String;

   --  ------------------------------
   --  Raw text component
   --  ------------------------------
   --  The <b>UIText</b> component represents a raw text to be written on the output stream.
   --  This text is also composed of EL expressions to evaluate.  The text and EL nodes
   --  are represented by <b>ASF.Views.Nodes.Text_Tag_Node</b> and is therefore shared
   --  among requests.
   type UIText is new Base.UIComponent with private;
   type UIText_Access is access all UIText'Class;

   --  Renders the UIText evaluating the EL expressions it may contain.
   overriding
   procedure Encode_Begin (UI      : in UIText;
                           Context : in out Faces_Context'Class);

   --  Set the expression array that contains reduced expressions.
   procedure Set_Expression_Table (UI         : in out UIText;
                                   Expr_Table : in ASF.Views.Nodes.Expression_Access_Array_Access);

   --  Finalize the object.
   overriding
   procedure Finalize (UI : in out UIText);

   function Create_UIText (Tag : in ASF.Views.Nodes.Text_Tag_Node_Access)
                           return UIText_Access;

   --  ------------------------------
   --  Abstract Leaf component
   --  ------------------------------
   --  The <b>UILeaf</b> component is an abstract component intended to be used
   --  for components without children.
   type UILeaf is new UIComponentBase with private;

   overriding
   procedure Encode_Children (UI      : in UILeaf;
                              Context : in out Faces_Context'Class) is null;

   overriding
   procedure Encode_Begin (UI      : in UILeaf;
                           Context : in out Faces_Context'Class) is null;

   overriding
   procedure Encode_End (UI      : in UILeaf;
                         Context : in out Faces_Context'Class) is null;

   --  ------------------------------
   --  Component Parameter
   --  ------------------------------
   type UIParameter is new UILeaf with private;
   type UIParameter_Access is access all UIParameter'Class;

   type UIParameter_Access_Array is array (Natural range <>) of UIParameter_Access;

   --  Get the parameter name
   function Get_Name (UI      : UIParameter;
                      Context : Faces_Context'Class) return String;

   --  Get the parameter value
   function Get_Value (UI      : UIParameter;
                       Context : Faces_Context'Class) return EL.Objects.Object;

   --  Get the list of parameters associated with a component.
   function Get_Parameters (UI : Base.UIComponent'Class) return UIParameter_Access_Array;

private

   type UIText is new Base.UIComponent with record
      Text       : ASF.Views.Nodes.Text_Tag_Node_Access;
      Expr_Table : ASF.Views.Nodes.Expression_Access_Array_Access := null;
   end record;

   type UILeaf is new UIComponentBase with null record;

   type UIParameter is new UILeaf with record
      N : Natural;
   end record;

end ASF.Components.Core;
