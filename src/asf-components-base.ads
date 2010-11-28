-----------------------------------------------------------------------
--  components -- Component tree
--  Copyright (C) 2009, 2010 Stephane Carrez
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

--  The <bASF.Components</b> describes the components that form the
--  tree view.  Each component has attributes and children.  Children
--  represent sub-components and attributes control the rendering and
--  behavior of the component.
--
--  The component tree is created from the <b>ASF.Views</b> tag nodes
--  for each request.  Unlike tag nodes, the component tree is not shared.
with Ada.Strings.Unbounded;
with EL.Objects;
with EL.Expressions;
with ASF.Contexts.Faces;
limited with ASF.Views.Nodes;
limited with ASF.Converters;
package ASF.Components.Base is

   use Ada.Strings.Unbounded;
   use ASF.Contexts.Faces;
   type UIComponent_List is private;

   --  ------------------------------
   --  Attribute of a component
   --  ------------------------------

   --     type UIComponent is new Ada.Finalization.Controlled with private;
   type UIComponent is tagged limited private;
   type UIComponent_Access is access all UIComponent'Class;

   --  Get the parent component.
   --  Returns null if the node is the root element.
   function Get_Parent (UI : UIComponent) return UIComponent_Access;

   --  Return a client-side identifier for this component, generating
   --  one if necessary.
   function Get_Client_Id (UI : UIComponent) return Unbounded_String;

   --  Get the list of children.
   function Get_Children (UI : UIComponent) return UIComponent_List;

   --  Get the number of children.
   function Get_Children_Count (UI : UIComponent) return Natural;

   --  Get the first child component.
   --  Returns null if the component has no children.
   function Get_First_Child (UI : UIComponent) return UIComponent_Access;

   --  Get the tag node that created this component.
   function Get_Tag (UI : in UIComponent'Class) return access ASF.Views.Nodes.Tag_Node'Class;

   --  Initialize the component when restoring the view.
   --  The default initialization gets the client ID and allocates it if necessary.
   procedure Initialize (UI      : in out UIComponent;
                         Context : in out Faces_Context'Class);

   procedure Append (UI    : in UIComponent_Access;
                     Child : in UIComponent_Access;
                     Tag   : access ASF.Views.Nodes.Tag_Node'Class);

   --  Search for and return the {@link UIComponent} with an <code>id</code>
   --  that matches the specified search expression (if any), according to
   --  the algorithm described below.
   function Find (UI : UIComponent;
                  Name : String) return UIComponent_Access;

   --  Check whether the component and its children must be rendered.
   function Is_Rendered (UI : UIComponent;
                         Context : Faces_Context'Class) return Boolean;

   --  Set whether the component is rendered.
   procedure Set_Rendered (UI       : in out UIComponent;
                           Rendered : in Boolean);


   function Get_Attribute (UI      : UIComponent;
                           Context : Faces_Context'Class;
                           Name    : String) return EL.Objects.Object;

   --  Get the attribute tag
   function Get_Attribute (UI      : UIComponent;
                           Name    : String) return access ASF.Views.Nodes.Tag_Attribute;

   procedure Set_Attribute (UI    : in out UIComponent;
                            Name  : in String;
                            Value : in EL.Objects.Object);

   procedure Set_Attribute (UI    : in out UIComponent;
                            Def   : access ASF.Views.Nodes.Tag_Attribute;
                            Value : in EL.Expressions.Expression);

   procedure Set_Attribute (UI    : in out UIComponent;
                            Def   : access ASF.Views.Nodes.Tag_Attribute;
                            Value : in EL.Objects.Object);

   --  Get the converter associated with the component
   function Get_Converter (UI      : in UIComponent;
                           Context : in Faces_Context'Class)
                           return access ASF.Converters.Converter'Class;

   --  Convert the string into a value.  If a converter is specified on the component,
   --  use it to convert the value.
   function Convert_Value (UI      : in UIComponent;
                           Value   : in String;
                           Context : in Faces_Context'Class) return EL.Objects.Object;

   --  Get the value expression
   function Get_Value_Expression (UI   : in UIComponent;
                                  Name : in String) return EL.Expressions.Value_Expression;

   procedure Encode_Begin (UI      : in UIComponent;
                           Context : in out Faces_Context'Class);

   procedure Encode_Children (UI      : in UIComponent;
                              Context : in out Faces_Context'Class);

   procedure Encode_End (UI      : in UIComponent;
                         Context : in out Faces_Context'Class);

   procedure Encode_All (UI      : in UIComponent'Class;
                         Context : in out Faces_Context'Class);

   procedure Decode (UI      : in out UIComponent;
                     Context : in out Faces_Context'Class);

   procedure Decode_Children (UI      : in UIComponent'Class;
                              Context : in out Faces_Context'Class);

   procedure Process_Decodes (UI      : in out UIComponent;
                              Context : in out Faces_Context'Class);

   --  Perform the component tree processing required by the <b>Process Validations</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows:
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Validators</b> of all facets and children.
   --  <ul>
   procedure Process_Validators (UI      : in out UIComponent;
                                 Context : in out Faces_Context'Class);

   procedure Process_Updates (UI      : in out UIComponent;
                              Context : in out Faces_Context'Class);

   type UIComponent_Array is array (Natural range <>) of UIComponent_Access;

   type UIComponent_Array_Access is access UIComponent_Array;

   --  type UIOutput is new UIComponent;

   function Get_Context (UI : in UIComponent)
                         return ASF.Contexts.Faces.Faces_Context_Access;

   --  Get the attribute value.
   function Get_Value (Attr : UIAttribute;
                       UI   : UIComponent'Class) return EL.Objects.Object;

   --   function Create_UIComponent (Parent  : UIComponent_Access;
   --                                Context : ASF.Contexts.Facelets.Facelet_Context'Class;
   --                                Tag    : access ASF.Views.Nodes.Tag_Node'Class)
   --                                return UIComponent_Access;

   --  Iterate over the children of the component and execute
   --  the <b>Process</b> procedure.
   generic
      with procedure Process (Child   : in UIComponent_Access);
   procedure Iterate (UI : in UIComponent'Class);

   --  Iterate over the attributes defined on the component and
   --  execute the <b>Process</b> procedure.
   generic
      with procedure Process (Name : in String;
                              Attr : in UIAttribute);
   procedure Iterate_Attributes (UI : in UIComponent'Class);

   --  Get the root component from the <b>UI</b> component tree.
   --  After the operation, the <b>UI</b> component tree will contain no
   --  nodes.
   --  If the <b>Root</b> pointer is not null, first deletes recursively
   --  the component tree.
   procedure Steal_Root_Component (UI   : in out UIComponent'Class;
                                   Root : in out UIComponent_Access);

private

   --  Delete the component tree recursively.
   procedure Delete (UI : in out UIComponent_Access);

   type UIComponent_List is record
      Child : UIComponent_Access := null;
   end record;

   type UIComponent is tagged limited record
      Id          : Unbounded_String;
      Tag         : access ASF.Views.Nodes.Tag_Node'Class;
      Parent      : UIComponent_Access;
      First_Child : UIComponent_Access;
      Last_Child  : UIComponent_Access;
      Next        : UIComponent_Access;
      Attributes  : UIAttribute_Access;
   end record;

end ASF.Components.Base;
