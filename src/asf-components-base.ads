-----------------------------------------------------------------------
--  components -- Component tree
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2018, 2020, 2022 Stephane Carrez
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

with Ada.Finalization;
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

with EL.Objects;
with Util.Beans.Objects;
with EL.Expressions;
with ASF.Contexts.Faces;
with ASF.Utils;
limited with ASF.Events.Faces;
package ASF.Components.Base is

   use ASF.Contexts.Faces;
   type UIComponent_List is private;

   --  ------------------------------
   --  Attribute of a component
   --  ------------------------------

   --     type UIComponent is new Ada.Finalization.Controlled with private;
   type UIComponent is new Ada.Finalization.Limited_Controlled with private;
   type UIComponent_Access is access all UIComponent'Class;

   --  Get the parent component.
   --  Returns null if the node is the root element.
   function Get_Parent (UI : UIComponent) return UIComponent_Access;

   --  Return a client-side identifier for this component, generating
   --  one if necessary.
   function Get_Client_Id (UI : UIComponent) return Unbounded_String;

   --  Returns True if the client-side identifier was generated automatically.
   function Is_Generated_Id (UI : in UIComponent) return Boolean;

   --  Returns True if the component has a client-side identifier matching the given name.
   function Is_Client_Id (UI : in UIComponent;
                          Id : in String) return Boolean;

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
   --   o look first in the sub-tree representing the parent node.
   --   o if not found, move to the parent's node
   --  Returns null if the component was not found in the view.
   function Find (UI : in UIComponent;
                  Id : in String) return UIComponent_Access;

   --  Search within the component tree for the {@link UIComponent} with
   --  an <code>id</code> that matches the specified search expression.
   function Find_Child (UI : in UIComponent'Class;
                        Id : in String) return UIComponent_Access;

   --  Get the number of facets that this component contains.
   function Get_Facet_Count (UI : in UIComponent'Class) return Natural;

   --  Get the facet identified by the given name.
   --  Returns null if there is no such facet.
   function Get_Facet (UI   : in UIComponent'Class;
                       Name : in String) return UIComponent_Access;

   --  Add the facet represented by the root component <b>Facet</b> under the name <b>Name</b>.
   --  The facet component is added to the facet map and it get be retrieved later on by
   --  using the <b>Get_Facet</b> operation.  The facet component will be destroyed when this
   --  component is deleted.
   procedure Add_Facet (UI    : in out UIComponent'Class;
                        Name  : in String;
                        Facet : in UIComponent_Access;
                        Tag   : access ASF.Views.Nodes.Tag_Node'Class);

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

   --  Get the attribute value as a boolean.
   --  If the attribute does not exist, returns the default.
   function Get_Attribute (UI      : in UIComponent;
                           Name    : in String;
                           Context : in Faces_Context'Class;
                           Default : in Boolean := False) return Boolean;

   --  Get the attribute value as an integer.
   --  If the attribute does not exist, returns the default.
   function Get_Attribute (UI      : in UIComponent;
                           Name    : in String;
                           Context : in Faces_Context'Class;
                           Default : in Integer := 0) return Integer;

   --  Get the attribute value as a string.
   --  If the attribute does not exist, returns the default.
   function Get_Attribute (UI      : in UIComponent;
                           Name    : in String;
                           Context : in Faces_Context'Class;
                           Default : in String := "") return String;

   procedure Set_Attribute (UI    : in out UIComponent;
                            Name  : in String;
                            Value : in EL.Objects.Object);

   procedure Set_Attribute (UI    : in out UIComponent;
                            Def   : access ASF.Views.Nodes.Tag_Attribute;
                            Value : in EL.Expressions.Expression);

   procedure Set_Attribute (UI    : in out UIComponent;
                            Def   : access ASF.Views.Nodes.Tag_Attribute;
                            Value : in EL.Objects.Object);

   --  Get the <b>label</b> attribute from the component.  If the attribute is
   --  empty, returns the client id.
   function Get_Label (UI      : in UIComponent'Class;
                       Context : in Faces_Context'Class) return Util.Beans.Objects.Object;

   --  Get the expression
   function Get_Expression (UI   : in UIComponent;
                            Name : in String) return EL.Expressions.Expression;

   --  Get the value expression
   function Get_Value_Expression (UI   : in UIComponent;
                                  Name : in String) return EL.Expressions.Value_Expression;

   --  Get the method expression
   --  Raise an Invalid_Expression if the method expression is invalid.
   function Get_Method_Expression (UI   : in UIComponent;
                                   Name : in String) return EL.Expressions.Method_Expression;

   --  Add a message for the component.  Look for the message attribute identified
   --  by <b>Name</b> on the <b>UI</b> component.  Add this message in the faces context
   --  and associated with the component client id.  Otherwise, add the default
   --  message whose bundle key is identified by <b>default</b>.
   procedure Add_Message (UI      : in UIComponent'Class;
                          Name    : in String;
                          Default : in String;
                          Context : in out Faces_Context'Class);

   --  Add a message for the component.  Look for the message attribute identified
   --  by <b>Name</b> on the <b>UI</b> component.  Add this message in the faces context
   --  and associated with the component client id.  Otherwise, add the default
   --  message whose bundle key is identified by <b>default</b>.
   procedure Add_Message (UI      : in UIComponent'Class;
                          Name    : in String;
                          Default : in String;
                          Arg1    : in Util.Beans.Objects.Object;
                          Context : in out Faces_Context'Class);

   --  Add a message for the component.  Look for the message attribute identified
   --  by <b>Name</b> on the <b>UI</b> component.  Add this message in the faces context
   --  and associated with the component client id.  Otherwise, add the default
   --  message whose bundle key is identified by <b>default</b>.
   procedure Add_Message (UI      : in UIComponent'Class;
                          Name    : in String;
                          Default : in String;
                          Arg1    : in Util.Beans.Objects.Object;
                          Arg2    : in Util.Beans.Objects.Object;
                          Context : in out Faces_Context'Class);

   --  Add a message for the component.  Look for the message attribute identified
   --  by <b>Name</b> on the <b>UI</b> component.  Add this message in the faces context
   --  and associated with the component client id.  Otherwise, use the default
   --  message whose bundle key is identified by <b>default</b>.  The message is
   --  formatted with the arguments passed in <b>Args</b>.
   procedure Add_Message (UI      : in UIComponent'Class;
                          Name    : in String;
                          Default : in String;
                          Args    : in ASF.Utils.Object_Array;
                          Context : in out Faces_Context'Class);

   procedure Encode_Begin (UI      : in UIComponent;
                           Context : in out Faces_Context'Class);

   procedure Encode_Children (UI      : in UIComponent;
                              Context : in out Faces_Context'Class);

   --  Encode the children components in a local buffer and after the rendering execute
   --  the <b>Process</b> procedure with the generated content.
   --  If this component is not rendered, do nothing.
   procedure Wrap_Encode_Children (UI      : in UIComponent;
                                   Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                                   Process : not null
                                   access procedure (Content : in Unbounded_String));
   procedure Wrap_Encode_Children (UI      : in UIComponent;
                                   Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                                   Process : not null access procedure (Content : in String));

   procedure Encode_End (UI      : in UIComponent;
                         Context : in out Faces_Context'Class);

   procedure Encode_All (UI      : in UIComponent'Class;
                         Context : in out Faces_Context'Class);

   procedure Decode (UI      : in out UIComponent;
                     Context : in out Faces_Context'Class);

   procedure Decode_Children (UI      : in UIComponent'Class;
                              Context : in out Faces_Context'Class);

   --  Decode any new state of the specified component from the request contained
   --  in the specified context and store that state on the component.
   --
   --  During decoding, events may be enqueued for later processing
   --  (by event listeners that have registered an interest), by calling
   --  the <b>Queue_Event</b> on the associated component.
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

   --  Perform the component tree processing required by the <b>Update Model Values</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows.
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Updates/b> of all facets and children.
   --  <ul>
   procedure Process_Updates (UI      : in out UIComponent;
                              Context : in out Faces_Context'Class);

   --  Queue an event for broadcast at the end of the current request
   --  processing lifecycle phase.  The default implementation in
   --  delegates this call to the parent component.  The <b>UIViewRoot</b>
   --  component is in charge of queueing events.  The event object
   --  will be freed after being dispatched.
   procedure Queue_Event (UI    : in out UIComponent;
                          Event : not null access ASF.Events.Faces.Faces_Event'Class);

   --  Broadcast the event to the event listeners installed on this component.
   --  Listeners are called in the order in which they were added.
   procedure Broadcast (UI      : in out UIComponent;
                        Event   : not null access ASF.Events.Faces.Faces_Event'Class;
                        Context : in out Faces_Context'Class);

   --  Finalize the object.
   overriding
   procedure Finalize (UI : in out UIComponent);

   type UIComponent_Array is array (Natural range <>) of UIComponent_Access;

   type UIComponent_Array_Access is access UIComponent_Array;

   --  type UIOutput is new UIComponent;

   function Get_Context (UI : in UIComponent)
                         return ASF.Contexts.Faces.Faces_Context_Access;

   --  Get the attribute value.
   function Get_Value (Attr : UIAttribute;
                       UI   : UIComponent'Class) return EL.Objects.Object;

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

   --  Report an error message in the logs caused by an invalid configuration or
   --  setting on the component.
   procedure Log_Error (UI      : in UIComponent'Class;
                        Message : in String;
                        Arg1    : in String := "";
                        Arg2    : in String := "";
                        Arg3    : in String := "");

   --  Get the root component from the <b>UI</b> component tree.
   --  After the operation, the <b>UI</b> component tree will contain no
   --  nodes.
   --  If the <b>Root</b> pointer is not null, first deletes recursively
   --  the component tree.
   procedure Steal_Root_Component (UI   : in out UIComponent'Class;
                                   Root : in out UIComponent_Access);

   --  ------------------------------
   --  Iterator over the component children
   --  ------------------------------
   type Cursor is private;

   --  Get an iterator to scan the component children.
   function First (UI : in UIComponent'Class) return Cursor;

   --  Returns True if the iterator points to a valid child.
   function Has_Element (Pos : in Cursor) return Boolean;

   --  Get the child component pointed to by the iterator.
   function Element (Pos : in Cursor) return UIComponent_Access;

   --  Move to the next child.
   procedure Next (Pos : in out Cursor);

private

   type UIComponent_List is record
      Child : UIComponent_Access := null;
   end record;

   --  The facet map maintains a mapping between a facet name and a component tree.
   --  The component tree is not directly visible (ie, it does not participate in the
   --  JSF component traversal by default).  To be taken into account, the component has
   --  to retrieve the facet and deal with it.
   --
   --  The facet map is not created by default and it is allocated only when a facet is
   --  added to the component.  By doing this, we avoid the cost of holding a Hashed_Map
   --  instance (even empty) in each UIComponent.F
   package Component_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                 Element_Type    => UIComponent_Access,
                                                 Hash            => Ada.Strings.Hash,
                                                 Equivalent_Keys => "=",
                                                 "="             => "=");

   type Component_Map_Access is access Component_Maps.Map;

   type UIComponent is new Ada.Finalization.Limited_Controlled with record
      Id           : Unbounded_String;
      Id_Generated : Boolean := False;
      Tag          : access ASF.Views.Nodes.Tag_Node'Class;
      Parent       : UIComponent_Access := null;
      First_Child  : UIComponent_Access := null;
      Last_Child   : UIComponent_Access := null;
      Next         : UIComponent_Access := null;
      Attributes   : UIAttribute_Access := null;
      Facets       : Component_Map_Access := null;
   end record;

   type Cursor is record
      Child : UIComponent_Access := null;
   end record;

end ASF.Components.Base;
