-----------------------------------------------------------------------
--  components-core -- ASF Core Components
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
with ASF.Events.Faces;
with ASF.Lifecycles;
private with Ada.Containers.Vectors;
package ASF.Components.Core is

   use ASF.Contexts.Faces;

   --     pragma Elaborate_Body;

   type UIComponentBase is new Base.UIComponent with null record;

   --  Return a client-side identifier for this component, generating
   --  one if necessary.
   overriding
   function Get_Client_Id (UI : UIComponentBase) return Unbounded_String;

   --  ------------------------------
   --  View component
   --  ------------------------------
   type UIView is new UIComponentBase with private;
   type UIView_Access is access all UIView'Class;

   --  Encode the begining of the view.  Set the response content type.
   overriding
   procedure Encode_Begin (UI      : in UIView;
                           Context : in out Faces_Context'Class);

   --  Decode any new state of the specified component from the request contained
   --  in the specified context and store that state on the component.
   --
   --  During decoding, events may be enqueued for later processing
   --  (by event listeners that have registered an interest), by calling
   --  the <b>Queue_Event</b> on the associated component.
   overriding
   procedure Process_Decodes (UI      : in out UIView;
                              Context : in out Faces_Context'Class);

   --  Perform the component tree processing required by the <b>Process Validations</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows:
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Validators</b> of all facets and children.
   --  <ul>
   overriding
   procedure Process_Validators (UI      : in out UIView;
                                 Context : in out Faces_Context'Class);

   --  Perform the component tree processing required by the <b>Update Model Values</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows.
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Updates/b> of all facets and children.
   --  <ul>
   overriding
   procedure Process_Updates (UI      : in out UIView;
                              Context : in out Faces_Context'Class);

   --  Broadcast any events that have been queued for the <b>Invoke Application</b>
   --  phase of the request processing lifecycle and to clear out any events
   --  for later phases if the event processing for this phase caused
   --  <b>renderResponse</b> or <b>responseComplete</b> to be called.
   procedure Process_Application (UI      : in out UIView;
                                  Context : in out Faces_Context'Class);

   --  Queue an event for broadcast at the end of the current request
   --  processing lifecycle phase.  The event object
   --  will be freed after being dispatched.
   procedure Queue_Event (UI    : in out UIView;
                          Event : not null access ASF.Events.Faces.Faces_Event'Class);

   --  Broadcast the events after the specified lifecycle phase.
   --  Events that were queued will be freed.
   procedure Broadcast (UI      : in out UIView;
                        Phase   : in ASF.Lifecycles.Phase_Type;
                        Context : in out Faces_Context'Class);

   --  Clear the events that were queued.
   procedure Clear_Events (UI : in out UIView);

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
                              Context : in out Faces_Context'Class);

   overriding
   procedure Encode_Begin (UI      : in UILeaf;
                           Context : in out Faces_Context'Class);

   overriding
   procedure Encode_End (UI      : in UILeaf;
                         Context : in out Faces_Context'Class);

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

   use ASF.Lifecycles;

   type Faces_Event_Access is access all ASF.Events.Faces.Faces_Event'Class;

   package Event_Vectors is new Ada.Containers.Vectors (Index_Type   => Natural,
                                                        Element_Type => Faces_Event_Access);

   type Event_Queues is array (Phase_Type) of Event_Vectors.Vector;

   type UIView is new UIComponentBase with record
      Phase_Events : Event_Queues;
   end record;

   type UIText is new Base.UIComponent with record
      Text       : ASF.Views.Nodes.Text_Tag_Node_Access;
      Expr_Table : ASF.Views.Nodes.Expression_Access_Array_Access := null;
   end record;

   type UILeaf is new UIComponentBase with null record;

   type UIParameter is new UILeaf with record
      N : Natural;
   end record;

end ASF.Components.Core;
