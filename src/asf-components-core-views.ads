-----------------------------------------------------------------------
--  components-core-views -- ASF View Components
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

with ASF.Contexts.Faces;
with ASF.Events.Faces;
with ASF.Lifecycles;
with Util.Beans.Objects;
with ASF.Components.Html.Forms;
with ASF.Components.Core;
with ASF.Views.Nodes;
private with Ada.Containers.Vectors;
package ASF.Components.Core.Views is

   use ASF.Contexts.Faces;

   --  Name of the facet that holds the metadata information
   --  (we use the same name as JSF 2 specification).
   METADATA_FACET_NAME : constant String := "javax_faces_metadata";

   --  ------------------------------
   --  View component
   --  ------------------------------
   type UIView is new Core.UIComponentBase with private;
   type UIView_Access is access all UIView'Class;

   --  Get the content type returned by the view.
   function Get_Content_Type (UI      : in UIView;
                              Context : in Faces_Context'Class) return String;

   --  Set the content type returned by the view.
   procedure Set_Content_Type (UI     : in out UIView;
                               Value  : in String);

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
   --  View Parameter Component
   --  ------------------------------
   --  The <b>UIViewParameter</b> component represents a request parameter that must be mapped
   --  to a backed bean object.  This component does not participate in the rendering.
   type UIViewParameter is new Html.Forms.UIInput with private;
   type UIViewParameter_Access is access all UIViewParameter'Class;

   --  ------------------------------
   --  View Metadata Component
   --  ------------------------------
   --  The <b>UIViewMetaData</b> component defines the view meta data components.
   --  These components defines how to handle some request parameters for a GET request
   --  as well as some actions that must be made upon reception of a request.
   --
   --  From ASF lifecycle management, if the request is a GET, this component is used
   --  as the root of the component tree.  The APPLY_REQUESTS .. INVOKE_APPLICATION actions
   --  are called on that component tree.  It is also used for the RENDER_RESPONSE, and
   --  we have to propagate the rendering on the real view root.  Therefore, the Encode_XXX
   --  operations are overriden to propagate on the real root.
   type UIViewMetaData is new UIView with private;
   type UIViewMetaData_Access is access all UIViewMetaData'Class;

   --  Start encoding the UIComponent.
   overriding
   procedure Encode_Begin (UI      : in UIViewMetaData;
                           Context : in out Faces_Context'Class);

   --  Encode the children of this component.
   overriding
   procedure Encode_Children (UI      : in UIViewMetaData;
                              Context : in out Faces_Context'Class);

   --  Finish encoding the component.
   overriding
   procedure Encode_End (UI      : in UIViewMetaData;
                         Context : in out Faces_Context'Class);

   --  Set the metadata facet on the UIView component.
   procedure Set_Metadata (UI    : in out UIView;
                           Meta  : in UIViewMetaData_Access;
                           Tag   : access ASF.Views.Nodes.Tag_Node'Class);

private

   use ASF.Lifecycles;

   type Faces_Event_Access is access all ASF.Events.Faces.Faces_Event'Class;

   package Event_Vectors is new Ada.Containers.Vectors (Index_Type   => Natural,
                                                        Element_Type => Faces_Event_Access);

   type Event_Queues is array (Phase_Type) of Event_Vectors.Vector;

   type UIView is new Core.UIComponentBase with record
      Content_Type : Util.Beans.Objects.Object;
      Phase_Events : Event_Queues;
      Meta         : UIViewMetaData_Access := null;
   end record;

   type UIViewParameter is new Html.Forms.UIInput with record
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type UIViewMetaData is new UIView with record
      Root : UIView_Access := null;
   end record;

end ASF.Components.Core.Views;