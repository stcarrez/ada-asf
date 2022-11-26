-----------------------------------------------------------------------
--  components-core-views -- ASF View Components
--  Copyright (C) 2009, 2010, 2011, 2012, 2019, 2022 Stephane Carrez
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
with Ada.Unchecked_Deallocation;

with ASF.Events.Phases;
with ASF.Components.Base;
with ASF.Events.Faces.Actions;
with ASF.Applications.Main;

package body ASF.Components.Core.Views is

   use type Base.UIComponent_Access;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => ASF.Events.Faces.Faces_Event'Class,
                                     Name   => Faces_Event_Access);

   --  ------------------------------
   --  Get the content type returned by the view.
   --  ------------------------------
   function Get_Content_Type (UI      : in UIView;
                              Context : in Faces_Context'Class) return String is
   begin
      if Util.Beans.Objects.Is_Null (UI.Content_Type) then
         return UI.Get_Attribute (Name => "contentType", Context => Context);
      else
         return Util.Beans.Objects.To_String (UI.Content_Type);
      end if;
   end Get_Content_Type;

   --  ------------------------------
   --  Set the content type returned by the view.
   --  ------------------------------
   procedure Set_Content_Type (UI     : in out UIView;
                               Value  : in String) is
   begin
      UI.Content_Type := Util.Beans.Objects.To_Object (Value);
   end Set_Content_Type;

   --  ------------------------------
   --  Get the locale to be used when rendering messages in the view.
   --  If a locale was set explicitly, return it.
   --  If the view component defines a <b>locale</b> attribute, evaluate and return its value.
   --  If the locale is empty, calculate the locale by using the request context and the view
   --  handler.
   --  ------------------------------
   function Get_Locale (UI      : in UIView;
                        Context : in Faces_Context'Class) return Util.Locales.Locale is
      use type Util.Locales.Locale;
   begin
      if UI.Locale /= Util.Locales.NULL_LOCALE then
         return UI.Locale;
      end if;
      declare
         Value : constant Util.Beans.Objects.Object := UI.Get_Attribute (Name    => "locale",
                                                                         Context => Context);
      begin
         --  If the root view does not specify any locale, calculate it from the request.
         if Util.Beans.Objects.Is_Null (Value) then
            return Context.Get_Application.Get_View_Handler.Calculate_Locale (Context);
         end if;

         --  Resolve the locale.  If it is not valid, calculate it from the request.
         declare
            Name   : constant String := Util.Beans.Objects.To_String (Value);
            Locale : constant Util.Locales.Locale := Util.Locales.Get_Locale (Name);
         begin
            if Locale /= Util.Locales.NULL_LOCALE then
               return Locale;
            else
               return Context.Get_Application.Get_View_Handler.Calculate_Locale (Context);
            end if;
         end;
      end;
   end Get_Locale;

   --  ------------------------------
   --  Set the locale to be used when rendering messages in the view.
   --  ------------------------------
   procedure Set_Locale (UI     : in out UIView;
                         Locale : in Util.Locales.Locale) is
   begin
      UI.Locale := Locale;
   end Set_Locale;

   --  ------------------------------
   --  Encode the beginning of the view.  Set the response content type.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIView;
                           Context : in out Faces_Context'Class) is
      Content_Type : constant String := UI.Get_Content_Type (Context => Context);
   begin
      Context.Get_Response.Set_Content_Type (Content_Type);
      if UI.Left_Tree /= null then
         UI.Left_Tree.Encode_All (Context);
      end if;
   end Encode_Begin;

   --  ------------------------------
   --  Encode the end of the view.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIView;
                         Context : in out Faces_Context'Class) is
   begin
      if UI.Right_Tree /= null then
         UI.Right_Tree.Encode_All (Context);
      end if;
   end Encode_End;

   --  ------------------------------
   --  Decode any new state of the specified component from the request contained
   --  in the specified context and store that state on the component.
   --
   --  During decoding, events may be enqueued for later processing
   --  (by event listeners that have registered an interest), by calling
   --  the <b>Queue_Event</b> on the associated component.
   --  ------------------------------
   overriding
   procedure Process_Decodes (UI      : in out UIView;
                              Context : in out Faces_Context'Class) is
   begin
      Base.UIComponent (UI).Process_Decodes (Context);

      --  Dispatch events queued for this phase.
      UIView'Class (UI).Broadcast (ASF.Events.Phases.APPLY_REQUEST_VALUES, Context);

      --  Drop other events if the response is to be returned.
      if Context.Get_Render_Response or else Context.Get_Response_Completed then
         UIView'Class (UI).Clear_Events;
      end if;
   end Process_Decodes;

   --  ------------------------------
   --  Perform the component tree processing required by the <b>Process Validations</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows:
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Validators</b> of all facets and children.
   --  <ul>
   --  ------------------------------
   overriding
   procedure Process_Validators (UI      : in out UIView;
                                 Context : in out Faces_Context'Class) is
   begin
      Base.UIComponent (UI).Process_Validators (Context);

      --  Dispatch events queued for this phase.
      UIView'Class (UI).Broadcast (ASF.Events.Phases.PROCESS_VALIDATION, Context);

      --  Drop other events if the response is to be returned.
      if Context.Get_Render_Response or else Context.Get_Response_Completed then
         UIView'Class (UI).Clear_Events;
      end if;
   end Process_Validators;

   --  ------------------------------
   --  Perform the component tree processing required by the <b>Update Model Values</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows.
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Updates/b> of all facets and children.
   --  <ul>
   --  ------------------------------
   overriding
   procedure Process_Updates (UI      : in out UIView;
                              Context : in out Faces_Context'Class) is
   begin
      Base.UIComponent (UI).Process_Updates (Context);

      --  Dispatch events queued for this phase.
      UIView'Class (UI).Broadcast (ASF.Events.Phases.UPDATE_MODEL_VALUES, Context);

      --  Drop other events if the response is to be returned.
      if Context.Get_Render_Response or else Context.Get_Response_Completed then
         UIView'Class (UI).Clear_Events;
      end if;
   end Process_Updates;

   --  ------------------------------
   --  Broadcast any events that have been queued for the <b>Invoke Application</b>
   --  phase of the request processing lifecycle and to clear out any events
   --  for later phases if the event processing for this phase caused
   --  <b>renderResponse</b> or <b>responseComplete</b> to be called.
   --  ------------------------------
   procedure Process_Application (UI      : in out UIView;
                                  Context : in out Faces_Context'Class) is
   begin
      --  Dispatch events queued for this phase.
      UIView'Class (UI).Broadcast (ASF.Events.Phases.INVOKE_APPLICATION, Context);
   end Process_Application;

   --  ------------------------------
   --  Queue an event for broadcast at the end of the current request
   --  processing lifecycle phase.  The event object
   --  will be freed after being dispatched.
   --  ------------------------------
   overriding
   procedure Queue_Event (UI    : in out UIView;
                          Event : not null access ASF.Events.Faces.Faces_Event'Class) is
      Parent : constant Base.UIComponent_Access := UI.Get_Parent;
   begin
      if Parent /= null then
         Parent.Queue_Event (Event);
      else
         UI.Phase_Events (Event.Get_Phase).Append (Event.all'Access);
      end if;
   end Queue_Event;

   --  ------------------------------
   --  Broadcast the events after the specified lifecycle phase.
   --  Events that were queued will be freed.
   --  ------------------------------
   procedure Broadcast (UI      : in out UIView;
                        Phase   : in ASF.Lifecycles.Phase_Type;
                        Context : in out Faces_Context'Class) is

      Pos : Natural := 0;

      --  Broadcast the event to the component's listeners
      --  and free that event.
      procedure Broadcast (Ev : in out Faces_Event_Access);

      procedure Broadcast (Ev : in out Faces_Event_Access) is
      begin
         if Ev /= null then
            declare
               C  : constant Base.UIComponent_Access := Ev.Get_Component;
            begin
               C.Broadcast (Ev, Context);
            end;
            Free (Ev);
         end if;
      end Broadcast;

      Parent : constant Base.UIComponent_Access := UI.Get_Parent;
   begin
      if Parent /= null then
         UIView'Class (Parent.all).Broadcast (Phase, Context);
      else
         --  Dispatch events in the order in which they were queued.
         --  More events could be queued as a result of the dispatch.
         --  After dispatching an event, it is freed but not removed
         --  from the event queue (the access will be cleared).
         loop
            exit when Pos > UI.Phase_Events (Phase).Last_Index;
            UI.Phase_Events (Phase).Update_Element (Pos, Broadcast'Access);
            Pos := Pos + 1;
         end loop;

         --  Now, clear the queue.
         UI.Phase_Events (Phase).Clear;
      end if;
   end Broadcast;

   --  ------------------------------
   --  Clear the events that were queued.
   --  ------------------------------
   procedure Clear_Events (UI : in out UIView) is
   begin
      for Phase in UI.Phase_Events'Range loop
         for I in 0 .. UI.Phase_Events (Phase).Last_Index loop
            declare
               Ev : Faces_Event_Access := UI.Phase_Events (Phase).Element (I);
            begin
               Free (Ev);
            end;
         end loop;
         UI.Phase_Events (Phase).Clear;
      end loop;
   end Clear_Events;

   --  ------------------------------
   --  Set the component tree that must be rendered before this view.
   --  This is an internal method used by Steal_Root_Component exclusively.
   --  ------------------------------
   procedure Set_Before_View (UI   : in out UIView'Class;
                              Tree : in Base.UIComponent_Access) is
   begin
      if UI.Left_Tree /= null then
         UI.Log_Error ("Set_Before_View called while there is a tree");
      end if;
      UI.Left_Tree := Tree;
   end Set_Before_View;

   --  ------------------------------
   --  Set the component tree that must be rendered after this view.
   --  This is an internal method used by Steal_Root_Component exclusively.
   --  ------------------------------
   procedure Set_After_View (UI   : in out UIView'Class;
                             Tree : in Base.UIComponent_Access) is
   begin
      if UI.Right_Tree /= null then
         UI.Log_Error ("Set_Before_View called while there is a tree");
      end if;
      UI.Right_Tree := Tree;
   end Set_After_View;

   --  ------------------------------
   --  Finalize the object.
   --  ------------------------------
   overriding
   procedure Finalize (UI : in out UIView) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Base.UIComponent'Class,
                                        Name   => Base.UIComponent_Access);

   begin
      Free (UI.Left_Tree);
      Free (UI.Right_Tree);
      Base.UIComponent (UI).Finalize;
   end Finalize;

   --  ------------------------------
   --  Set the metadata facet on the UIView component.
   --  ------------------------------
   procedure Set_Metadata (UI    : in out UIView;
                           Meta  : in UIViewMetaData_Access;
                           Tag   : access ASF.Views.Nodes.Tag_Node'Class) is
   begin
      if UI.Meta /= null then
         UI.Log_Error ("A <f:metadata> component was already registered.");
--           Delete (UI.Meta);
      end if;
      Meta.Root := UI'Unchecked_Access;
      UI.Meta := Meta;
      UI.Add_Facet (METADATA_FACET_NAME, Meta.all'Access, Tag);
   end Set_Metadata;

   --  ------------------------------
   --  Get the input parameter from the submitted context.  This operation is called by
   --  <tt>Process_Decodes</tt> to retrieve the request parameter associated with the component.
   --  ------------------------------
   overriding
   function Get_Parameter (UI      : in UIViewParameter;
                           Context : in Faces_Context'Class) return String is
      Name  : constant String := UI.Get_Attribute ("name", Context);
   begin
      if Name'Length > 0 then
         return Context.Get_Parameter (Name);
      else
         declare
            Value : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, "from");
         begin
            if Util.Beans.Objects.Is_Null (Value) then
               return Html.Forms.UIInput (UI).Get_Parameter (Context);
            else
               return Util.Beans.Objects.To_String (Value);
            end if;
         end;
      end if;
   end Get_Parameter;

   --  ------------------------------
   --  Decode the request and prepare for the execution for the view action.
   --  ------------------------------
   overriding
   procedure Process_Decodes (UI      : in out UIViewAction;
                              Context : in out Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      begin
         ASF.Events.Faces.Actions.Post_Event (UI     => UI,
                                              Method => UI.Get_Action_Expression (Context));

      exception
         when EL.Expressions.Invalid_Expression =>
            null;
      end;
   end Process_Decodes;

   --  ------------------------------
   --  Get the root component.
   --  ------------------------------
   function Get_Root (UI      : in UIViewMetaData) return Base.UIComponent_Access is
      Result : Base.UIComponent_Access := UI.Get_Parent;
      Parent : Base.UIComponent_Access := Result.Get_Parent;
   begin
      while Parent /= null loop
         Result := Parent;
         Parent := Parent.Get_Parent;
      end loop;
      return Result;
   end Get_Root;

   --  ------------------------------
   --  Start encoding the UIComponent.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIViewMetaData;
                           Context : in out Faces_Context'Class) is
   begin
      UI.Get_Root.Encode_Begin (Context);
   end Encode_Begin;

   --  ------------------------------
   --  Encode the children of this component.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UIViewMetaData;
                              Context : in out Faces_Context'Class) is
   begin
      UI.Get_Root.Encode_Children (Context);
   end Encode_Children;

   --  ------------------------------
   --  Finish encoding the component.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIViewMetaData;
                         Context : in out Faces_Context'Class) is
   begin
      UI.Get_Root.Encode_End (Context);
   end Encode_End;

   --  ------------------------------
   --  Queue an event for broadcast at the end of the current request
   --  processing lifecycle phase.  The event object
   --  will be freed after being dispatched.
   --  ------------------------------
   overriding
   procedure Queue_Event (UI    : in out UIViewMetaData;
                          Event : not null access ASF.Events.Faces.Faces_Event'Class) is
   begin
      UI.Root.Queue_Event (Event);
   end Queue_Event;

   --  ------------------------------
   --  Broadcast the events after the specified lifecycle phase.
   --  Events that were queued will be freed.
   --  ------------------------------
   overriding
   procedure Broadcast (UI      : in out UIViewMetaData;
                        Phase   : in ASF.Lifecycles.Phase_Type;
                        Context : in out Faces_Context'Class) is
   begin
      UI.Root.Broadcast (Phase, Context);
   end Broadcast;

   --  ------------------------------
   --  Clear the events that were queued.
   --  ------------------------------
   overriding
   procedure Clear_Events (UI : in out UIViewMetaData) is
   begin
      UI.Root.Clear_Events;
   end Clear_Events;

end ASF.Components.Core.Views;
