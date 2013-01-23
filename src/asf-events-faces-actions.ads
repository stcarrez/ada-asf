-----------------------------------------------------------------------
--  asf-events-faces-actions -- Actions Events
--  Copyright (C) 2010, 2011, 2012, 2013 Stephane Carrez
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

with Util.Events;

with Ada.Strings.Unbounded;
with EL.Expressions;
with EL.Methods.Proc_1;

with ASF.Contexts.Faces;
with ASF.Components.Base;
package ASF.Events.Faces.Actions is

   package Action_Method is
     new EL.Methods.Proc_1 (Param1_Type => Ada.Strings.Unbounded.Unbounded_String);

   --  ------------------------------
   --  Action event
   --  ------------------------------
   --  The <b>Action_Event</b> is posted when a method bean action must be executed
   --  as a result of a command (such as button press).  The method action must return
   --  an outcome string that indicates which view to return.
   type Action_Event is new Faces_Event with private;
   type Action_Event_Access is access all Action_Event'Class;

   --  Get the method expression to invoke
   function Get_Method (Event : in Action_Event) return EL.Expressions.Method_Expression;

   --  Get the method binding with the Ada bean object to invoke.
   function Get_Method_Info (Event : in Action_Event;
                             Context : in Contexts.Faces.Faces_Context'Class)
                             return EL.Expressions.Method_Info;

   --  Post an <b>Action_Event</b> on the component.
   procedure Post_Event (UI     : in out Components.Base.UIComponent'Class;
                         Method : in EL.Expressions.Method_Expression);

   --  ------------------------------
   --  Action Listener
   --  ------------------------------
   --  The <b>Action_Listener</b> is the interface to receive the <b>Action_Event</b>.
   type Action_Listener is limited interface and Util.Events.Event_Listener;
   type Action_Listener_Access is access all Action_Listener'Class;

   --  Process the action associated with the action event.
   procedure Process_Action (Listener : in Action_Listener;
                             Event    : in Action_Event'Class;
                             Context  : in out Contexts.Faces.Faces_Context'Class) is abstract;

private

   type Action_Event is new Faces_Event with record
      Method : EL.Expressions.Method_Expression;
   end record;

end ASF.Events.Faces.Actions;
