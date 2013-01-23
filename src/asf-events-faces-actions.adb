-----------------------------------------------------------------------
--  asf-events-faces-actions -- Actions Events
--  Copyright (C) 2009, 2010, 2011, 2012, 2013 Stephane Carrez
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

package body ASF.Events.Faces.Actions is

   --  ------------------------------
   --  Get the method expression to invoke
   --  ------------------------------
   function Get_Method (Event : in Action_Event) return EL.Expressions.Method_Expression is
   begin
      return Event.Method;
   end Get_Method;

   --  ------------------------------
   --  Get the method binding with the Ada bean object to invoke.
   --  ------------------------------
   function Get_Method_Info (Event : in Action_Event;
                             Context : in Contexts.Faces.Faces_Context'Class)
                             return EL.Expressions.Method_Info is
   begin
      return Event.Method.Get_Method_Info (Context => Context.Get_ELContext.all);
   end Get_Method_Info;

   --  ------------------------------
   --  Post an <b>Action_Event</b> on the component.
   --  ------------------------------
   procedure Post_Event (UI     : in out Components.Base.UIComponent'Class;
                         Method : in EL.Expressions.Method_Expression) is
      Ev : constant Action_Event_Access := new Action_Event;
   begin
      Ev.Phase     := ASF.Events.Phases.INVOKE_APPLICATION;
      Ev.Component := UI'Unchecked_Access;
      Ev.Method    := Method;
      UI.Queue_Event (Ev.all'Access);
   end Post_Event;

end ASF.Events.Faces.Actions;
