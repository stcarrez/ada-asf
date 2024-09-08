-----------------------------------------------------------------------
--  asf-events-faces-actions -- Actions Events
--  Copyright (C) 2009, 2010, 2011, 2012, 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
