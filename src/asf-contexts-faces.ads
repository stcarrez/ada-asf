-----------------------------------------------------------------------
--  asf-contexts.faces -- Faces Contexts
--  Copyright (C) 2009, 2010, 2011, 2012, 2015, 2023 Stephane Carrez
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

with ASF.Requests;
with ASF.Responses;
with ASF.Sessions;
limited with ASF.Converters;
with ASF.Components.Root;
limited with ASF.Applications.Main;
with ASF.Applications.Messages;
with ASF.Applications.Messages.Vectors;
with ASF.Contexts.Writer;
with ASF.Contexts.Exceptions;
limited with ASF.Contexts.Flash;
with ASF.Events.Exceptions;
with ASF.Events.Phases;

with EL.Objects;
with EL.Contexts;
with Util.Locales;
with Util.Beans.Basic;

with Ada.Exceptions;
with Ada.Strings.Unbounded;

private with Ada.Finalization;
private with Ada.Strings.Unbounded.Hash;
private with Ada.Containers.Hashed_Maps;

--  The <b>Faces_Context</b> is an object passed to the component tree and
--  bean actions to provide the full context in which the view is rendered
--  or evaluated.  The faces context gives access to the bean variables,
--  the request and its parameters, the response writer to write on the
--  output stream.
--
--  The <b>Faces_Context</b> is never shared: it is specific to each request.
package ASF.Contexts.Faces is

   use Ada.Strings.Unbounded;

   type Application_Access is access all ASF.Applications.Main.Application'Class;
   type Flash_Context_Access is access all ASF.Contexts.Flash.Flash_Context'Class;

   type Faces_Context is tagged limited private;

   type Faces_Context_Access is access all Faces_Context'Class;

   --  Get the response writer to write the response stream.
   function Get_Response_Writer (Context : Faces_Context)
     return ASF.Contexts.Writer.Response_Writer_Access;

   --  Set the response writer to write to the response stream.
   procedure Set_Response_Writer (Context : in out Faces_Context;
                                  Writer  : in ASF.Contexts.Writer.Response_Writer_Access);

   --  Get the EL context for evaluating expressions.
   function Get_ELContext (Context : in Faces_Context)
                           return EL.Contexts.ELContext_Access;

   --  Set the EL context for evaluating expressions.
   procedure Set_ELContext (Context   : in out Faces_Context;
                            ELContext : in EL.Contexts.ELContext_Access);

   --  Set the attribute having given name with the value.
   procedure Set_Attribute (Context : in out Faces_Context;
                            Name    : in String;
                            Value   : in EL.Objects.Object);

   --  Set the attribute having given name with the value.
   procedure Set_Attribute (Context : in out Faces_Context;
                            Name    : in Unbounded_String;
                            Value   : in EL.Objects.Object);

   --  Get the attribute with the given name.
   function Get_Attribute (Context : in Faces_Context;
                           Name    : in String) return EL.Objects.Object;

   --  Get the attribute with the given name.
   function Get_Attribute (Context : in Faces_Context;
                           Name    : in Unbounded_String) return EL.Objects.Object;

   --  Get the bean attribute with the given name.
   --  Returns null if the attribute does not exist or is not a bean.
   function Get_Bean (Context : in Faces_Context;
                      Name    : in String) return Util.Beans.Basic.Readonly_Bean_Access;

   --  Get a request parameter
   function Get_Parameter (Context : Faces_Context;
                           Name    : String) return String;

   --  Get the session associated with the current faces context.
   function Get_Session (Context : in Faces_Context;
                         Create  : in Boolean := False) return ASF.Sessions.Session;

   --  Get the request
   function Get_Request (Context : Faces_Context) return ASF.Requests.Request_Access;

   --  Set the request
   procedure Set_Request (Context : in out Faces_Context;
                          Request : in ASF.Requests.Request_Access);

   --  Get the response
   function Get_Response (Context : Faces_Context) return ASF.Responses.Response_Access;

   --  Set the response
   procedure Set_Response (Context  : in out Faces_Context;
                           Response : in ASF.Responses.Response_Access);

   --  Signal the JavaServer faces implementation that, as soon as the
   --  current phase of the request processing lifecycle has been completed,
   --  control should be passed to the <b>Render Response</b> phase,
   --  bypassing any phases that have not been executed yet.
   procedure Render_Response (Context : in out Faces_Context);

   --  Check whether the <b>Render_Response</b> phase must be processed immediately.
   function Get_Render_Response (Context : in Faces_Context) return Boolean;

   --  Signal the JavaServer Faces implementation that the HTTP response
   --  for this request has already been generated (such as an HTTP redirect),
   --  and that the request processing lifecycle should be terminated as soon
   --  as the current phase is completed.
   procedure Response_Completed (Context : in out Faces_Context);

   --  Check whether the response has been completed.
   function Get_Response_Completed (Context : in Faces_Context) return Boolean;

   --  Get the flash context allowing to add flash attributes.
   function Get_Flash (Context : in Faces_Context) return Flash_Context_Access;

   --  Set the flash context.
   procedure Set_Flash (Context : in out Faces_Context;
                        Flash   : in Flash_Context_Access);

   --  Append the message to the list of messages associated with the specified
   --  client identifier.  If <b>Client_Id</b> is empty, the message is global
   --  (or not associated with a component)
   procedure Add_Message (Context   : in out Faces_Context;
                          Client_Id : in String;
                          Message   : in ASF.Applications.Messages.Message);

   --  Append the message to the list of messages associated with the specified
   --  client identifier.  If <b>Client_Id</b> is empty, the message is global
   --  (or not associated with a component)
   procedure Add_Message (Context   : in out Faces_Context;
                          Client_Id : in String;
                          Message   : in String;
                          Severity  : in Applications.Messages.Severity
                          := Applications.Messages.ERROR);

   --  Append the messages defined in <b>Messages</b> to the current list of messages
   --  in the faces context.
   procedure Add_Messages (Context   : in out Faces_Context;
                           Client_Id : in String;
                           Messages  : in ASF.Applications.Messages.Vectors.Vector);

   --  Get an iterator for the messages associated with the specified client
   --  identifier.  If the <b>Client_Id</b> ie empty, an iterator for the
   --  global messages is returned.
   function Get_Messages (Context   : in Faces_Context;
                          Client_Id : in String) return ASF.Applications.Messages.Vectors.Cursor;

   --  Returns the maximum severity level recorded for any message that has been queued.
   --  Returns NONE if no message has been queued.
   function Get_Maximum_Severity (Context : in Faces_Context)
                                  return ASF.Applications.Messages.Severity;

   --  Get a converter from a name.
   --  Returns the converter object or null if there is no converter.
   function Get_Converter (Context : in Faces_Context;
                           Name    : in EL.Objects.Object)
                           return access ASF.Converters.Converter'Class;

   --  Get the application associated with this faces context.
   function Get_Application (Context : in Faces_Context)
                             return Application_Access;

   --  Get the current lifecycle phase.
   function Get_Current_Phase (Context : in Faces_Context) return ASF.Events.Phases.Phase_Type;

   --  Set the current lifecycle phase.  This operation is called by the lifecycle manager
   --  each time the lifecycle phase changes.
   procedure Set_Current_Phase (Context : in out Faces_Context;
                                Phase   : in ASF.Events.Phases.Phase_Type);

   --  Get the locale defined by the view root component.
   --  Returns the NULL_LOCALE if there is no view root component.
   function Get_Locale (Context : in Faces_Context) return Util.Locales.Locale;

   --  Set the locale that must be used when rendering the view components.
   procedure Set_Locale (Context : in out Faces_Context;
                         Locale  : in Util.Locales.Locale);

   --  Get the component view root.
   function Get_View_Root (Context : in Faces_Context)
                           return ASF.Components.Root.UIViewRoot;

   --  Get the component view root.
   procedure Set_View_Root (Context : in out Faces_Context;
                            View    : in ASF.Components.Root.UIViewRoot);

   --  Get the view name associated with the current faces request.
   --  The view name is obtained from the request and the route mapping definition.
   --  If a pretty URL configuration was set through the `url-mapping` definition, the view
   --  name correspond to the `view-id` declaration.  Otherwise, the view name corresponds
   --  to the servlet's path.
   function Get_View_Name (Context : in Faces_Context) return String;

   --  Create an identifier for a component.
   procedure Create_Unique_Id (Context : in out Faces_Context;
                               Id      : out Natural);

   --  Verify the CSRF token validity for the component identified by `Id`.
   --  Returns True if the token is valid and false if it has expired or is invalid.
   function Verify_Token (Context : in Faces_Context;
                          Id      : in String;
                          Token   : in String) return Boolean;

   --  Create a CSRF token for the component identified by `Id`.
   --  The token is associated with the web session and signed by the application.
   function Create_Token (Context : in Faces_Context;
                          Id      : in String;
                          Expire  : in Duration) return String;

   --  Set the exception handler that will receive unexpected exceptions and process them.
   procedure Set_Exception_Handler (Context : in out Faces_Context;
                                    Handler : in Exceptions.Exception_Handler_Access);

   --  Get the exception handler.
   function Get_Exception_Handler (Context : in Faces_Context)
                                   return Exceptions.Exception_Handler_Access;

   --  Queue an exception event to the exception handler associated with the context.
   --  The exception event will be processed at the end of the current ASF phase.
   procedure Queue_Exception (Context : in out Faces_Context;
                              Ex      : in Ada.Exceptions.Exception_Occurrence);

   --  Iterate over the exceptions that have been queued and execute the <b>Process</b>
   --  procedure.  When the procedure returns True in <b>Remove</b, the exception event
   --  is removed from the queue.  The procedure can update the faces context to add some
   --  error message or redirect to an error page.
   --
   --  The application exception handler uses this procedure to process the exceptions.
   --  The exception handler is called after each ASF phase.
   procedure Iterate_Exception (Context : in out Faces_Context'Class;
                                Process : not null access
                                  procedure (Event   : in Events.Exceptions.Exception_Event'Class;
                                             Remove  : out Boolean;
                                             Context : in out Faces_Context'Class));

   --  Returns True if the current request is an AJAX request.
   function Is_Ajax_Request (Context : in Faces_Context'Class) return Boolean;

   --  Set the Ajax request status.
   procedure Set_Ajax_Request (Context : in out Faces_Context'Class;
                               Status  : in Boolean);

   --  Get the current faces context.  The faces context is saved
   --  in a per-thread/task attribute.
   function Current return Faces_Context_Access;

   --  Set the current faces context in the per-thread/task attribute.
   procedure Set_Current (Context     : in Faces_Context_Access;
                          Application : in Application_Access);

   --  Restore the previous faces context.
   procedure Restore (Context : in Faces_Context_Access);

private

   use ASF.Applications.Messages;

   type Exception_Queue_Access is access ASF.Contexts.Exceptions.Exception_Queue;

   --  Map of messages associated with a component
   package Message_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_String,
                                 Element_Type    => Vectors.Vector,
                                 Hash            => Hash,
                                 Equivalent_Keys => "=",
                                 "="             => Vectors."=");

   type Faces_Context is new Ada.Finalization.Limited_Controlled with record
      --  The response writer.
      Writer   : ASF.Contexts.Writer.Response_Writer_Access;

      --  The expression context;
      Context  : EL.Contexts.ELContext_Access;

      --  The request
      Request  : ASF.Requests.Request_Access;

      --  The response
      Response : ASF.Responses.Response_Access;

      --  The application
      Application : Application_Access;

      --  The exception handler and exception queue.
      Except_Handler     : Exceptions.Exception_Handler_Access;
      Except_Queue       : Exception_Queue_Access;

      Render_Response    : Boolean := False;
      Response_Completed : Boolean := False;

      --  True if the view is processed as part of an AJAX request.
      Ajax               : Boolean := False;

      --  List of messages added indexed by the client identifier.
      Messages           : Message_Maps.Map;

      --  The maximum severity for the messages that were collected.
      Max_Severity       : Severity := NONE;

      Root               : ASF.Components.Root.UIViewRoot;

      --  The flash context.
      Flash              : Flash_Context_Access;

      --  The current lifecycle phase.
      Phase              : ASF.Events.Phases.Phase_Type := ASF.Events.Phases.RESTORE_VIEW;

      --  The locale defined by the view root component.  Unlike JSF, we store the locale
      --  in the faces context.  This is easier for the implementation.
      Locale             : Util.Locales.Locale := Util.Locales.NULL_LOCALE;
   end record;

   --  Release any storage held by this context.
   overriding
   procedure Finalize (Context : in out Faces_Context);

end ASF.Contexts.Faces;
