-----------------------------------------------------------------------
--  asf-contexts.faces -- Faces Contexts
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

with ASF.Requests;
with ASF.Responses;
with ASF.Contexts.Writer;
with EL.Objects;
with EL.Contexts;
with Ada.Strings.Unbounded;

--  The <b>Faces_Context</b> is an object passed to the component tree and
--  bean actions to provide the full context in which the view is rendered
--  or evaluated.  The faces context gives access to the bean variables,
--  the request and its parameters, the response writer to write on the
--  output stream.
--
--  The <b>Faces_Context</b> is never shared: it is specific to each request.
package ASF.Contexts.Faces is

   use Ada.Strings.Unbounded;

   type Faces_Context is tagged private;

   type Faces_Context_Access is access all Faces_Context'Class;

   --  Get the response writer to write the response stream.
   function Get_Response_Writer (Context : Faces_Context)
     return ASF.Contexts.Writer.ResponseWriter_Access;

   --  Set the response writer to write to the response stream.
   procedure Set_Response_Writer (Context : in out Faces_Context;
                                  Writer  : in ASF.Contexts.Writer.ResponseWriter_Access);

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

   --  Get a request parameter
   function Get_Parameter (Context : Faces_Context;
                           Name    : String) return String;

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

   --  Get the current faces context.  The faces context is saved
   --  in a per-thread/task attribute.
   function Current return Faces_Context_Access;

   --  Set the current faces context in the per-thread/task attribute.
   procedure Set_Current (Context : Faces_Context_Access);

private

   type Faces_Context is tagged record
      --  The response writer.
      Writer   : ASF.Contexts.Writer.ResponseWriter_Access;

      --  The expression context;
      Context  : EL.Contexts.ELContext_Access;

      --  The request
      Request  : ASF.Requests.Request_Access;

      --  The response
      Response : ASF.Responses.Response_Access;
   end record;

end ASF.Contexts.Faces;
