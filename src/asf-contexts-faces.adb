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

with EL.Variables;
with Ada.Task_Attributes;
package body ASF.Contexts.Faces is

   package Task_Context is new Ada.Task_Attributes
     (Faces_Context_Access, null);

   --  ------------------------------
   --  Get the response writer to write the response stream.
   --  ------------------------------
   function Get_Response_Writer (Context : Faces_Context)
                                 return ASF.Contexts.Writer.ResponseWriter_Access is
   begin
      return Context.Writer;
   end Get_Response_Writer;

   --  ------------------------------
   --  Set the response writer to write to the response stream.
   --  ------------------------------
   procedure Set_Response_Writer (Context : in out Faces_Context;
                                  Writer  : in ASF.Contexts.Writer.ResponseWriter_Access) is
   begin
      Context.Writer := Writer;
   end Set_Response_Writer;

   --  ------------------------------
   --  Get the EL context for evaluating expressions.
   --  ------------------------------
   function Get_ELContext (Context : in Faces_Context)
                           return EL.Contexts.ELContext_Access is
   begin
      return Context.Context;
   end Get_ELContext;

   --  ------------------------------
   --  Set the EL context for evaluating expressions.
   --  ------------------------------
   procedure Set_ELContext (Context   : in out Faces_Context;
                            ELContext : in EL.Contexts.ELContext_Access) is
   begin
      Context.Context := ELContext;
   end Set_ELContext;

   --  ------------------------------
   --  Set the attribute having given name with the value.
   --  ------------------------------
   procedure Set_Attribute (Context : in out Faces_Context;
                            Name    : in String;
                            Value   : in EL.Objects.Object) is
      use EL.Variables;
      Mapper : constant access VariableMapper'Class := Context.Context.Get_Variable_Mapper;
   begin
      Mapper.Bind (Name, Value);
   end Set_Attribute;

   --  ------------------------------
   --  Set the attribute having given name with the value.
   --  ------------------------------
   procedure Set_Attribute (Context : in out Faces_Context;
                            Name    : in Unbounded_String;
                            Value   : in EL.Objects.Object) is
   begin
      Context.Set_Attribute (To_String (Name), Value);
   end Set_Attribute;

   --  ------------------------------
   --  Get a request parameter
   --  ------------------------------
   function Get_Parameter (Context : Faces_Context;
                           Name    : String) return String is
   begin
      return Context.Request.Get_Parameter (Name);
   end Get_Parameter;

   --  ------------------------------
   --  Get the request
   --  ------------------------------
   function Get_Request (Context : Faces_Context)
                         return ASF.Requests.Request_Access is
   begin
      return Context.Request;
   end Get_Request;

   --  ------------------------------
   --  Set the request
   --  ------------------------------
   procedure Set_Request (Context : in out Faces_Context;
                          Request : in ASF.Requests.Request_Access) is
   begin
      Context.Request := Request;
   end Set_Request;

   --  ------------------------------
   --  Get the current faces context.  The faces context is saved
   --  in a per-thread/task attribute.
   --  ------------------------------
   function Current return Faces_Context_Access is
   begin
      return Task_Context.Value;
   end Current;

   --  ------------------------------
   --  Set the current faces context in the per-thread/task attribute.
   --  ------------------------------
   procedure Set_Current (Context : Faces_Context_Access) is
   begin
      Task_Context.Set_Value (Context);
   end Set_Current;

end ASF.Contexts.Faces;
