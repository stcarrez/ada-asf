-----------------------------------------------------------------------
--  asf-contexts-faces-mockups - Mockup for faces context
--  Copyright (C) 2013 Stephane Carrez
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
with EL.Variables.Default;
with EL.Contexts.Default;

with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Beans.Resolvers;
with ASF.Contexts.Flash;
with ASF.Contexts.Writer.String;

package ASF.Contexts.Faces.Mockup is

   type Mockup_Faces_Context is new ASF.Contexts.Faces.Faces_Context with private;

   --  Set the path info
   procedure Set_Path_Info (Req  : in out Mockup_Faces_Context;
                            Path : in String);

   --  Set the parameter
   procedure Set_Parameter (Req   : in out Mockup_Faces_Context;
                            Name  : in String;
                            Value : in String);

   --  Sets the HTTP method.
   procedure Set_Method (Req    : in out Mockup_Faces_Context;
                         Method : in String);

   --  Sets the protocol version
   procedure Set_Protocol (Req      : in out Mockup_Faces_Context;
                           Protocol : in String);

   --  Set the request URI.
   procedure Set_Request_URI (Req : in out Mockup_Faces_Context;
                              URI : in String);

   --  Sets the peer address
   procedure Set_Remote_Addr (Req  : in out Mockup_Faces_Context;
                              Addr : in String);

   --  Set the request cookie by using the cookie returned in the response.
   procedure Set_Cookie (Req  : in out Mockup_Faces_Context;
                         From : in ASF.Responses.Mockup.Response'Class);

   --  Get the content written to the mockup output stream.
   procedure Read_Response (Resp : in out Mockup_Faces_Context;
                            Into : out Ada.Strings.Unbounded.Unbounded_String);

private

   type Mockup_Faces_Context is new ASF.Contexts.Faces.Faces_Context with record
      Mock_Request  : aliased ASF.Requests.Mockup.Request;
      Mock_Response : aliased ASF.Responses.Mockup.Response;
      Resolver      : aliased ASF.Beans.Resolvers.ELResolver;
      Flash_Ctx     : aliased ASF.Contexts.Flash.Flash_Context;
      Variables     : aliased EL.Variables.Default.Default_Variable_Mapper;
      Output        : aliased ASF.Contexts.Writer.String.String_Writer;
      ELContext     : aliased EL.Contexts.Default.Default_Context;
      Prev_Context  : Contexts.Faces.Faces_Context_Access;
   end record;

   --  Initialize the mockup context.
   overriding
   procedure Initialize (Context : in out Mockup_Faces_Context);

   --  Release any storage held by this context.
   overriding
   procedure Finalize (Context : in out Mockup_Faces_Context);

end ASF.Contexts.Faces.Mockup;
