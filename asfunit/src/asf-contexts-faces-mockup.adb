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
with ASF.Tests;
package body ASF.Contexts.Faces.Mockup is

   --  ------------------------------
   --  Initialize the mockup context.
   --  ------------------------------
   overriding
   procedure Initialize (Context : in out Mockup_Faces_Context) is
   begin
      Faces_Context (Context).Initialize;
      Context.Prev_Context := Current;
      Context.Request  := Context.Mock_Request'Unchecked_Access;
      Context.Response := Context.Mock_Response'Unchecked_Access;
      Context.Flash    := Context.Flash_Ctx'Unchecked_Access;

      Context.Resolver.Initialize (ASF.Tests.Get_Application, Context.Request);
      Context.ELContext.Set_Resolver (Context.Resolver'Unchecked_Access);
      Context.ELContext.Set_Variable_Mapper (Context.Variables'Unchecked_Access);

      Context.Set_ELContext (Context.ELContext'Unchecked_Access);
      Context.Set_Response_Writer (Context.Output'Unchecked_Access);
      Context.Output.Initialize ("text/html", "UTF-8", Context.Response.Get_Output_Stream);
      Set_Current (Context     => Context'Unchecked_Access,
                   Application => ASF.Tests.Get_Application.all'Access);
   end Initialize;

   --  ------------------------------
   --  Release any storage held by this context.
   --  ------------------------------
   overriding
   procedure Finalize (Context : in out Mockup_Faces_Context) is
   begin
      Faces_Context (Context).Finalize;
      Restore (Context.Prev_Context);
   end Finalize;

   --  ------------------------------
   --  Set the path info
   --  ------------------------------
   procedure Set_Path_Info (Req  : in out Mockup_Faces_Context;
                            Path : in String) is
   begin
      Req.Mock_Request.Set_Path_Info (Path);
   end Set_Path_Info;

   --  ------------------------------
   --  Set the parameter
   --  ------------------------------
   procedure Set_Parameter (Req   : in out Mockup_Faces_Context;
                            Name  : in String;
                            Value : in String) is
   begin
      Req.Mock_Request.Set_Parameter (Name, Value);
   end Set_Parameter;

   --  ------------------------------
   --  Sets the HTTP method.
   --  ------------------------------
   procedure Set_Method (Req    : in out Mockup_Faces_Context;
                         Method : in String) is
   begin
      Req.Mock_Request.Set_Method (Method);
   end Set_Method;

   --  ------------------------------
   --  Sets the protocol version
   --  ------------------------------
   procedure Set_Protocol (Req      : in out Mockup_Faces_Context;
                           Protocol : in String) is
   begin
      Req.Mock_Request.Set_Protocol (Protocol);
   end Set_Protocol;

   --  ------------------------------
   --  Set the request URI.
   --  ------------------------------
   procedure Set_Request_URI (Req : in out Mockup_Faces_Context;
                              URI : in String) is
   begin
      Req.Mock_Request.Set_Request_URI (URI);
   end Set_Request_URI;

   --  ------------------------------
   --  Sets the peer address
   --  ------------------------------
   procedure Set_Remote_Addr (Req  : in out Mockup_Faces_Context;
                              Addr : in String) is
   begin
      Req.Mock_Request.Set_Remote_Addr (Addr);
   end Set_Remote_Addr;

   --  ------------------------------
   --  Set the request cookie by using the cookie returned in the response.
   --  ------------------------------
   procedure Set_Cookie (Req  : in out Mockup_Faces_Context;
                         From : in ASF.Responses.Mockup.Response'Class) is
   begin
      Req.Mock_Request.Set_Cookie (From);
   end Set_Cookie;

   --  ------------------------------
   --  Get the content written to the mockup output stream.
   --  ------------------------------
   procedure Read_Response (Resp : in out Mockup_Faces_Context;
                            Into : out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Resp.Mock_Response.Read_Content (Into);
   end Read_Response;

end ASF.Contexts.Faces.Mockup;
