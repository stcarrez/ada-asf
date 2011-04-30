-----------------------------------------------------------------------
--  asf.server -- ASF Server for AWS
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
with AWS.Config.Set;
with AWS.Status;
with AWS.Response;

with ASF.Requests.Web;
with ASF.Responses.Web;
with ASF.Clients.Web;

with Util.Log.Loggers;
package body ASF.Server.Web is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("ASF.Server.Web");
   --  The logger

   function Server_Callback (Request : in AWS.Status.Data) return AWS.Response.Data;

   Server : AWS_Container_Access;

   procedure Initialize (Instance : in out AWS_Container) is
   begin
      Instance.Conf := AWS.Config.Get_Current;
      AWS.Config.Set.Reuse_Address (O => Instance.Conf, Value => True);
      Server := Instance'Unchecked_Access;
   end Initialize;

   procedure Start (Server : in out AWS_Container) is
   begin
      Log.Info ("Starting server...");

      AWS.Server.Start (Web_Server => Server.WS,
                        Config     => Server.Conf,
                        Callback   => ASF.Server.Web.Server_Callback'Access);
   end Start;
--
--     function Application_Dispatch (App     : Main.Application_Access;
--                                    Page    : String;
--                                    Request : AWS.Status.Data;
--                                    Status  : AWS.Messages.Status_Code)
--                                    return AWS.Response.Data is
--        Writer   : aliased ASF.Contexts.Writer.String.String_Writer;
--        Req      : aliased ASF.Requests.Web.Request;
--     begin
--        Writer.Initialize ("text/html", "UTF-8", 8192);
--
--        App.Dispatch (Page    => Page,
--                      Writer  => Writer'Unchecked_Access,
--                      Request => Req'Unchecked_Access);
--
--        return AWS.Response.Build (Content_Type    => Writer.Get_Content_Type,
--                                   Status_Code     => Status,
--                                   UString_Message => Writer.Get_Response);
--     end Application_Dispatch;

   ----------------------
   --  Main server callback
   ----------------------
   function Server_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      Req   : ASF.Requests.Web.Request;
      Resp  : ASF.Responses.Web.Response;
   begin
      Req.Set_Request (Request'Unrestricted_Access);

      Server.Service (Req, Resp);

      Resp.Build;
      return Resp.Get_Data;
   end Server_Callback;

begin
   ASF.Clients.Web.Register;
end ASF.Server.Web;
