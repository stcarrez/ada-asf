-----------------------------------------------------------------------
--  asf.server -- ASF Server for AWS
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
with AWS.Config.Set;
with AWS.Status;
with AWS.Response;

with ASF.Requests.Web;
with ASF.Responses.Web;
with Util.Http.Clients.Web;

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

   ----------------------
   --  Start the applications that have been registered.
   ----------------------
   procedure Start (Server : in out AWS_Container) is
   begin
      Log.Info ("Starting server...");

      Container (Server).Start;

      AWS.Config.Set.Upload_Directory (Server.Conf, "upload");
      AWS.Server.Start (Web_Server => Server.WS,
                        Config     => Server.Conf,
                        Callback   => ASF.Server.Web.Server_Callback'Access);
   end Start;

   ----------------------
   --  Configure the AWS server.
   ----------------------
   procedure Configure (Server : in out AWS_Container;
                        Process : not null access procedure (Config : in out AWS.Config.Object)) is
   begin
      Process (Server.Conf);
   end Configure;

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
   Util.Http.Clients.Web.Register;
end ASF.Server.Web;
