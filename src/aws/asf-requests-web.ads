-----------------------------------------------------------------------
--  asf.requests -- ASF Requests
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
with AWS.Status;
package ASF.Requests.Web is

   type Request is new ASF.Requests.Request with private;
   type Request_Access is access all Request'Class;

   function Get_Parameter (R : Request; Name : String) return String;

   procedure Set_Request (R : in out Request;
                          Data : access AWS.Status.Data);

private

   type Request is new ASF.Requests.Request with record
      Data : access AWS.Status.Data;
   end record;

end ASF.Requests.Web;
