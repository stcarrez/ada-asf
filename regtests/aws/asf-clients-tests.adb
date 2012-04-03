-----------------------------------------------------------------------
--  asf-clients-tests - Unit tests for HTTP clients
--  Copyright (C) 2011 Stephane Carrez
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

with Util.Test_Caller;
with Util.Log.Loggers;

with ASF.Clients.Web;
package body ASF.Clients.Tests is

   use Util.Tests;
   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Clients.Tests");

   package Caller is new Util.Test_Caller (Test, "Clients");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ASF.Clients.Do_Get",
                       Test_Http_Get'Access);
      ASF.Clients.Web.Register;
   end Add_Tests;

   --  ------------------------------
   --  Test creation of cookie
   --  ------------------------------
   procedure Test_Http_Get (T : in out Test) is
      pragma Unreferenced (T);

      C     : ASF.Clients.Client;
      Reply : ASF.Clients.Response;
   begin
      C.Do_Get (URL   => "http://www.google.com/", Reply => Reply);
      Log.Info ("Result: {0}", Reply.Get_Body);
   end Test_Http_Get;

end ASF.Clients.Tests;
