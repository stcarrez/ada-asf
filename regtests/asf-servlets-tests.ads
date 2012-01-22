-----------------------------------------------------------------------
--  Servlets Tests - Unit tests for ASF.Servlets
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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

with Util.Tests;

package ASF.Servlets.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test_Servlet1 is new Servlet with null record;

   procedure Do_Get (Server   : in Test_Servlet1;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class);

   type Test_Servlet2 is new Test_Servlet1 with null record;

   procedure Do_Post (Server   : in Test_Servlet2;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class);

   type Test is new Util.Tests.Test with record
      Writer    : Integer;
   end record;

   --  Test creation of session
   procedure Test_Create_Servlet (T : in out Test);

   --  Test add servlet
   procedure Test_Add_Servlet (T : in out Test);

   --  Test getting a resource path
   procedure Test_Get_Resource (T : in out Test);

   procedure Test_Request_Dispatcher (T : in out Test);

   --  Check that the mapping for the given URI matches the server.
   procedure Check_Mapping (T      : in out Test;
                            Ctx    : in Servlet_Registry;
                            URI    : in String;
                            Server : in Servlet_Access);

end ASF.Servlets.Tests;
