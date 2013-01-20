-----------------------------------------------------------------------
--  ASF tests - ASF Tests Framework
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with Util.Properties;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Server;
with ASF.Applications.Main;
with Util.Tests;

with GNAT.Source_Info;

--  The <b>ASF.Tests</b> package provides a set of utility procedures to write a unit test
--  on top of ASF.
package ASF.Tests is

   --  Initialize the asf test framework mockup.  If the application is not specified,
   --  a default ASF application is created.
   procedure Initialize (Props       : in Util.Properties.Manager;
                         Application : in ASF.Applications.Main.Application_Access := null;
                         Factory     : in out ASF.Applications.Main.Application_Factory'Class);

   --  Get the server
   function Get_Server return access ASF.Server.Container;

   --  Get the test application.
   function Get_Application return ASF.Applications.Main.Application_Access;

   --  Simulate a GET request on the given URI with the request parameters.
   --  Get the result in the response object.
   procedure Do_Get (Request  : in out ASF.Requests.Mockup.Request;
                     Response : in out ASF.Responses.Mockup.Response;
                     URI      : in String;
                     Save     : in String := "");

   --  Simulate a POST request on the given URI with the request parameters.
   --  Get the result in the response object.
   procedure Do_Post (Request  : in out ASF.Requests.Mockup.Request;
                      Response : in out ASF.Responses.Mockup.Response;
                      URI      : in String;
                      Save     : in String := "");

   --  Simulate a raw request.  The URI and method must have been set on the Request object.
   procedure Do_Req (Request  : in out ASF.Requests.Mockup.Request;
                     Response : in out ASF.Responses.Mockup.Response);

   --  Check that the response body contains the string
   procedure Assert_Contains (T       : in Util.Tests.Test'Class;
                              Value   : in String;
                              Reply   : in out ASF.Responses.Mockup.Response;
                              Message : in String := "Test failed";
                              Source  : String := GNAT.Source_Info.File;
                              Line    : Natural := GNAT.Source_Info.Line);

   --  Check that the response body matches the regular expression
   procedure Assert_Matches (T       : in Util.Tests.Test'Class;
                             Pattern : in String;
                             Reply   : in out ASF.Responses.Mockup.Response;
                             Message : in String := "Test failed";
                             Status  : in Natural := ASF.Responses.SC_OK;
                             Source  : String := GNAT.Source_Info.File;
                             Line    : Natural := GNAT.Source_Info.Line);

   --  Check that the response body is a redirect to the given URI.
   procedure Assert_Redirect (T       : in Util.Tests.Test'Class;
                              Value   : in String;
                              Reply   : in out ASF.Responses.Mockup.Response;
                              Message : in String := "Test failed";
                              Source  : String := GNAT.Source_Info.File;
                              Line    : Natural := GNAT.Source_Info.Line);

end ASF.Tests;
