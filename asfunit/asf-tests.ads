-----------------------------------------------------------------------
--  asf-tests - ASF Tests Framework
--  Copyright (C) 2011, 2012, 2015, 2020, 2023 Stephane Carrez
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
with Servlet.Tests;
with Util.Tests;
with Util.XUnit;

with EL.Contexts.Default;
with EL.Variables;

with GNAT.Source_Info;

--  The <b>ASF.Tests</b> package provides a set of utility procedures to write a unit test
--  on top of ASF.
package ASF.Tests is

   --  Initialize the asf test framework mockup.  If the application is not specified,
   --  a default ASF application is created.
   procedure Initialize (Props       : in Util.Properties.Manager;
                         Application : in ASF.Applications.Main.Application_Access := null;
                         Factory     : in out ASF.Applications.Main.Application_Factory'Class);

   --  Called when the testsuite execution has finished.
   procedure Finish (Status : in Util.XUnit.Status) renames Servlet.Tests.Finish;

   --  Get the server
   function Get_Server return access ASF.Server.Container renames Servlet.Tests.Get_Server;

   --  Get the test application.
   function Get_Application return ASF.Applications.Main.Application_Access;

   --  Extract from the response output saved in `Filename` the form parameter
   --  that corresponds to the `Field` hidden field.
   function Extract (Field    : in String;
                     Filename : in String) return String;

   --  Set in the request the CSRF form parameter identified by `Field` that
   --  can be extracted from the response output saved in `Filename`.
   procedure Set_CSRF (Request  : in out ASF.Requests.Mockup.Request'Class;
                       Field    : in String;
                       Filename : in String);

   --  Simulate a GET request on the given URI with the request parameters.
   --  Get the result in the response object.
   procedure Do_Get (Request  : in out ASF.Requests.Mockup.Request'Class;
                     Response : in out ASF.Responses.Mockup.Response'Class;
                     URI      : in String;
                     Save     : in String := "") renames Servlet.Tests.Do_Get;

   --  Simulate a POST request on the given URI with the request parameters.
   --  Get the result in the response object.
   procedure Do_Post (Request  : in out ASF.Requests.Mockup.Request'Class;
                      Response : in out ASF.Responses.Mockup.Response'Class;
                      URI      : in String;
                      Save     : in String := "") renames Servlet.Tests.Do_Post;

   --  Simulate a raw request.  The URI and method must have been set on the Request object.
   procedure Do_Req (Request  : in out ASF.Requests.Mockup.Request'Class;
                     Response : in out ASF.Responses.Mockup.Response'Class)
                     renames Servlet.Tests.Do_Req;

   --  Check that the response body contains the string
   procedure Assert_Contains (T       : in Util.Tests.Test'Class;
                              Value   : in String;
                              Reply   : in out ASF.Responses.Mockup.Response;
                              Message : in String := "Test failed";
                              Source  : String := GNAT.Source_Info.File;
                              Line    : Natural := GNAT.Source_Info.Line)
                              renames Servlet.Tests.Assert_Contains;

   --  Check that the response body matches the regular expression
   procedure Assert_Matches (T       : in Util.Tests.Test'Class;
                             Pattern : in String;
                             Reply   : in out ASF.Responses.Mockup.Response;
                             Message : in String := "Test failed";
                             Status  : in Natural := ASF.Responses.SC_OK;
                             Source  : String := GNAT.Source_Info.File;
                             Line    : Natural := GNAT.Source_Info.Line)
                             renames Servlet.Tests.Assert_Matches;

   --  Check that the response body is a redirect to the given URI.
   procedure Assert_Redirect (T       : in Util.Tests.Test'Class;
                              Value   : in String;
                              Reply   : in out ASF.Responses.Mockup.Response;
                              Message : in String := "Test failed";
                              Source  : String := GNAT.Source_Info.File;
                              Line    : Natural := GNAT.Source_Info.Line)
                              renames Servlet.Tests.Assert_Redirect;

   --  Check that the response contains the given header.
   procedure Assert_Header (T       : in Util.Tests.Test'Class;
                            Header  : in String;
                            Value   : in String;
                            Reply   : in out ASF.Responses.Mockup.Response;
                            Message : in String := "Test failed";
                            Status  : in Natural := ASF.Responses.SC_OK;
                            Source  : String := GNAT.Source_Info.File;
                            Line    : Natural := GNAT.Source_Info.Line)
                            renames Servlet.Tests.Assert_Header;

   type EL_Test is new Util.Tests.Test with record
      --  The ELContext, Variables, Resolver, Form area controlled object.
      --  Due to AUnit implementation, we cannot store a controlled object in the Test object.
      --  This is due to the 'for Obj'Address use Ret;' clause used by AUnit to allocate
      --  a test object.
      --  The application object is allocated dyanmically by Set_Up.
      ELContext      : EL.Contexts.Default.Default_Context_Access;
      Variables      : EL.Variables.Variable_Mapper_Access;
      Root_Resolver  : EL.Contexts.Default.Default_ELResolver_Access;
   end record;

   --  Cleanup the test instance.
   overriding
   procedure Tear_Down (T : in out EL_Test);

   --  Setup the test instance.
   overriding
   procedure Set_Up (T : in out EL_Test);

end ASF.Tests;
