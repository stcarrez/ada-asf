-----------------------------------------------------------------------
--  ASF tests - ASF Tests Framework
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

with Ada.Strings.Unbounded;

with Util.Tests;
with Util.Files;

with ASF.Servlets.Faces;
with ASF.Servlets.Files;
with ASF.Servlets.Measures;
with ASF.Responses;
with ASF.Responses.Tools;

with ASF.Filters.Dump;

package body ASF.Tests is

   use Ada.Strings.Unbounded;

   CONTEXT_PATH : constant String := "/awa";

   Server   : access ASF.Server.Container;

   App      : ASF.Applications.Main.Application_Access := null;
   Fact     : ASF.Applications.Main.Application_Factory;
   Faces    : aliased ASF.Servlets.Faces.Faces_Servlet;
   Files    : aliased ASF.Servlets.Files.File_Servlet;
   Dump     : aliased ASF.Filters.Dump.Dump_Filter;
   Measures : aliased ASF.Servlets.Measures.Measure_Servlet;

   --  ------------------------------
   --  Initialize the awa test framework mockup.
   --  ------------------------------
   procedure Initialize (Props       : in Util.Properties.Manager;
                         Application : in ASF.Applications.Main.Application_Access := null) is
      use type ASF.Applications.Main.Application_Access;

      C        : ASF.Applications.Config;
      Database : constant String := Props.Get ("test.database");
   begin
      if Application /= null then
         App := Application;
      else
         App := new ASF.Applications.Main.Application;
      end if;

      Server := new ASF.Server.Container;
      Server.Register_Application (CONTEXT_PATH, App.all'Access);

      C.Copy (Props);
      C.Set ("database", Database);
      App.Initialize (C, Fact);
      App.Register ("layoutMsg", "layout");
      App.Set_Global ("contextPath", "/awa");

      --  Register the servlets and filters
      App.Add_Servlet (Name => "faces", Server => Faces'Access);
      App.Add_Servlet (Name => "files", Server => Files'Access);
      App.Add_Servlet (Name => "measures", Server => Measures'Access);
      App.Add_Filter (Name => "dump", Filter => Dump'Access);
      App.Add_Filter (Name => "measures", Filter => Measures'Access);

      --  Define servlet mappings
      App.Add_Mapping (Name => "faces", Pattern => "*.html");
      App.Add_Mapping (Name => "files", Pattern => "*.css");
      App.Add_Mapping (Name => "files", Pattern => "*.js");
      App.Add_Mapping (Name => "measures", Pattern => "stats.xml");

      App.Add_Filter_Mapping (Name => "measures", Pattern => "*");
      App.Add_Filter_Mapping (Name => "dump", Pattern => "*.html");
      App.Add_Filter_Mapping (Name => "dump", Pattern => "*.css");
   end Initialize;

   --  ------------------------------
   --  Get the server
   --  ------------------------------
   function Get_Server return access ASF.Server.Container is
   begin
      return Server;
   end Get_Server;

   --  ------------------------------
   --  Get the test application.
   --  ------------------------------
   function Get_Application return ASF.Applications.Main.Application_Access is
   begin
      return App;
   end Get_Application;

   --  ------------------------------
   --  Save the response headers and content in a file
   --  ------------------------------
   procedure Save_Response (Name     : in String;
                            Response : in out ASF.Responses.Mockup.Response) is
      use ASF.Responses;

      Info        : constant String := Tools.To_String (Reply         => Response,
                                                        Html          => False,
                                                        Print_Headers => True);
      Result_Path : constant String := Util.Tests.Get_Test_Path ("regtests/result");
      Content     : Unbounded_String;
   begin
      Response.Read_Content (Content);
      Insert (Content, 1, Info);
      Util.Files.Write_File (Result_Path & "/" & Name, Content);
   end Save_Response;

   procedure Do_Req (Request  : in out ASF.Requests.Mockup.Request;
                     Response : in out ASF.Responses.Mockup.Response) is
   begin
      Server.Service (Request  => Request,
                      Response => Response);
   end Do_Req;

   --  ------------------------------
   --  Simulate a GET request on the given URI with the request parameters.
   --  Get the result in the response object.
   --  ------------------------------
   procedure Do_Get (Request  : in out ASF.Requests.Mockup.Request;
                     Response : in out ASF.Responses.Mockup.Response;
                     URI      : in String;
                     Save     : in String := "") is
   begin
      Request.Set_Method (Method => "GET");
      Request.Set_Request_URI (URI => CONTEXT_PATH & URI);
      Request.Set_Protocol (Protocol => "HTTP/1.1");
      Do_Req (Request, Response);

      if Save'Length > 0 then
         Save_Response (Save, Response);
      end if;
   end Do_Get;

   --  ------------------------------
   --  Simulate a POST request on the given URI with the request parameters.
   --  Get the result in the response object.
   --  ------------------------------
   procedure Do_Post (Request  : in out ASF.Requests.Mockup.Request;
                      Response : in out ASF.Responses.Mockup.Response;
                      URI      : in String;
                      Save     : in String := "") is
   begin
      Request.Set_Method (Method => "POST");
      Request.Set_Request_URI (URI => CONTEXT_PATH & URI);
      Request.Set_Protocol (Protocol => "HTTP/1.1");
      Do_Req (Request, Response);

      if Save'Length > 0 then
         Save_Response (Save, Response);
      end if;
   end Do_Post;

end ASF.Tests;
