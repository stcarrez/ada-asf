-----------------------------------------------------------------------
--  Sessions Tests - Unit tests for ASF.Sessions
--  Copyright (C) 2010 Stephane Carrez
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

with AUnit.Test_Caller;
with Ada.Text_IO;
with ASF.Sessions.Factory;
with Ada.Calendar;
with Util.Tests;
with Util.Measures;
with EL.Objects;

with ASF.Streams;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
package body ASF.Servlets.Tests is

   use Util.Tests;

   procedure Do_Get (Server   : in Test_Servlet1;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class) is
      Output : ASF.Streams.Print_Stream := Response.Get_Output_Stream;
   begin
      Output.Write ("URI: " & Request.Get_Request_URI);
      Response.Set_Status (Responses.SC_OK);
   end Do_Get;

   procedure Do_Post (Server   : in Test_Servlet2;
                      Request  : in out Requests.Request'Class;
                      Response : in out Responses.Response'Class) is
   begin
      null;
   end Do_Post;

   S1 : aliased Test_Servlet1;
   S2 : aliased Test_Servlet2;

   --  ------------------------------
   --  Test request dispatcher and servlet invocation
   --  ------------------------------
   procedure Test_Request_Dispatcher (T : in out Test) is
      Ctx : Servlet_Registry;

      S1  : aliased Test_Servlet1;
      S2  : aliased Test_Servlet2;
   begin
      Ctx.Add_Servlet ("Faces", S1'Unchecked_Access);

      Ctx.Add_Mapping (Pattern => "*.jsf", Name => "Faces");

      declare
         Dispatcher : Request_Dispatcher := Ctx.Get_Request_Dispatcher (Path => "/home/test.jsf");
         Req        : ASF.Requests.Mockup.Request;
         Resp       : ASF.Responses.Mockup.Response;
         Result     : Unbounded_String;
      begin
         Assert (T, Dispatcher.Mapping /= null, "No mapping found");

         Req.Set_Request_URI ("test1");
         Req.Set_Method ("GET");
         Forward (Dispatcher, Req, Resp);

         --  Check the response after the Test_Servlet1.Do_Get method execution.
         Resp.Read_Content (Result);
         Assert_Equals (T, ASF.Responses.SC_OK, Resp.Get_Status, "Invalid status");
         Assert_Equals (T, "URI: test1", Result, "Invalid content");

         Req.Set_Method ("POST");
         Forward (Dispatcher, Req, Resp);

         Assert_Equals (T, ASF.Responses.SC_METHOD_NOT_ALLOWED, Resp.Get_Status,
                        "Invalid status for an operation not implemented");

      end;
   end Test_Request_Dispatcher;

   --  ------------------------------
   --  Test add servlet
   --  ------------------------------
   procedure Test_Add_Servlet (T : in out Test) is
      Ctx : Servlet_Registry;

      S1  : aliased Test_Servlet1;
      S2  : aliased Test_Servlet2;
   begin
      Ctx.Add_Servlet ("Faces", S1'Unchecked_Access);

      Assert_Equals (T, "Faces", S1.Get_Name, "Invalid name for the servlet");

      begin
         Ctx.Add_Servlet ("Faces", S1'Unchecked_Access);
         T.Assert (False, "No exception raised if the servlet is registered several times");

      exception
         when Servlet_Error =>
            null;
      end;
   end Test_Add_Servlet;

   --  ------------------------------
   --  Check that the mapping for the given URI matches the server.
   --  ------------------------------
   procedure Check_Mapping (T      : in out Test;
                            Ctx    : in Servlet_Registry;
                            URI    : in String;
                            Server : in Servlet_Access) is
      Map : Mapping_Access := Ctx.Find_Mapping (URI);
   begin
      if Map = null then
         T.Assert (Server = null, "No mapping returned for URI: " & URI);
      else
         T.Assert (Server /= null, "A mapping is returned for URI: " & URI);
         T.Assert (Map.Servlet = Server, "Invalid mapping returned for URI: " & URI);
      end if;
   end Check_Mapping;

   --  ------------------------------
   --  Test session creation.
   --  ------------------------------
   procedure Test_Create_Servlet (T : in out Test) is
      Ctx : Servlet_Registry;

      Map : Mapping_Access;
   begin
      Ctx.Add_Servlet (Name => "Faces", Server => S1'Access);
      Ctx.Add_Servlet (Name => "Text", Server => S2'Access);

      Ctx.Add_Mapping (Pattern => "*.jsf", Name => "Faces");
      Ctx.Add_Mapping (Pattern => "*.html", Name => "Faces");
      Ctx.Add_Mapping (Pattern => "*.txt", Name => "Text");

      --  Ctx.Add_Mapping (Pattern => "/server", Server => S2'Access);
      Ctx.Add_Mapping (Pattern => "/server/john/*", Server => S2'Access);
      Ctx.Add_Mapping (Pattern => "/server/info", Server => S1'Access);
      Ctx.Add_Mapping (Pattern => "/server/list", Server => S1'Access);
      Ctx.Add_Mapping (Pattern => "/server/list2", Server => S2'Access);
      Ctx.Add_Mapping (Pattern => "/1/2/3/4/5/6/7/8/9/server/list2", Server => S2'Access);
      Ctx.Add_Mapping (Pattern => "/1/2/3/4/5/6/7/8/A/server/list2", Server => S1'Access);

      Ctx.Mappings.Dump_Map (" ");

      T.Check_Mapping (Ctx, "/joe/black/joe.jsf", S1'Access);
      T.Check_Mapping (Ctx, "/joe/black/joe.txt", S2'Access);
      T.Check_Mapping (Ctx, "/server/info", S1'Access);
      T.Check_Mapping (Ctx, "/server/list2", S2'Access);
      T.Check_Mapping (Ctx, "/1/2/3/4/5/6/7/8/9/server/list2", S2'Access);
      T.Check_Mapping (Ctx, "/1/2/3/4/5/6/7/8/A/server/list2", S1'Access);

      declare
         St : Util.Measures.Stamp;
      begin
         for I in 1 .. 1000 loop
            Map := Ctx.Find_Mapping (URI => "/joe/black/joe.jsf");
         end loop;
         Util.Measures.Report (St, "Find 1000 mapping (extension)");
      end;

      T.Assert (Map /= null, "No mapping for 'joe.jsf'");
      T.Assert (Map.Servlet /= null, "No servlet for mapping for 'joe.jsf'");
      T.Assert (Map.Servlet = S1'Access, "Invalid servlet");
      --        Util.Measures.Report (St, "10 Session create");

      declare
         St : Util.Measures.Stamp;
      begin
         for I in 1 .. 1000 loop
            Map := Ctx.Find_Mapping (URI => "/1/2/3/4/5/6/7/8/9/server/list2");
         end loop;
         Util.Measures.Report (St, "Find 1000 mapping (path)");
      end;

      T.Assert (Map /= null, "No mapping for '/server/john/joe.jsf'");
      T.Assert (Map.Servlet /= null, "No servlet for mapping for 'joe.jsf'");
      T.Assert (Map.Servlet = S2'Access, "Invalid servlet");
      --        Util.Measures.Report (St, "10 Session create");

   end Test_Create_Servlet;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is

   begin
      --  To document what is tested, register the test methods for each
      --  operation that is tested.
      Suite.Add_Test (Caller.Create ("Test ASF.Servlets.Add_Mapping,Find_Mapping",
                                      Test_Create_Servlet'Access));
      Suite.Add_Test (Caller.Create ("Test ASF.Servlets.Add_Servlet",
                                      Test_Add_Servlet'Access));
      Suite.Add_Test (Caller.Create ("Test ASF.Servlets.Get_Request_Dispatcher",
                                      Test_Request_Dispatcher'Access));
   end Add_Tests;

end ASF.Servlets.Tests;
