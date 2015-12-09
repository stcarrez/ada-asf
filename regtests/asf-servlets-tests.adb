-----------------------------------------------------------------------
--  Sessions Tests - Unit tests for ASF.Sessions
--  Copyright (C) 2010, 2011, 2012, 2013, 2015 Stephane Carrez
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
with Util.Measures;
with Util.Beans.Objects;

with EL.Contexts.Default;

with ASF.Applications;
with ASF.Streams;
with ASF.Routes.Servlets;
with ASF.Requests.Mockup;
with ASF.Responses.Mockup;
with ASF.Filters.Dump;
with ASF.Filters.Cache_Control;
with ASF.Filters.Tests;
with ASF.Beans.Resolvers;
with ASF.Applications.Tests;
with ASF.Routes.Servlets.Faces;
package body ASF.Servlets.Tests is

   use Util.Tests;

   procedure Do_Get (Server   : in Test_Servlet1;
                     Request  : in out Requests.Request'Class;
                     Response : in out Responses.Response'Class) is
      pragma Unreferenced (Server);

      ELContext      : aliased EL.Contexts.Default.Default_Context;
      Root_Resolver  : aliased ASF.Beans.Resolvers.ELResolver;
      Output         : ASF.Streams.Print_Stream := Response.Get_Output_Stream;
   begin
      --  Minimal setting for the EL context creation to inject URI parameters in an Ada bean.
      Root_Resolver.Initialize (null, Request'Unchecked_Access);
      ELContext.Set_Resolver (Root_Resolver'Unchecked_Access);
      Request.Inject_Parameters (ELContext);

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
   --  Check that the request is done on the good servlet and with the correct servlet path
   --  and path info.
   --  ------------------------------
   procedure Check_Request (T            : in out Test;
                            Ctx          : in Servlet_Registry;
                            URI          : in String;
                            Servlet_Path : in String;
                            Path_Info    : in String) is
      use type ASF.Routes.Route_Type_Access;

      Dispatcher : constant Request_Dispatcher
        := Ctx.Get_Request_Dispatcher (Path => URI);
      Req        : ASF.Requests.Mockup.Request;
      Resp       : ASF.Responses.Mockup.Response;
      Result     : Unbounded_String;
   begin
      T.Assert (Dispatcher.Context.Get_Route /= null, "No mapping found for " & URI);

      Req.Set_Request_URI ("test1");
      Req.Set_Method ("GET");
      Forward (Dispatcher, Req, Resp);

      Assert_Equals (T, Servlet_Path, Req.Get_Servlet_Path, "Invalid servlet path");
      Assert_Equals (T, Path_Info, Req.Get_Path_Info,
                     "The request path info is invalid");

      --  Check the response after the Test_Servlet1.Do_Get method execution.
      Resp.Read_Content (Result);
      Assert_Equals (T, ASF.Responses.SC_OK, Resp.Get_Status, "Invalid status");
      Assert_Equals (T, "URI: test1", Result, "Invalid content");

      Req.Set_Method ("POST");
      Forward (Dispatcher, Req, Resp);

      --  Assert_Equals (T, ASF.Responses.SC_METHOD_NOT_ALLOWED, Resp.Get_Status,
      --               "Invalid status for an operation not implemented");
   end Check_Request;

   --  ------------------------------
   --  Test request dispatcher and servlet invocation
   --  ------------------------------
   procedure Test_Request_Dispatcher (T : in out Test) is
      Ctx : Servlet_Registry;

      S1  : aliased Test_Servlet1;
   begin
      Ctx.Add_Servlet ("Faces", S1'Unchecked_Access);

      Ctx.Add_Mapping (Pattern => "*.jsf", Name => "Faces");

      Ctx.Add_Mapping (Pattern => "/p1/p2/p3/*", Name => "Faces");

      Check_Request (T, Ctx, "/home/test.jsf", "/home/test.jsf", "");
   end Test_Request_Dispatcher;

   --  ------------------------------
   --  Test mapping and servlet path on a request.
   --  ------------------------------
   procedure Test_Servlet_Path (T : in out Test) is
      Ctx : Servlet_Registry;

      S1  : aliased Test_Servlet1;
   begin
      Ctx.Add_Servlet ("Faces", S1'Unchecked_Access);

      Ctx.Add_Mapping (Pattern => "*.jsf", Name => "Faces");

      Ctx.Add_Mapping (Pattern => "/p1/p2/p3/*", Name => "Faces");

      Check_Request (T, Ctx, "/p1/p2/p3/home/test.html", "/p1/p2/p3", "/home/test.html");
      Check_Request (T, Ctx, "/root/home/test.jsf", "/root/home/test.jsf", "");
      Check_Request (T, Ctx, "/test.jsf", "/test.jsf", "");
   end Test_Servlet_Path;

   --  ------------------------------
   --  Test mapping and servlet path on a request.
   --  ------------------------------
   procedure Test_Filter_Mapping (T : in out Test) is
      Ctx : Servlet_Registry;

      S1  : aliased Test_Servlet1;
      S2  : aliased Test_Servlet2;
      F1  : aliased ASF.Filters.Dump.Dump_Filter;
      F2  : aliased ASF.Filters.Dump.Dump_Filter;
   begin
      Ctx.Add_Servlet ("Faces", S1'Unchecked_Access);
      Ctx.Add_Servlet ("Json", S2'Unchecked_Access);
      Ctx.Add_Filter ("Dump", F1'Unchecked_Access);
      Ctx.Add_Filter ("Dump2", F2'Unchecked_Access);
      Ctx.Add_Mapping (Pattern => "*.html", Name => "Faces");
      Ctx.Add_Mapping (Pattern => "*.json", Name => "Json");
      Ctx.Add_Filter_Mapping (Pattern => "/dump/file.html", Name => "Dump");
      Ctx.Add_Filter_Mapping (Pattern => "/dump/result/test.html", Name => "Dump");
      Ctx.Add_Filter_Mapping (Pattern => "/dump/result/test.html", Name => "Dump2");
      Ctx.Start;
      T.Check_Mapping (Ctx, "test.html", S1'Unchecked_Access);
      T.Check_Mapping (Ctx, "file.html", S1'Unchecked_Access);
      T.Check_Mapping (Ctx, "/dump/test.html", S1'Unchecked_Access);
      T.Check_Mapping (Ctx, "/dump/file.html", S1'Unchecked_Access, 1);
      T.Check_Mapping (Ctx, "/dump/result/test.html", S1'Unchecked_Access, 2);
      T.Check_Mapping (Ctx, "test.json", S2'Unchecked_Access);
   end Test_Filter_Mapping;

   --  ------------------------------
   --  Test execution of filters
   --  ------------------------------
   procedure Test_Filter_Execution (T : in out Test) is
      Ctx : Servlet_Registry;

      S1  : aliased Test_Servlet1;
      S2  : aliased Test_Servlet2;
      F1  : aliased ASF.Filters.Tests.Test_Filter;
      F2  : aliased ASF.Filters.Tests.Test_Filter;
   begin
      Ctx.Add_Servlet ("Faces", S1'Unchecked_Access);
      Ctx.Add_Servlet ("Json", S2'Unchecked_Access);
      Ctx.Add_Filter ("F1", F1'Unchecked_Access);
      Ctx.Add_Filter ("F2", F2'Unchecked_Access);
      Ctx.Add_Mapping (Pattern => "*.html", Name => "Faces");
      Ctx.Add_Mapping (Pattern => "*.json", Name => "Json");
      Ctx.Add_Filter_Mapping (Pattern => "/html/*.html", Name => "F1");
      Ctx.Add_Filter_Mapping (Pattern => "/json/*.json", Name => "F2");
      Ctx.Add_Filter_Mapping (Pattern => "/list/*.html", Name => "F1");
      Ctx.Add_Filter_Mapping (Pattern => "/list/*.json", Name => "F1");
      Ctx.Add_Filter_Mapping (Pattern => "/list/admin/*.html", Name => "F2");
      Ctx.Add_Filter_Mapping (Pattern => "/list/admin/*.json", Name => "F2");
      Ctx.Start;
      Ctx.Dump_Routes (Util.Log.INFO_LEVEL);

      --  Filter not traversed.
      T.Check_Mapping (Ctx, "test.html", S1'Unchecked_Access);
      T.Check_Mapping (Ctx, "test.json", S2'Unchecked_Access);
      T.Check_Request (Ctx, "test.html", "test.html", "");
      Assert_Equals (T, 0, F1.Counter, "Filter was not executed for *.html and *.json");
      Assert_Equals (T, 0, F2.Counter, "Filter was not executed for *.html and *.json");

      T.Check_Mapping (Ctx, "/html/test.json", S2'Unchecked_Access);
      T.Check_Request (Ctx, "/html/test.json", "/html/test.json", "");
      Assert_Equals (T, 0, F1.Counter, "Filter was executed for /html/*.json");
      Assert_Equals (T, 0, F2.Counter, "Filter was not executed for /html/*.json");

      T.Check_Mapping (Ctx, "/json/test.html", S1'Unchecked_Access);
      T.Check_Request (Ctx, "/json/test.html", "/json/test.html", "");
      Assert_Equals (T, 0, F1.Counter, "Filter was not executed for /json/*.html");
      Assert_Equals (T, 0, F2.Counter, "Filter was executed for /json/*.html");

      --  Only one filter is traversed.
      F1.Counter := 0;
      F2.Counter := 0;
      T.Check_Mapping (Ctx, "/html/test.html", S1'Unchecked_Access, 1);
      T.Check_Request (Ctx, "/html/test.html", "/html/test.html", "");
      Assert_Equals (T, 2, F1.Counter, "Filter was executed for /html/*.html");
      Assert_Equals (T, 0, F2.Counter, "Filter was not executed for /html/*.html");

      F1.Counter := 0;
      F2.Counter := 0;
      T.Check_Mapping (Ctx, "/json/test.json", S2'Unchecked_Access, 1);
      T.Check_Request (Ctx, "/json/test.json", "/json/test.json", "");
      Assert_Equals (T, 0, F1.Counter, "Filter was not executed for /json/*.json");
      Assert_Equals (T, 2, F2.Counter, "Filter was executed for /json/*.json");

      F1.Counter := 0;
      F2.Counter := 0;
      T.Check_Mapping (Ctx, "/list/test.html", S1'Unchecked_Access, 1);
      T.Check_Request (Ctx, "/list/test.html", "/list/test.html", "");
      Assert_Equals (T, 2, F1.Counter, "Filter was executed for /list/*.html");
      Assert_Equals (T, 0, F2.Counter, "Filter was not executed for /list/*.html");

      F1.Counter := 0;
      F2.Counter := 0;
      T.Check_Mapping (Ctx, "/list/test.json", S2'Unchecked_Access, 1);
      T.Check_Request (Ctx, "/list/test.json", "/list/test.json", "");
      Assert_Equals (T, 2, F1.Counter, "Filter was executed for /list/*.json");
      Assert_Equals (T, 0, F2.Counter, "Filter was not executed for /list/*.json");

      --  Both filters are traversed.
      F1.Counter := 0;
      F2.Counter := 0;
      T.Check_Mapping (Ctx, "/list/admin/test.json", S2'Unchecked_Access, 2);
      T.Check_Request (Ctx, "/list/admin/test.json", "/list/admin/test.json", "");
      Assert_Equals (T, 2, F1.Counter, "Filter was executed for /list/admin/*.json");
      Assert_Equals (T, 2, F2.Counter, "Filter was executed for /list/admin/*.json");

      F1.Counter := 0;
      F2.Counter := 0;
      T.Check_Mapping (Ctx, "/list/admin/test.html", S1'Unchecked_Access, 2);
      T.Check_Request (Ctx, "/list/admin/test.html", "/list/admin/test.html", "");
      Assert_Equals (T, 2, F1.Counter, "Filter was executed for /list/admin/*.html");
      Assert_Equals (T, 2, F2.Counter, "Filter was executed for /list/admin/*.html");
   end Test_Filter_Execution;

   --  ------------------------------
   --  Test execution of filters on complex mapping.
   --  ------------------------------
   procedure Test_Complex_Filter_Execution (T : in out Test) is
      use Util.Beans.Objects;

      Ctx     : Servlet_Registry;
      S1      : aliased Test_Servlet1;
      F1      : aliased ASF.Filters.Tests.Test_Filter;
      F2      : aliased ASF.Filters.Tests.Test_Filter;
      User    : aliased ASF.Applications.Tests.Form_Bean;
      EL_Ctx  : EL.Contexts.Default.Default_Context;
      Request : ASF.Requests.Mockup.Request;
      Reply   : ASF.Responses.Mockup.Response;
      Route   : ASF.Routes.Servlets.Faces.Faces_Route_Type_Access;
   begin
      Route := new ASF.Routes.Servlets.Faces.Faces_Route_Type;
      Route.Servlet := S1'Unchecked_Access;
      Ctx.Add_Servlet ("Faces", S1'Unchecked_Access);
      Ctx.Add_Filter ("F1", F1'Unchecked_Access);
      Ctx.Add_Filter ("F2", F2'Unchecked_Access);
      Ctx.Add_Route (Pattern   => "/wikis/#{user.name}/#{user.email}/view.html",
                     To        => Route.all'Access,
                     ELContext => EL_Ctx);

      Route := new ASF.Routes.Servlets.Faces.Faces_Route_Type;
      Route.Servlet := S1'Unchecked_Access;
      Ctx.Add_Route (Pattern   => "/wikis/#{user.name}/#{user.email}/view",
                     To        => Route.all'Access,
                     ELContext => EL_Ctx);
      Ctx.Add_Mapping (Pattern => "/wikis/*.html", Name => "Faces");
      Ctx.Add_Filter_Mapping (Pattern => "/wikis/*", Name => "F1");
      Ctx.Add_Filter_Mapping (Pattern => "/wikis/admin/*", Name => "F2");
      Ctx.Start;
      Ctx.Dump_Routes (Util.Log.INFO_LEVEL);
      Request.Set_Attribute ("user",
                             To_Object (Value => User'Unchecked_Access, Storage => STATIC));
      Request.Set_Method ("GET");
      declare
         Dispatcher : constant Request_Dispatcher
           := Ctx.Get_Request_Dispatcher (Path => "/wikis/Gandalf/Mithrandir/view.html");
         Result : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Request.Set_Request_URI ("/wikis/Gandalf/Mithrandir/view.html");
         Forward (Dispatcher, Request, Reply);

         --  Check the response after the Test_Servlet1.Do_Get method execution.
         Reply.Read_Content (Result);
         Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status, "Invalid status");
         Assert_Equals (T, "URI: /wikis/Gandalf/Mithrandir/view.html", Result, "Invalid content");
         Assert_Equals (T, "Gandalf", User.Name, "User name was not extracted from the URI");

         --  And verify that the filter are traversed.
         Assert_Equals (T, 1, F1.Counter, "Filter was executed for /html/*.html");
         Assert_Equals (T, 0, F2.Counter, "Filter was not executed for /html/*.html");
      end;

      F1.Counter := 0;
      declare
         Dispatcher : constant Request_Dispatcher
           := Ctx.Get_Request_Dispatcher (Path => "/wikis/Gandalf/Mithrandir/view");
         Result : Ada.Strings.Unbounded.Unbounded_String;
      begin
         User.Name := Ada.Strings.Unbounded.To_Unbounded_String ("");
         Request.Set_Request_URI ("/wikis/Gandalf/Mithrandir/view");
         Forward (Dispatcher, Request, Reply);

         --  Check the response after the Test_Servlet1.Do_Get method execution.
         Reply.Read_Content (Result);
         Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status, "Invalid status");
         Assert_Equals (T, "URI: /wikis/Gandalf/Mithrandir/view", Result, "Invalid content");
         Assert_Equals (T, "Gandalf", User.Name, "User name was not extracted from the URI");

         --  And verify that the filter are traversed.
         Assert_Equals (T, 1, F1.Counter, "Filter was executed for /html/*.html");
         Assert_Equals (T, 0, F2.Counter, "Filter was not executed for /html/*.html");
      end;

   end Test_Complex_Filter_Execution;

   --  ------------------------------
   --  Test execution of the cache control filter.
   --  ------------------------------
   procedure Test_Cache_Control_Filter (T : in out Test) is
      use Util.Beans.Objects;

      Ctx     : Servlet_Registry;
      S1      : aliased Test_Servlet1;
      F1      : aliased ASF.Filters.Cache_Control.Cache_Control_Filter;
      F2      : aliased ASF.Filters.Cache_Control.Cache_Control_Filter;
      User    : aliased ASF.Applications.Tests.Form_Bean;
      EL_Ctx  : EL.Contexts.Default.Default_Context;
      Request : ASF.Requests.Mockup.Request;
      Route   : ASF.Routes.Servlets.Faces.Faces_Route_Type_Access;
   begin
      Ctx.Set_Init_Parameter ("F1." & ASF.Filters.Cache_Control.CACHE_CONTROL_PARAM, "no-cache");
      Ctx.Set_Init_Parameter ("F2." & ASF.Filters.Cache_Control.CACHE_CONTROL_PARAM, "max-age: 10");

      Route := new ASF.Routes.Servlets.Faces.Faces_Route_Type;
      Route.Servlet := S1'Unchecked_Access;
      Ctx.Add_Servlet ("Faces", S1'Unchecked_Access);
      Ctx.Add_Filter ("F1", F1'Unchecked_Access);
      Ctx.Add_Route (Pattern   => "/wikis/no-cache/view.html",
                     To        => Route.all'Access,
                     ELContext => EL_Ctx);

      Route := new ASF.Routes.Servlets.Faces.Faces_Route_Type;
      Route.Servlet := S1'Unchecked_Access;
      Ctx.Add_Filter ("F2", F2'Unchecked_Access);
      Ctx.Add_Route (Pattern   => "/wikis/cache/view.html",
                     To        => Route.all'Access,
                     ELContext => EL_Ctx);

      Ctx.Add_Filter_Mapping (Pattern => "/wikis/no-cache/*", Name => "F1");
      Ctx.Add_Filter_Mapping (Pattern => "/wikis/cache/*", Name => "F2");
      Ctx.Start;
      Request.Set_Attribute ("user",
                             To_Object (Value => User'Unchecked_Access, Storage => STATIC));
      Request.Set_Method ("GET");
      declare
         Dispatcher : constant Request_Dispatcher
           := Ctx.Get_Request_Dispatcher (Path => "/wikis/no-cache/view.html");
         Result : Ada.Strings.Unbounded.Unbounded_String;
         Reply  : ASF.Responses.Mockup.Response;
      begin
         Request.Set_Request_URI ("/wikis/no-cache/view.html");
         Forward (Dispatcher, Request, Reply);

         --  Check the response after the Test_Servlet1.Do_Get method execution.
         Reply.Read_Content (Result);
         Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status, "Invalid status");
         T.Assert (Reply.Contains_Header ("Cache-Control"),
                   "A Cache-Control is missing in the response");
         Assert_Equals (T, "no-cache", Reply.Get_Header ("Cache-Control"),
                        "Invalid Cache-Control header");
      end;

      declare
         Dispatcher : constant Request_Dispatcher
           := Ctx.Get_Request_Dispatcher (Path => "/wikis/cache/view.html");
         Result : Ada.Strings.Unbounded.Unbounded_String;
         Reply  : ASF.Responses.Mockup.Response;
      begin
         Request.Set_Request_URI ("/wikis/cache/view.html");
         Forward (Dispatcher, Request, Reply);

         --  Check the response after the Test_Servlet1.Do_Get method execution.
         Reply.Read_Content (Result);
         Assert_Equals (T, ASF.Responses.SC_OK, Reply.Get_Status, "Invalid status");
         T.Assert (Reply.Contains_Header ("Cache-Control"),
                   "A Cache-Control is missing in the response");
         Assert_Equals (T, "max-age: 10", Reply.Get_Header ("Cache-Control"),
                        "Invalid Cache-Control header");
      end;
   end Test_Cache_Control_Filter;

   --  ------------------------------
   --  Test add servlet
   --  ------------------------------
   procedure Test_Add_Servlet (T : in out Test) is
      Ctx : Servlet_Registry;

      S1  : aliased Test_Servlet1;
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
   --  Test getting a resource path
   --  ------------------------------
   procedure Test_Get_Resource (T : in out Test) is
      Ctx : Servlet_Registry;

      Conf  : Applications.Config;
      S1    : aliased Test_Servlet1;
      Dir   : constant String := "regtests/files";
      Path  : constant String := Util.Tests.Get_Path (Dir);
   begin
      Conf.Load_Properties ("regtests/view.properties");
      Conf.Set ("view.dir", Path);
      Ctx.Set_Init_Parameters (Conf);
      Ctx.Add_Servlet ("Faces", S1'Unchecked_Access);

      --  Resource exist, check the returned path.
      declare
         P : constant String := Ctx.Get_Resource ("/tests/form-text.xhtml");
      begin
         Assert_Matches (T, ".*/regtests/files/tests/form-text.xhtml",
                         P, "Invalid resource path");
      end;

      --  Resource does not exist
      declare
         P : constant String := Ctx.Get_Resource ("/tests/form-text-missing.xhtml");
      begin
         Assert_Equals (T, "", P, "Invalid resource path for missing resource");
      end;
   end Test_Get_Resource;

   --  ------------------------------
   --  Check that the mapping for the given URI matches the server.
   --  ------------------------------
   procedure Check_Mapping (T      : in out Test;
                            Ctx    : in Servlet_Registry;
                            URI    : in String;
                            Server : in Servlet_Access;
                            Filter : in Natural := 0) is
      use type ASF.Routes.Route_Type_Access;
      use type ASF.Filters.Filter_List_Access;

      Disp  : constant Request_Dispatcher := Ctx.Get_Request_Dispatcher (URI);
      Route : constant ASF.Routes.Route_Type_Access := Disp.Context.Get_Route;
      Servlet_Route : ASF.Routes.Servlets.Servlet_Route_Type_Access;
   begin
      if Server = null then
         T.Assert (Route = null, "No mapping returned for URI: " & URI);
      else
         T.Assert (Route /= null, "A mapping is returned for URI: " & URI);
         T.Assert (Route.all in ASF.Routes.Servlets.Servlet_Route_Type'Class,
                   "The route is not a Servlet route");
         Servlet_Route := ASF.Routes.Servlets.Servlet_Route_Type'Class (Route.all)'Access;
         T.Assert (Servlet_Route.Servlet = Server,
                   "Invalid mapping returned for URI: " & URI);
         if Filter = 0 then
            T.Assert (Disp.Filters = null,
                      "Filters are configured for URI: " & URI);
         else
            T.Assert (Disp.Filters /= null, "No filter on the route URI: " & URI);
            Util.Tests.Assert_Equals (T, Filter, Disp.Filters'Length,
                                      "Invalid mapping returned for URI: " & URI);
         end if;
      end if;
   end Check_Mapping;

   --  ------------------------------
   --  Test session creation.
   --  ------------------------------
   procedure Test_Create_Servlet (T : in out Test) is
      Ctx : Servlet_Registry;
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

      --  Ctx.Mappings.Dump_Map (" ");

      T.Check_Mapping (Ctx, "/joe/black/joe.jsf", S1'Access);
      T.Check_Mapping (Ctx, "/joe/black/joe.txt", S2'Access);
      T.Check_Mapping (Ctx, "/server/info", S1'Access);
      T.Check_Mapping (Ctx, "/server/list2", S2'Access);
      T.Check_Mapping (Ctx, "/1/2/3/4/5/6/7/8/9/server/list2", S2'Access);
      T.Check_Mapping (Ctx, "/1/2/3/4/5/6/7/8/A/server/list2", S1'Access);

      declare
         use type ASF.Routes.Route_Type_Access;

         St : Util.Measures.Stamp;
      begin
         for I in 1 .. 1000 loop
            declare
               Disp : constant Request_Dispatcher
                 := Ctx.Get_Request_Dispatcher (Path => "/joe/black/joe.jsf");
            begin
               T.Assert (Disp.Context.Get_Route /= null,
                         "No mapping found for /joe/black/joe.jsf");
            end;
         end loop;
         Util.Measures.Report (St, "Find 1000 mapping (extension)");
      end;

--        T.Assert (Map /= null, "No mapping for 'joe.jsf'");
--        T.Assert (Map.Servlet /= null, "No servlet for mapping for 'joe.jsf'");
--        T.Assert (Map.Servlet = S1'Access, "Invalid servlet");
      --        Util.Measures.Report (St, "10 Session create");

--        declare
--           St : Util.Measures.Stamp;
--        begin
--           for I in 1 .. 1000 loop
--              Map := Ctx.Find_Mapping (URI => "/1/2/3/4/5/6/7/8/9/server/list2");
--           end loop;
--           Util.Measures.Report (St, "Find 1000 mapping (path)");
--        end;
--
--        T.Assert (Map /= null, "No mapping for '/server/john/joe.jsf'");
--        T.Assert (Map.Servlet /= null, "No servlet for mapping for 'joe.jsf'");
--        T.Assert (Map.Servlet = S2'Access, "Invalid servlet");
      --        Util.Measures.Report (St, "10 Session create");

   end Test_Create_Servlet;

   package Caller is new Util.Test_Caller (Test, "Servlets");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is

   begin
      --  To document what is tested, register the test methods for each
      --  operation that is tested.
      Caller.Add_Test (Suite, "Test ASF.Servlets.Add_Mapping,Find_Mapping",
                       Test_Create_Servlet'Access);
      Caller.Add_Test (Suite, "Test ASF.Servlets.Add_Servlet",
                       Test_Add_Servlet'Access);
      Caller.Add_Test (Suite, "Test ASF.Servlets.Get_Request_Dispatcher",
                       Test_Request_Dispatcher'Access);
      Caller.Add_Test (Suite, "Test ASF.Servlets.Get_Resource",
                       Test_Get_Resource'Access);
      Caller.Add_Test (Suite, "Test ASF.Requests.Get_Servlet_Path",
                       Test_Servlet_Path'Access);
      Caller.Add_Test (Suite, "Test ASF.Servlets.Add_Filter",
                       Test_Filter_Mapping'Access);
      Caller.Add_Test (Suite, "Test ASF.Filters.Do_Filter",
                       Test_Filter_Execution'Access);
      Caller.Add_Test (Suite, "Test ASF.Filters.Do_Filter (complex)",
                       Test_Complex_Filter_Execution'Access);
      Caller.Add_Test (Suite, "Test ASF.Filters.Cache_Control.Do_Filter",
                       Test_Cache_Control_Filter'Access);
   end Add_Tests;

end ASF.Servlets.Tests;
