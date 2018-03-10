-----------------------------------------------------------------------
--  ASF tests - ASF Tests Framework
--  Copyright (C) 2011, 2012, 2013, 2015, 2017, 2018 Stephane Carrez
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

with GNAT.Regpat;

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Util.Files;

with ASF.Streams;
with ASF.Servlets.Faces;
with Servlet.Core.Files;
with ASF.Servlets.Ajax;
with Servlet.Core.Measures;
with ASF.Responses;
with ASF.Responses.Tools;

with ASF.Filters.Dump;
with ASF.Contexts.Faces;
with EL.Variables.Default;

package body ASF.Tests is

   use Ada.Strings.Unbounded;
   use Util.Tests;

   CONTEXT_PATH : constant String := "/asfunit";

   type Container_Access is access ASF.Server.Container;

   App_Created : ASF.Applications.Main.Application_Access;
   App         : ASF.Applications.Main.Application_Access;
   Faces       : aliased ASF.Servlets.Faces.Faces_Servlet;
   Ajax        : aliased ASF.Servlets.Ajax.Ajax_Servlet;
   Measures    : aliased Servlet.Core.Measures.Measure_Servlet;


   --  ------------------------------
   --  Initialize the awa test framework mockup.
   --  ------------------------------
   procedure Initialize (Props       : in Util.Properties.Manager;
                         Application : in ASF.Applications.Main.Application_Access := null;
                         Factory     : in out ASF.Applications.Main.Application_Factory'Class) is
      use type ASF.Applications.Main.Application_Access;

      C        : ASF.Applications.Config;
      Empty    : Util.Properties.Manager;
   begin
      if Application /= null then
         App := Application;
      else
         if App_Created = null then
            App_Created := new ASF.Applications.Main.Application;
         end if;
         App := App_Created;
      end if;

      C.Copy (Props);
      App.Initialize (C, Factory);
      App.Register ("layoutMsg", "layout");
      App.Set_Global ("contextPath", CONTEXT_PATH);

      --  Register the servlets and filters
      App.Add_Servlet (Name => "faces", Server => Faces'Access);
      App.Add_Servlet (Name => "ajax", Server => Ajax'Access);

      --  Define servlet mappings
      App.Add_Mapping (Name => "faces", Pattern => "*.html");
      App.Add_Mapping (Name => "ajax", Pattern => "/ajax/*");

      Servlet.Tests.Initialize (Empty, CONTEXT_PATH, App.all'Access);

   end Initialize;

   --  ------------------------------
   --  Get the test application.
   --  ------------------------------
   function Get_Application return ASF.Applications.Main.Application_Access is
   begin
      return App;
   end Get_Application;

   --  ------------------------------
   --  Cleanup the test instance.
   --  ------------------------------
   overriding
   procedure Tear_Down (T : in out EL_Test) is
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Contexts.Default.Default_Context'Class,
                                        EL.Contexts.Default.Default_Context_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Variables.Variable_Mapper'Class,
                                        EL.Variables.Variable_Mapper_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Contexts.Default.Default_ELResolver'Class,
                                        EL.Contexts.Default.Default_ELResolver_Access);
   begin
      ASF.Contexts.Faces.Restore (null);
      Free (T.ELContext);
      Free (T.Variables);
      Free (T.Root_Resolver);
   end Tear_Down;

   --  ------------------------------
   --  Setup the test instance.
   --  ------------------------------
   overriding
   procedure Set_Up (T : in out EL_Test) is
   begin
      T.ELContext     := new EL.Contexts.Default.Default_Context;
      T.Root_Resolver := new EL.Contexts.Default.Default_ELResolver;
      T.Variables     := new EL.Variables.Default.Default_Variable_Mapper;
      T.ELContext.Set_Resolver (T.Root_Resolver.all'Access);
      T.ELContext.Set_Variable_Mapper (T.Variables.all'Access);
   end Set_Up;

end ASF.Tests;
