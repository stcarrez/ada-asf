-----------------------------------------------------------------------
--  applications-main-configs -- Configuration support for ASF Applications
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with Util.Log.Loggers;

with ASF.Navigations;
with ASF.Navigations.Mappers;
with ASF.Servlets.Mappers;
with ASF.Beans.Mappers;

package body ASF.Applications.Main.Configs is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Applications.Main.Configs");

   --  ------------------------------
   --  Setup the XML parser to read the managed bean definitions.
   --  By instantiating this package, the <b>Reader</b> gets populated with the XML mappings
   --  to read the servlet, managed beans and navigation rules.
   --  ------------------------------
   package body Reader_Config is
      --  Get the navigation handler for the Navigation_Config instantiation
      --  GNAT crashes if the call is made in the instantation part.
      Nav     : constant ASF.Navigations.Navigation_Handler_Access := App.Get_Navigation_Handler;

      package Bean_Config is
        new ASF.Beans.Mappers.Reader_Config (Reader, App.Factory'Access,
                                             Context.all'Access);
      package Navigation_Config is
        new ASF.Navigations.Mappers.Reader_Config (Reader, Nav, Context.all'Access);

      package Servlet_Config is
        new ASF.Servlets.Mappers.Reader_Config (Reader, App.all'Access,
                                                Context.all'Access);
      pragma Warnings (Off, Bean_Config);
      pragma Warnings (Off, Navigation_Config);
      pragma Warnings (Off, Servlet_Config);

   begin
      --  Install the property context that gives access
      --  to the application configuration properties
      Prop_Context.Set_Properties (App.Conf);
      Context.Set_Resolver (Prop_Context'Unchecked_Access);
   end Reader_Config;

   --  ------------------------------
   --  Read the configuration file associated with the application.  This includes:
   --  <ul>
   --     <li>The servlet and filter mappings</li>
   --     <li>The managed bean definitions</li>
   --     <li>The navigation rules</li>
   --  </ul>
   --  ------------------------------
   procedure Read_Configuration (App  : in out Application'Class;
                                 File : in String) is

      Reader  : Util.Serialize.IO.XML.Parser;
      Context : aliased EL.Contexts.Default.Default_Context;

      --  Setup the <b>Reader</b> to parse and build the configuration for managed beans,
      --  navigation rules, servlet rules.  Each package instantiation creates a local variable
      --  used while parsing the XML file.
      package Config is
        new Reader_Config (Reader, App'Unchecked_Access, Context'Unchecked_Access);

      pragma Warnings (Off, Config);
   begin
      Log.Info ("Reading module configuration file {0}", File);

      --        Util.Serialize.IO.Dump (Reader, AWA.Modules.Log);

      --  Read the configuration file and record managed beans, navigation rules.
      Reader.Parse (File);
   end Read_Configuration;

   --  ------------------------------
   --  Create the configuration parameter definition instance.
   --  ------------------------------
   package body Parameter is
      PARAM_NAME  : aliased constant String := Name;
      PARAM_VALUE : aliased constant String := Default;
      Param       : constant Config_Param := ASF.Applications.P '(Name => PARAM_NAME'Access,
                                                                  Default => PARAM_VALUE'Access);

      --  ------------------------------
      --  Returns the configuration parameter.
      --  ------------------------------
      function P return Config_Param is
      begin
         return Param;
      end P;

   end Parameter;

end ASF.Applications.Main.Configs;
