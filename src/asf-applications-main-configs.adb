-----------------------------------------------------------------------
--  applications-main-configs -- Configuration support for ASF Applications
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2017, 2018 Stephane Carrez
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
with Util.Serialize.IO.XML;

with EL.Functions.Namespaces;

with ASF.Navigations;
with ASF.Navigations.Mappers;
with Servlet.Core.Mappers;
with ASF.Servlets.Faces.Mappers;
with ASF.Beans.Mappers;
with ASF.Views.Nodes.Core;

package body ASF.Applications.Main.Configs is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Applications.Main.Configs");

   function Get_Locale (Value : in Util.Beans.Objects.Object) return Util.Locales.Locale;

   function Get_Locale (Value : in Util.Beans.Objects.Object) return Util.Locales.Locale is
      Name   : constant String := Util.Beans.Objects.To_String (Value);
      Locale : constant Util.Locales.Locale := Util.Locales.Get_Locale (Name);
   begin
      return Locale;
   end Get_Locale;

   --  ------------------------------
   --  Save in the application config object the value associated with the given field.
   --  When the <b>TAG_MESSAGE_BUNDLE</b> field is reached, insert the new bundle definition
   --  in the application.
   --  ------------------------------
   procedure Set_Member (N     : in out Application_Config;
                         Field : in Application_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when TAG_MESSAGE_VAR =>
            N.Name := Value;

         when TAG_MESSAGE_BUNDLE =>
            declare
               Bundle : constant String := Util.Beans.Objects.To_String (Value);
            begin
               if Util.Beans.Objects.Is_Null (N.Name) then
                  N.App.Register (Name   => Bundle & "Msg",
                                  Bundle => Bundle);
               else
                  N.App.Register (Name   => Util.Beans.Objects.To_String (N.Name),
                                  Bundle => Bundle);
               end if;
               N.Name := Util.Beans.Objects.Null_Object;
            end;

         when TAG_DEFAULT_LOCALE =>
            N.App.Set_Default_Locale (Get_Locale (Value));

         when TAG_SUPPORTED_LOCALE =>
            N.App.Add_Supported_Locale (Get_Locale (Value));

      end case;
   end Set_Member;

   AMapper : aliased Application_Mapper.Mapper;

   --  ------------------------------
   --  Setup the XML parser to read the managed bean definitions.
   --  By instantiating this package, the <b>Reader</b> gets populated with the XML mappings
   --  to read the servlet, managed beans and navigation rules.
   --  ------------------------------
   package body Reader_Config is
      procedure Set_Property_Context (Params : in Util.Properties.Manager'Class);

      --  Get the navigation handler for the Navigation_Config instantiation
      --  GNAT crashes if the call is made in the instantiation part.
      Nav     : constant ASF.Navigations.Navigation_Handler_Access := App.Get_Navigation_Handler;

      package Bean_Config is
        new ASF.Beans.Mappers.Reader_Config (Mapper, App.Factory'Access,
                                             Context.all'Access);
      package Navigation_Config is
        new ASF.Navigations.Mappers.Reader_Config (Mapper, Nav, Context.all'Access);

      package Servlet_Config is
        new Servlet.Core.Mappers.Reader_Config (Mapper, App.all'Access,
                                                Context.all'Access);
      package Faces_Config is
        new ASF.Servlets.Faces.Mappers.Reader_Config (Mapper, App.all'Access,
                                                      Context.all'Access);
      pragma Warnings (Off, Bean_Config);
      pragma Warnings (Off, Navigation_Config);
      pragma Warnings (Off, Faces_Config);

      Config    : aliased Application_Config;
      NS_Mapper : aliased EL.Functions.Namespaces.NS_Function_Mapper;

      procedure Set_Property_Context (Params : in Util.Properties.Manager'Class) is
      begin
         Prop_Context.Set_Properties (Params);
      end Set_Property_Context;

   begin
      --  Install the property context that gives access
      --  to the application configuration properties
      App.Get_Init_Parameters (Set_Property_Context'Access);
      Context.Set_Resolver (Prop_Context'Unchecked_Access);

      --  Setup the function mapper to resolve uses of functions within EL expressions.
      NS_Mapper.Set_Namespace (Prefix => "fn",
                               URI    => ASF.Views.Nodes.Core.FN_URI);
      NS_Mapper.Set_Function_Mapper (App.Functions'Unchecked_Access);
      Context.Set_Function_Mapper (NS_Mapper'Unchecked_Access);

      Mapper.Add_Mapping ("faces-config", AMapper'Access);
      Mapper.Add_Mapping ("module", AMapper'Access);
      Mapper.Add_Mapping ("web-app", AMapper'Access);
      Config.App := App;
      Servlet_Config.Config.Override_Context := Override_Context;
      Application_Mapper.Set_Context (Mapper, Config'Unchecked_Access);
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
      Mapper  : Util.Serialize.Mappers.Processing;
      Context : aliased EL.Contexts.Default.Default_Context;

      --  Setup the <b>Reader</b> to parse and build the configuration for managed beans,
      --  navigation rules, servlet rules.  Each package instantiation creates a local variable
      --  used while parsing the XML file.
      package Config is
        new Reader_Config (Mapper, App'Unchecked_Access, Context'Unchecked_Access);

      pragma Warnings (Off, Config);
   begin
      Log.Info ("Reading module configuration file {0}", File);

      --        Util.Serialize.IO.Dump (Reader, AWA.Modules.Log);

      --  Read the configuration file and record managed beans, navigation rules.
      Reader.Parse (File, Mapper);
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

begin
   AMapper.Add_Mapping ("application/message-bundle/@var", TAG_MESSAGE_VAR);
   AMapper.Add_Mapping ("application/message-bundle", TAG_MESSAGE_BUNDLE);
   AMapper.Add_Mapping ("application/locale-config/default-locale", TAG_DEFAULT_LOCALE);
   AMapper.Add_Mapping ("application/locale-config/supported-locale", TAG_SUPPORTED_LOCALE);
end ASF.Applications.Main.Configs;
