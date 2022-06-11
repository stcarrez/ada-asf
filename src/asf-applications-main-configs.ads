-----------------------------------------------------------------------
--  applications-main-configs -- Configuration support for ASF Applications
--  Copyright (C) 2009, 2010, 2011, 2012, 2015, 2017, 2018, 2022 Stephane Carrez
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

with EL.Contexts.Default;
with EL.Contexts.Properties;

with ASF.Contexts.Faces;

with Util.Beans.Objects;
with Util.Serialize.Mappers.Record_Mapper;
package ASF.Applications.Main.Configs is

   --  Read the configuration file associated with the application.  This includes:
   --  <ul>
   --     <li>The servlet and filter mappings</li>
   --     <li>The managed bean definitions</li>
   --     <li>The navigation rules</li>
   --  </ul>
   procedure Read_Configuration (App  : in out Application'Class;
                                 File : in String);

   --  Setup the XML parser to read the managed bean definitions.
   --  By instantiating this package, the <b>Reader</b> gets populated with the XML mappings
   --  to read the servlet, managed beans and navigation rules.
   generic
      Mapper  : in out Util.Serialize.Mappers.Processing;
      App     : in ASF.Contexts.Faces.Application_Access;
      Context : in EL.Contexts.Default.Default_Context_Access;
      Override_Context : in Boolean := False;
   package Reader_Config is
      Prop_Context : aliased EL.Contexts.Properties.Property_Resolver;
   end Reader_Config;

   --  Create the configuration parameter definition instance.
   generic
      --  The parameter name.
      Name    : in String;

      --  The default value.
      Default : in String;
   package Parameter is

      --  Returns the configuration parameter.
      function P return Config_Param;
      pragma Inline_Always (P);
   end Parameter;

   --  ------------------------------
   --  Application Specific Configuration
   --  ------------------------------
   --  Read the application specific configuration by using the XML mapper.
   --  The application configuration looks like:
   --
   --    <application>
   --      <message-bundle>name</message-bundle>
   --      <message-bundle var='name'>bundle-name</message-bundle>
   --    </application>
   --
   type Application_Config is limited record
      Name    : Util.Beans.Objects.Object;
      App     : ASF.Contexts.Faces.Application_Access;
   end record;
   type Application_Config_Access is access all Application_Config;

   type Application_Fields is (TAG_MESSAGE_BUNDLE, TAG_MESSAGE_VAR,
                               TAG_DEFAULT_LOCALE, TAG_SUPPORTED_LOCALE);

   --  Save in the application config object the value associated with the given field.
   --  When the <b>TAG_MESSAGE_BUNDLE</b> field is reached, insert the new bundle definition
   --  in the application.
   procedure Set_Member (N     : in out Application_Config;
                         Field : in Application_Fields;
                         Value : in Util.Beans.Objects.Object);

private

   package Application_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Application_Config,
                                               Element_Type_Access => Application_Config_Access,
                                               Fields              => Application_Fields,
                                               Set_Member          => Set_Member);

end ASF.Applications.Main.Configs;
