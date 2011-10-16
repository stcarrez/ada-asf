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

with EL.Contexts;

with ASF.Contexts.Faces;

with Util.Serialize.IO.XML;
with ASF.Applications.Main;
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
   generic
      Reader  : in out Util.Serialize.IO.XML.Parser;
      App     : in ASF.Contexts.Faces.Application_Access;
      Context : in EL.Contexts.ELContext_Access;
   package Reader_Config is
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

end ASF.Applications.Main.Configs;
