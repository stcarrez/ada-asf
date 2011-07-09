-----------------------------------------------------------------------
--  asf-modules-reader -- Read module configuration files
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

with Util.Serialize.IO.XML;

with ASF.Applications.Main;
with ASF.Navigations.Mappers;
with ASF.Beans.Mappers;

--  The <b>ASF.Modules.Reader</b> package reads the module configuration files
--  and initializes the module.
package body ASF.Modules.Reader is

   --  Read the module configuration file and configure the components
   procedure Read_Configuration (Plugin : in out Module'Class;
                                 File   : in String) is

      Reader     : Util.Serialize.IO.XML.Parser;
      MBean      : aliased ASF.Beans.Mappers.Managed_Bean;
      Navigation : aliased ASF.Navigations.Mappers.Nav_Config;
   begin
      Log.Info ("Reading module configuration file {0}", File);

      Reader.Add_Mapping ("faces-config", ASF.Beans.Mappers.Get_Managed_Bean_Mapper);
      Reader.Add_Mapping ("faces-config", ASF.Navigations.Mappers.Get_Navigation_Mapper);

      --  Setup the managed bean context to read the <managed-bean> elements.
      MBean.Factory := Plugin.Factory'Unchecked_Access;
      ASF.Beans.Mappers.Config_Mapper.Set_Context (Ctx     => Reader,
                                                   Element => MBean'Unchecked_Access);

      --  Setup the navigation context to read the <navigation-rule> elements.
      Navigation.Handler := Plugin.App.Get_Navigation_Handler;
      ASF.Navigations.Mappers.Case_Mapper.Set_Context (Ctx     => Reader,
                                                       Element => Navigation'Unchecked_Access);

      Util.Serialize.IO.Dump (Reader, ASF.Modules.Log);

      --  Read the configuration file and record managed beans, navigation rules.
      Reader.Parse (File);
   end Read_Configuration;

end ASF.Modules.Reader;
