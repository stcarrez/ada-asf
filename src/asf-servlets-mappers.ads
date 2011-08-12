-----------------------------------------------------------------------
--  asf-servlets-mappers -- Read servlet configuration files
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

with Util.Beans.Objects;
with Util.Serialize.Mappers.Record_Mapper;
with Util.Serialize.IO.XML;

--  The <b>ASF.Servlets.Mappers</b> package defines an XML mapper that can be used
--  to read the servlet configuration files.
--
--  The servlet configuration used by ASF is a subset of the servlet deployment descriptor
--  defined in JSR 315 - Java Servlet Specification Version 3.0.  It includes:
--
--  <ul>
--    <li>Definition of filter mapping (<b>filter-mapping</b> tag)</li>
--    <li>Definition of servlet mapping (<b>servlet-mapping</b> tag)</li>
--    <li>Definition of context parameters (<b>context-param</b> tag)</li>
--    <li>Definition of mime types (<b>mime-mapping</b> tag)</li>
--    <li>Definition of error pages (<b>error-page</b> tag)</li>
--  </ul>
--
--  Other configurations are ignored by the mapper.
--
--  Note: several JSR 315 configuration parameters do not makes sense in the ASF world
--  because we cannot create a servlet or a filter through the configuration file.
package ASF.Servlets.Mappers is

   type Servlet_Fields is (FILTER_MAPPING, FILTER_NAME, SERVLET_NAME,
                           URL_PATTERN, SERVLET_MAPPING,
                           CONTEXT_PARAM, PARAM_NAME, PARAM_VALUE,
                           MIME_MAPPING, MIME_TYPE, EXTENSION,
                           ERROR_PAGE, ERROR_CODE, LOCATION);

   --  ------------------------------
   --  Servlet Config Reader
   --  ------------------------------
   --  When reading and parsing the servlet configuration file, the <b>Servlet_Config</b> object
   --  is populated by calls through the <b>Set_Member</b> procedure.  The data is
   --  collected and when the end of an element (FILTER_MAPPING, SERVLET_MAPPING, CONTEXT_PARAM)
   --  is reached, the definition is updated in the servlet registry.
   type Servlet_Config is record
      Filter_Name      : Util.Beans.Objects.Object;
      Servlet_Name     : Util.Beans.Objects.Object;
      URL_Pattern      : Util.Beans.Objects.Object;
      Param_Name       : Util.Beans.Objects.Object;
      Param_Value      : Util.Beans.Objects.Object;
      Mime_Type        : Util.Beans.Objects.Object;
      Extension        : Util.Beans.Objects.Object;
      Error_Code       : Util.Beans.Objects.Object;
      Location         : Util.Beans.Objects.Object;
      Handler          : Servlet_Registry_Access;
   end record;
   type Servlet_Config_Access is access all Servlet_Config;

   --  Save in the servlet config object the value associated with the given field.
   --  When the <b>FILTER_MAPPING</b>, <b>SERVLET_MAPPING</b> or <b>CONTEXT_PARAM</b> field
   --  is reached, insert the new configuration rule in the servlet registry.
   procedure Set_Member (N     : in out Servlet_Config;
                         Field : in Servlet_Fields;
                         Value : in Util.Beans.Objects.Object);

   --  Setup the XML parser to read the servlet and mapping rules <b>context-param</b>,
   --  <b>filter-mapping</b> and <b>servlet-mapping</b>.
   generic
      Reader  : in out Util.Serialize.IO.XML.Parser;
      Handler : in Servlet_Registry_Access;
   package Reader_Config is
      Config : aliased Servlet_Config;
   end Reader_Config;

private

   package Servlet_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Servlet_Config,
                                               Element_Type_Access => Servlet_Config_Access,
                                               Fields              => Servlet_Fields,
                                               Set_Member          => Set_Member);

end ASF.Servlets.Mappers;
