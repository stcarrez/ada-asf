-----------------------------------------------------------------------
--  asf-servlets-faces-mappers -- Read faces specific configuration files
--  Copyright (C) 2015, 2017 Stephane Carrez
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

with EL.Contexts;

--  == Faces Pretty URLs ==
--  The faces servlet supports pretty URLs with a custom XML configuration.
--  The `url-mapping` XML description allows to define a URL pattern that
--  contains parameters that will be injected in some Ada bean and will be
--  mapped to a specific faces XHTML view file.
--
--    <url-mapping>
--      <pattern>/wikis/#{wiki.wiki_space_id}/admin/#{wiki.page_id}/view.html</pattern>
--      <view-id>/wikis/admin/view.html</view-id>
--    </url-mapping>
--
--
package ASF.Servlets.Faces.Mappers is

   type Servlet_Fields is (URL_MAPPING, URL_PATTERN, VIEW_ID);

   --  ------------------------------
   --  Servlet Config Reader
   --  ------------------------------
   --  When reading and parsing the servlet configuration file, the <b>Servlet_Config</b> object
   --  is populated by calls through the <b>Set_Member</b> procedure.  The data is
   --  collected and when the end of an element (URL_MAPPING, URL_PATTERN, VIEW_ID)
   --  is reached, the definition is updated in the servlet registry.
   type Servlet_Config is limited record
      URL_Pattern      : Util.Beans.Objects.Object;
      View_Id          : Util.Beans.Objects.Object;
      Handler          : Servlet_Registry_Access;
      Context          : EL.Contexts.ELContext_Access;
   end record;
   type Servlet_Config_Access is access all Servlet_Config;

   --  Save in the servlet config object the value associated with the given field.
   --  When the URL_MAPPING, URL_PATTERN, VIEW_ID field
   --  is reached, insert the new configuration rule in the servlet registry.
   procedure Set_Member (N     : in out Servlet_Config;
                         Field : in Servlet_Fields;
                         Value : in Util.Beans.Objects.Object);

   --  Setup the XML parser to read the servlet and mapping rules <b>url-mapping</b>.
   generic
      Mapper  : in out Util.Serialize.Mappers.Processing;
      Handler : in Servlet_Registry_Access;
      Context : in EL.Contexts.ELContext_Access;
   package Reader_Config is
      Config : aliased Servlet_Config;
   end Reader_Config;

private

   package Servlet_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Servlet_Config,
                                               Element_Type_Access => Servlet_Config_Access,
                                               Fields              => Servlet_Fields,
                                               Set_Member          => Set_Member);

end ASF.Servlets.Faces.Mappers;
