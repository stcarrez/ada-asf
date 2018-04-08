-----------------------------------------------------------------------
--  asf-navigations-mappers -- Read XML navigation files
--  Copyright (C) 2010, 2011, 2017, 2018 Stephane Carrez
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

--  The <b>ASF.Navigations.Reader</b> package defines an XML mapper that can be used
--  to read the XML navigation files.
package ASF.Navigations.Mappers is

   type Navigation_Case_Fields is (FROM_VIEW_ID, OUTCOME, ACTION, TO_VIEW, REDIRECT, CONDITION,
                                   CONTENT, CONTENT_TYPE, NAVIGATION_CASE, NAVIGATION_RULE,
                                   STATUS);

   --  ------------------------------
   --  Navigation Config Reader
   --  ------------------------------
   --  When reading and parsing the XML navigation file, the <b>Nav_Config</b> object
   --  is populated by calls through the <b>Set_Member</b> procedure.  The data is
   --  collected and when the end of the navigation case element is reached,
   --  the new navigation case is inserted in the navigation handler.
   type Nav_Config is limited record
      Outcome      : Util.Beans.Objects.Object;
      Action       : Util.Beans.Objects.Object;
      To_View      : Util.Beans.Objects.Object;
      From_View    : Util.Beans.Objects.Object;
      Redirect     : Boolean := False;
      Condition    : Util.Beans.Objects.Object;
      Content      : Util.Beans.Objects.Object;
      Content_Type : Util.Beans.Objects.Object;
      Status       : Natural := 0;
      Context      : EL.Contexts.ELContext_Access;
      Handler      : Navigation_Handler_Access;
   end record;
   type Nav_Config_Access is access all Nav_Config;

   --  Save in the navigation config object the value associated with the given field.
   --  When the <b>NAVIGATION_CASE</b> field is reached, insert the new navigation rule
   --  that was collected in the navigation handler.
   procedure Set_Member (N     : in out Nav_Config;
                         Field : in Navigation_Case_Fields;
                         Value : in Util.Beans.Objects.Object);

   --  Setup the XML parser to read the navigation rules.
   generic
      Mapper  : in out Util.Serialize.Mappers.Processing;
      Handler : in Navigation_Handler_Access;
      Context : in EL.Contexts.ELContext_Access;
   package Reader_Config is
      Config : aliased Nav_Config;
   end Reader_Config;

private

   --  Reset the navigation config before parsing a new rule.
   procedure Reset (N : in out Nav_Config);

   package Navigation_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Nav_Config,
                                               Element_Type_Access => Nav_Config_Access,
                                               Fields              => Navigation_Case_Fields,
                                               Set_Member          => Set_Member);

end ASF.Navigations.Mappers;
