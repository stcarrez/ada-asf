-----------------------------------------------------------------------
--  asf-navigations-reader -- Read XML navigation files
--  Copyright (C) 2010, 2011, 2012, 2013, 2017, 2018 Stephane Carrez
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

with ASF.Navigations.Redirect;
with ASF.Navigations.Render;
package body ASF.Navigations.Mappers is

   use Util.Beans.Objects;

   Empty : constant Util.Beans.Objects.Object := To_Object (String '(""));

   --  ------------------------------
   --  Reset the navigation config before parsing a new rule.
   --  ------------------------------
   procedure Reset (N : in out Nav_Config) is
   begin
      N.To_View   := Empty;
      N.Outcome   := Empty;
      N.Action    := Empty;
      N.Condition := Empty;
      N.Status    := 0;
      N.Redirect  := False;
   end Reset;

   --  ------------------------------
   --  Save in the navigation config object the value associated with the given field.
   --  When the <b>NAVIGATION_CASE</b> field is reached, insert the new navigation rule
   --  that was collected in the navigation handler.
   --  ------------------------------
   procedure Set_Member (N : in out Nav_Config;
                         Field : in Navigation_Case_Fields;
                         Value : in Util.Beans.Objects.Object) is
      use ASF.Navigations.Redirect;
      use ASF.Navigations.Render;
   begin
      case Field is
         when OUTCOME =>
            N.Outcome := Value;

         when ACTION =>
            N.Action := Value;

         when TO_VIEW =>
            N.To_View := Value;

         when FROM_VIEW_ID =>
            N.From_View := Value;

         when REDIRECT =>
            N.Redirect := True;

         when CONDITION =>
            N.Condition := Value;
            null;

         when CONTENT =>
            N.Content := Value;

         when CONTENT_TYPE =>
            N.Content_Type := Value;

         when STATUS =>
            N.Status := Util.Beans.Objects.To_Integer (Value);

         when NAVIGATION_CASE =>
            declare
               Navigator : Navigation_Access;
            begin
               if N.Redirect then
                  Navigator := Create_Redirect_Navigator (To_String (N.To_View), N.Context.all);
               else
                  Navigator := Create_Render_Navigator (To_String (N.To_View), N.Status);
               end if;
               N.Handler.Add_Navigation_Case (Navigator => Navigator,
                                              From      => To_String (N.From_View),
                                              Outcome   => To_String (N.Outcome),
                                              Action    => To_String (N.Action),
                                              Condition => To_String (N.Condition),
                                              Context   => N.Context.all);
            end;
            Reset (N);

         when NAVIGATION_RULE =>
            N.From_View := Empty;

      end case;
   end Set_Member;

   Mapping        : aliased Navigation_Mapper.Mapper;

   --  ------------------------------
   --  Setup the XML parser to read the navigation rules.
   --  ------------------------------
   package body Reader_Config is
   begin
      Mapper.Add_Mapping ("faces-config", Mapping'Access);
      Mapper.Add_Mapping ("module", Mapping'Access);
      Mapper.Add_Mapping ("web-app", Mapping'Access);
      Config.Handler := Handler;
      Config.Context := Context;
      Config.From_View := Empty;
      Reset (Config);
      Navigation_Mapper.Set_Context (Mapper, Config'Unchecked_Access);
   end Reader_Config;

begin

   --  <navigation-rule> mapping
   Mapping.Add_Mapping ("navigation-rule/from-view-id", FROM_VIEW_ID);
   Mapping.Add_Mapping ("navigation-rule/navigation-case/from-action", ACTION);
   Mapping.Add_Mapping ("navigation-rule/navigation-case/from-outcome", OUTCOME);
   Mapping.Add_Mapping ("navigation-rule/navigation-case/to-view-id", TO_VIEW);
   Mapping.Add_Mapping ("navigation-rule/navigation-case/if", CONDITION);
--     Mapping.Add_Mapping ("navigation-case/redirect/view-param/name", VIEW_PARAM_NAME);
--     Mapping.Add_Mapping ("navigation-case/redirect/view-param/value", VIEW_PARAM_VALUE);
--     Mapping.Add_Mapping ("navigation-case/redirect/include-view-params", INCLUDE_VIEW_PARAMS);
   Mapping.Add_Mapping ("navigation-rule/navigation-case/redirect", REDIRECT);
   Mapping.Add_Mapping ("navigation-rule/navigation-case/content", CONTENT);
   Mapping.Add_Mapping ("navigation-rule/navigation-case/status", STATUS);
   Mapping.Add_Mapping ("navigation-rule/navigation-case/content/@type", CONTENT_TYPE);
   Mapping.Add_Mapping ("navigation-rule/navigation-case", NAVIGATION_CASE);
   Mapping.Add_Mapping ("navigation-rule", NAVIGATION_RULE);

end ASF.Navigations.Mappers;
