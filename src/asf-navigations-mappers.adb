-----------------------------------------------------------------------
--  asf-navigations-reader -- Read XML navigation files
--  Copyright (C) 2010, 2011 Stephane Carrez
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

package body ASF.Navigations.Mappers is

   use Util.Beans.Objects;

   Empty : constant Util.Beans.Objects.Object := To_Object (String '(""));

   --  ------------------------------
   --  Save in the navigation config object the value associated with the given field.
   --  When the <b>NAVIGATION_CASE</b> field is reached, insert the new navigation rule
   --  that was collected in the navigation handler.
   --  ------------------------------
   procedure Set_Member (N : in out Nav_Config;
                         Field : in Navigation_Case_Fields;
                         Value : in Util.Beans.Objects.Object) is
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
            N.Redirect := Value;

         when CONDITION =>
            N.Condition := Value;
            null;

         when CONTENT =>
            N.Content := Value;

         when CONTENT_TYPE =>
            N.Content_Type := Value;

         when NAVIGATION_CASE =>
            N.Handler.Add_Navigation_Case (From      => To_String (N.From_View),
                                           To        => To_String (N.To_View),
                                           Outcome   => To_String (N.Outcome),
                                           Action    => To_String (N.Action),
                                           Condition => To_String (N.Condition));
            N.From_View := Empty;
            N.To_View   := Empty;
            N.Outcome   := Empty;
            N.Action    := Empty;
            N.Condition := Empty;

      end case;
   end Set_Member;

   Case_Mapping        : aliased Case_Mapper.Mapper;

   --  ------------------------------
   --  Get the mapper definition for the <b>navigation-rule</b> definition.
   --  ------------------------------
   function Get_Navigation_Mapper return Util.Serialize.Mappers.Mapper_Access is
   begin
      return Case_Mapping'Access;
   end Get_Navigation_Mapper;

begin

   --  <navigation-rule> mapping
   Case_Mapping.Add_Mapping ("navigation-rule/from-view-id", FROM_VIEW_ID);
   Case_Mapping.Add_Mapping ("navigation-rule/navigation-case/from-action", ACTION);
   Case_Mapping.Add_Mapping ("navigation-rule/navigation-case/from-outcome", OUTCOME);
   Case_Mapping.Add_Mapping ("navigation-rule/navigation-case/to-view-id", TO_VIEW);
   Case_Mapping.Add_Mapping ("navigation-rule/navigation-case/if", CONDITION);
--     Case_Mapping.Add_Mapping ("navigation-case/redirect/view-param/name", VIEW_PARAM_NAME);
--     Case_Mapping.Add_Mapping ("navigation-case/redirect/view-param/value", VIEW_PARAM_VALUE);
--     Case_Mapping.Add_Mapping ("navigation-case/redirect/include-view-params", INCLUDE_VIEW_PARAMS);
   Case_Mapping.Add_Mapping ("navigation-rule/navigation-case/content", CONTENT);
   Case_Mapping.Add_Mapping ("navigation-rule/navigation-case/content/@type", CONTENT_TYPE);
   Case_Mapping.Add_Mapping ("navigation-rule/navigation-case", NAVIGATION_CASE);

end ASF.Navigations.Mappers;
