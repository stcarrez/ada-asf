-----------------------------------------------------------------------
--  asf-beans-mappers -- Read XML managed bean declaratiosn
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

package body ASF.Beans.Mappers is

   Empty : constant Util.Beans.Objects.Object := Util.Beans.Objects.To_Object (String '(""));

   --  ------------------------------
   --  Set the field identified by <b>Field</b> with the <b>Value</b>.
   --  ------------------------------
   procedure Set_Member (MBean : in out Managed_Bean;
                         Field : in Managed_Bean_Fields;
                         Value : in Util.Beans.Objects.Object) is
   begin
      case Field is
         when FIELD_NAME =>
            MBean.Name := Value;

         when FIELD_CLASS =>
            MBean.Class := Value;

         when FIELD_SCOPE =>
            declare
               Scope : constant String := Util.Beans.Objects.To_String (Value);
            begin
               if Scope = "request" then
                  MBean.Scope := REQUEST_SCOPE;
               elsif Scope = "session" then
                  MBean.Scope := SESSION_SCOPE;
               elsif Scope = "application" then
                  MBean.Scope := APPLICATION_SCOPE;
               else
                  MBean.Scope := REQUEST_SCOPE;
               end if;
            end;

         when FIELD_PROPERTY_NAME =>
            null;

         when FIELD_PROPERTY_VALUE =>
            null;

         when FIELD_PROPERTY_CLASS =>
            null;

         when FIELD_MANAGED_BEAN =>
            declare
               Name  : constant String := Util.Beans.Objects.To_String (MBean.Name);
               Class : constant String := Util.Beans.Objects.To_String (MBean.Class);
            begin
               Register (Factory => MBean.Factory.all,
                         Name    => Name,
                         Class   => Class,
                         Scope   => MBean.Scope);
            end;
            MBean.Name  := Empty;
            MBean.Class := Empty;
            MBean.Scope := REQUEST_SCOPE;

      end case;
   end Set_Member;

   MBean_Mapping : aliased Config_Mapper.Mapper;

   --  ------------------------------
   --  Get the mapper definition for the <b>managed-bean</b> definition.
   --  ------------------------------
   function Get_Managed_Bean_Mapper return Util.Serialize.Mappers.Mapper_Access is
   begin
      return MBean_Mapping'Access;
   end Get_Managed_Bean_Mapper;

begin
   --  <managed-bean> mapping
   MBean_Mapping.Add_Mapping ("managed-bean", FIELD_MANAGED_BEAN);
   MBean_Mapping.Add_Mapping ("managed-bean/managed-bean-name", FIELD_NAME);
   MBean_Mapping.Add_Mapping ("managed-bean/managed-bean-class", FIELD_CLASS);
   MBean_Mapping.Add_Mapping ("managed-bean/managed-bean-scope", FIELD_SCOPE);
   MBean_Mapping.Add_Mapping ("managed-bean/managed-property/property-name", FIELD_PROPERTY_NAME);
   MBean_Mapping.Add_Mapping ("managed-bean/managed-property/value", FIELD_PROPERTY_VALUE);
   MBean_Mapping.Add_Mapping ("managed-bean/managed-property/property-class",
                              FIELD_PROPERTY_CLASS);
end ASF.Beans.Mappers;
