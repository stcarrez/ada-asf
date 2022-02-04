-----------------------------------------------------------------------
--  asf-beans-mappers -- Read XML managed bean declarations
--  Copyright (C) 2010, 2011, 2017, 2019 Stephane Carrez
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

   Empty_Params : ASF.Beans.Parameter_Bean_Ref.Ref;

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
                  raise Util.Serialize.Mappers.Field_Error with "Invalid scope: " & Scope;
               end if;
            end;

         when FIELD_PROPERTY_NAME =>
            MBean.Prop_Name := Value;

         when FIELD_PROPERTY_VALUE =>
            MBean.Prop_Value := Value;

         when FIELD_PROPERTY_CLASS =>
            null;

         when FIELD_PROPERTY =>
            --  Create the parameter list only the first time a property is seen.
            if MBean.Params.Is_Null then
               MBean.Params := ASF.Beans.Parameter_Bean_Ref.Create (new ASF.Beans.Parameter_Bean);
            end if;

            --  Add the parameter.  The property value is parsed as an EL expression.
            EL.Beans.Add_Parameter (MBean.Params.Value.Params,
                                    Util.Beans.Objects.To_String (MBean.Prop_Name),
                                    Util.Beans.Objects.To_String (MBean.Prop_Value),
                                    MBean.Context.all);
            MBean.Prop_Name  := Empty;
            MBean.Prop_Value := Empty;

         when FIELD_MANAGED_BEAN =>
            declare
               Name  : constant String := Util.Beans.Objects.To_String (MBean.Name);
               Class : constant String := Util.Beans.Objects.To_String (MBean.Class);
            begin
               Register (Factory => MBean.Factory.all,
                         Name    => Name,
                         Class   => Class,
                         Params  => MBean.Params,
                         Scope   => MBean.Scope);
            end;
            MBean.Name  := Empty;
            MBean.Class := Empty;
            MBean.Scope := REQUEST_SCOPE;
            MBean.Params := Empty_Params;

      end case;
   end Set_Member;

   MBean_Mapping : aliased Config_Mapper.Mapper;

   --  ------------------------------
   --  Setup the XML parser to read the managed bean definitions.
   --  ------------------------------
   package body Reader_Config is
   begin
      Mapper.Add_Mapping ("faces-config", MBean_Mapping'Access);
      Mapper.Add_Mapping ("module", MBean_Mapping'Access);
      Mapper.Add_Mapping ("web-app", MBean_Mapping'Access);
      Config.Factory := Factory;
      Config.Context := Context;
      Config_Mapper.Set_Context (Mapper, Config'Unchecked_Access);
   end Reader_Config;

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
   MBean_Mapping.Add_Mapping ("managed-bean/managed-property", FIELD_PROPERTY);
end ASF.Beans.Mappers;
