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

with Util.Beans.Objects;
with Util.Serialize.Mappers.Record_Mapper;

package ASF.Beans.Mappers is

   type Managed_Bean_Fields is (FIELD_NAME,
                                FIELD_CLASS,
                                FIELD_SCOPE,
                                FIELD_MANAGED_BEAN,
                                FIELD_PROPERTY_NAME,
                                FIELD_PROPERTY_VALUE,
                                FIELD_PROPERTY_CLASS);

   type Managed_Bean is record
      Name         : Util.Beans.Objects.Object;
      Class        : Util.Beans.Objects.Object;
      Scope        : Scope_Type := REQUEST_SCOPE;
      Factory      : access ASF.Beans.Bean_Factory;
   end record;
   type Managed_Bean_Access is access all Managed_Bean;

   --  Set the field identified by <b>Field</b> with the <b>Value</b>.
   procedure Set_Member (MBean : in out Managed_Bean;
                         Field : in Managed_Bean_Fields;
                         Value : in Util.Beans.Objects.Object);

   package Config_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Managed_Bean,
                                               Element_Type_Access => Managed_Bean_Access,
                                               Fields              => Managed_Bean_Fields,
                                               Set_Member          => Set_Member);

   --  Get the mapper definition for the <b>managed-bean</b> definition.
   function Get_Managed_Bean_Mapper return Util.Serialize.Mappers.Mapper_Access;

end ASF.Beans.Mappers;
