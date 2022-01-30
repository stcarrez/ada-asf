-----------------------------------------------------------------------
--  asf-beans-mappers -- Read XML managed bean declarations
--  Copyright (C) 2010, 2011, 2017 Stephane Carrez
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

package ASF.Beans.Mappers is

   type Managed_Bean_Fields is (FIELD_NAME,
                                FIELD_CLASS,
                                FIELD_SCOPE,
                                FIELD_MANAGED_BEAN,
                                FIELD_PROPERTY,
                                FIELD_PROPERTY_NAME,
                                FIELD_PROPERTY_VALUE,
                                FIELD_PROPERTY_CLASS);

   type Bean_Factory_Access is access all ASF.Beans.Bean_Factory;

   type Managed_Bean is limited record
      Name         : Util.Beans.Objects.Object;
      Class        : Util.Beans.Objects.Object;
      Scope        : Scope_Type := REQUEST_SCOPE;
      Factory      : Bean_Factory_Access  := null;
      Params       : ASF.Beans.Parameter_Bean_Ref.Ref;
      Prop_Name    : Util.Beans.Objects.Object;
      Prop_Value   : Util.Beans.Objects.Object;
      Context      : EL.Contexts.ELContext_Access := null;
   end record;
   type Managed_Bean_Access is access all Managed_Bean;

   --  Set the field identified by <b>Field</b> with the <b>Value</b>.
   procedure Set_Member (MBean : in out Managed_Bean;
                         Field : in Managed_Bean_Fields;
                         Value : in Util.Beans.Objects.Object);

   --  Setup the XML parser to read the managed bean definitions.
   generic
      Mapper  : in out Util.Serialize.Mappers.Processing;
      Factory : in Bean_Factory_Access;
      Context : in EL.Contexts.ELContext_Access;
   package Reader_Config is
      Config : aliased Managed_Bean;
   end Reader_Config;

private

   package Config_Mapper is
     new Util.Serialize.Mappers.Record_Mapper (Element_Type        => Managed_Bean,
                                               Element_Type_Access => Managed_Bean_Access,
                                               Fields              => Managed_Bean_Fields,
                                               Set_Member          => Set_Member);

end ASF.Beans.Mappers;
