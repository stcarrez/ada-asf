-----------------------------------------------------------------------
--  applications -- Ada Web Application
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with Util.Properties.Basic;
package body ASF.Applications is

   --  ------------------------------
   --  Get the configuration parameter;
   --  ------------------------------
   function Get (Self  : Config;
                 Param : Config_Param) return String is
   begin
      return Self.Get (Param.Name.all, Param.Default.all);
   end Get;

   --  ------------------------------
   --  Get the configuration parameter;
   --  ------------------------------
   function Get (Self  : Config;
                 Param : Config_Param) return Boolean is
      use Util.Properties.Basic.Boolean_Property;
   begin
      return Get (Self, Param.Name.all, Boolean'Value (Param.Default.all));
   end Get;

end ASF.Applications;
