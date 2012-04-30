-----------------------------------------------------------------------
--  asf-beans-globals -- Bean giving access to the global init parameters
--  Copyright (C) 2012 Stephane Carrez
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

with Util.Beans.Basic;
with Util.Beans.Objects;

package ASF.Beans.Globals is

   --  Context variable giving access to the global init parameters.
   INIT_PARAM_ATTRIBUTE_NAME    : constant String := "initParam";

   --  ------------------------------
   --  Init Parameter Bean
   --  ------------------------------
   --  The <b>Global_Bean</b> gives access to the global init parameters.
   --  The bean instance is global to the application.
   type Global_Bean is new Util.Beans.Basic.Readonly_Bean with private;

   --  Get the init parameter identified by the given name.
   --  Returns Null_Object if the application does not define such parameter.
   overriding
   function Get_Value (Bean : in Global_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Return the Param_Bean instance.
   function Instance return Util.Beans.Objects.Object;

private

   type Global_Bean is new Util.Beans.Basic.Readonly_Bean with null record;

end ASF.Beans.Globals;
