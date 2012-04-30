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

with ASF.Contexts.Faces;
with ASF.Applications.Main;

package body ASF.Beans.Globals is

   Bean : aliased Global_Bean;

   --  ------------------------------
   --  Get the init parameter identified by the given name.
   --  Returns Null_Object if the application does not define such parameter.
   --  ------------------------------
   overriding
   function Get_Value (Bean : in Global_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      pragma Unreferenced (Bean);
      use type ASF.Contexts.Faces.Faces_Context_Access;

      Ctx : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
   begin
      if Ctx = null then
         return Util.Beans.Objects.Null_Object;
      end if;
      declare
         App   : constant ASF.Contexts.Faces.Application_Access := Ctx.Get_Application;
         Param : constant String := App.Get_Init_Parameter (Name, "");
      begin
         if Param = "" then
            return Util.Beans.Objects.Null_Object;
         else
            return Util.Beans.Objects.To_Object (Param);
         end if;
      end;
   end Get_Value;

   --  ------------------------------
   --  Return the Param_Bean instance.
   --  ------------------------------
   function Instance return Util.Beans.Objects.Object is
   begin
      return Util.Beans.Objects.To_Object (Bean'Access, Util.Beans.Objects.STATIC);
   end Instance;

end ASF.Beans.Globals;
