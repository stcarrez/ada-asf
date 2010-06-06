-----------------------------------------------------------------------
--  nodes-core -- Tag Factory
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

with EL.Functions.Default;
package body ASF.Views.Nodes.Factory is

   Function_Mapper : aliased EL.Functions.Default.Default_Function_Mapper;

   function Functions return EL.Functions.Function_Mapper_Access is
   begin
      return Function_Mapper'Access;
   end Functions;

begin
   Function_Mapper.Set_Function (Name      => "truncate",
                                 Namespace => "http://code.google.com/p/ada-el",
                                 Func      => EL.Functions.Default.Truncate'Access);
end ASF.Views.Nodes.Factory;
