-----------------------------------------------------------------------
--  nodes-core -- Tag Factory
--  Copyright (C) 2009, 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
