-----------------------------------------------------------------------
--  applications -- Ada Web Application
--  Copyright (C) 2009, 2010, 2013, 2015, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
                 Param : Config_Param) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      if Self.Exists (Param.Name.all) then
         return Self.Get (Param.Name.all);
      else
         return Ada.Strings.Unbounded.To_Unbounded_String (Param.Default.all);
      end if;
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

   function Get (Self  : Config;
                 Param : Config_Param) return Integer is
      use Util.Properties.Basic.Integer_Property;
   begin
      return Get (Self, Param.Name.all, Integer'Value (Param.Default.all));
   end Get;

   --  ------------------------------
   --  Create the configuration parameter definition instance.
   --  ------------------------------
   package body Parameter is
      Param       : constant Config_Param := ASF.Applications.P '(Name => PARAM_NAME'Access,
                                                                  Default => PARAM_VALUE'Access);

      --  ------------------------------
      --  Returns the configuration parameter.
      --  ------------------------------
      function P return Config_Param is
      begin
         return Param;
      end P;

   end Parameter;

end ASF.Applications;
