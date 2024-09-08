-----------------------------------------------------------------------
--  components-ajax-factory -- Factory for AJAX Components
--  Copyright (C) 2011, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Components.Base;
with ASF.Views.Nodes;
with ASF.Components.Ajax.Includes;
package body ASF.Components.Ajax.Factory is

   function Create_Include return Base.UIComponent_Access;

   --  ------------------------------
   --  Create an UIInclude component
   --  ------------------------------
   function Create_Include return Base.UIComponent_Access is
   begin
      return new ASF.Components.Ajax.Includes.UIInclude;
   end Create_Include;

   use ASF.Views.Nodes;

   URI                     : aliased constant String := "http://code.google.com/p/ada-asf/ajax";
   INCLUDE_TAG             : aliased constant String := "include";

   --  ------------------------------
   --  Register the Ajax component factory.
   --  ------------------------------
   procedure Register (Factory : in out ASF.Factory.Component_Factory) is
   begin
      null;
      ASF.Factory.Register (Factory,
                            URI    => URI'Access,
                            Name   => INCLUDE_TAG'Access,
                            Tag    => Create_Component_Node'Access,
                            Create => Create_Include'Access);
   end Register;

end ASF.Components.Ajax.Factory;
