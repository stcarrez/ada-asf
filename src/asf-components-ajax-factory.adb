-----------------------------------------------------------------------
--  components-ajax-factory -- Factory for AJAX Components
--  Copyright (C) 2011, 2018 Stephane Carrez
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
