-----------------------------------------------------------------------
--  components-ajax-factory -- Factory for AJAX Components
--  Copyright (C) 2011 Stephane Carrez
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

   Ajax_Bindings : aliased constant ASF.Factory.Binding_Array
     := (1 => (Name      => INCLUDE_TAG'Access,
               Component => Create_Include'Access,
               Tag       => Create_Component_Node'Access)
        );

   Ajax_Factory : aliased constant ASF.Factory.Factory_Bindings
     := (URI => URI'Access, Bindings => Ajax_Bindings'Access);

   --  ------------------------------
   --  Get the Ajax component factory.
   --  ------------------------------
   function Definition return ASF.Factory.Factory_Bindings_Access is
   begin
      return Ajax_Factory'Access;
   end Definition;

end ASF.Components.Ajax.Factory;
