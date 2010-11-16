-----------------------------------------------------------------------
--  core-factory -- Factory for Core UI Components
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

with ASF.Views.Nodes;
with ASF.Views.Nodes.Jsf;
package body ASF.Components.Core.Factory is

   function Create_View return UIComponent_Access;
   function Create_Parameter return UIComponent_Access;

   --  ------------------------------
   --  Create an UIView component
   --  ------------------------------
   function Create_View return UIComponent_Access is
   begin
      return new ASF.Components.Core.UIView;
   end Create_View;

   --  ------------------------------
   --  Create an UIParameter component
   --  ------------------------------
   function Create_Parameter return UIComponent_Access is
   begin
      return new ASF.Components.Core.UIParameter;
   end Create_Parameter;

   use ASF.Views.Nodes;

   URI           : aliased constant String := "http://java.sun.com/jsf/core";
   ATTRIBUTE_TAG : aliased constant String := "attribute";
   CONVERTER_TAG : aliased constant String := "converter";
   PARAM_TAG     : aliased constant String := "param";
   VIEW_TAG      : aliased constant String := "view";

   Core_Bindings : aliased constant ASF.Factory.Binding_Array
     := (1 => (Name      => ATTRIBUTE_TAG'Access,
               Component => null,
               Tag       => Views.Nodes.Jsf.Create_Attribute_Tag_Node'Access),

         2 => (Name      => CONVERTER_TAG'Access,
               Component => null,
               Tag       => Views.Nodes.Jsf.Create_Converter_Tag_Node'Access),

         3 => (Name      => PARAM_TAG'Access,
               Component => Create_Parameter'Access,
               Tag       => Create_Component_Node'Access),

         4 => (Name      => VIEW_TAG'Access,
               Component => Create_View'Access,
               Tag       => Create_Component_Node'Access)
        );

   Core_Factory : aliased constant ASF.Factory.Factory_Bindings
     := (URI => URI'Access, Bindings => Core_Bindings'Access);

   --  ------------------------------
   --  Get the HTML component factory.
   --  ------------------------------
   function Definition return ASF.Factory.Factory_Bindings_Access is
   begin
      return Core_Factory'Access;
   end Definition;

end ASF.Components.Core.Factory;
