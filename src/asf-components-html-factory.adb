-----------------------------------------------------------------------
--  html-factory -- Factory for HTML UI Components
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

with ASF.Components.Html.Text;
with ASF.Components.Html.Lists;
with ASF.Views.Nodes;
package body ASF.Components.Html.Factory is

   function Create_Output return UIComponent_Access;
   function Create_Label return UIComponent_Access;
   function Create_List return UIComponent_Access;

   --  ------------------------------
   --  Create an UIOutput component
   --  ------------------------------
   function Create_Output return UIComponent_Access is
   begin
      return new ASF.Components.Html.Text.UIOutput;
   end Create_Output;

   --  ------------------------------
   --  Create a label component
   --  ------------------------------
   function Create_Label return UIComponent_Access is
   begin
      return new ASF.Components.Html.Text.UILabel;
   end Create_Label;

   --  ------------------------------
   --  Create an UIList component
   --  ------------------------------
   function Create_List return UIComponent_Access is
   begin
      return new ASF.Components.Html.Lists.UIList;
   end Create_List;

   use ASF.Views.Nodes;

   URI        : aliased constant String := "http://java.sun.com/jsf/html";
   OUTPUT_TAG : aliased constant String := "output";
   LABEL_TAG  : aliased constant String := "label";
   LIST_TAG   : aliased constant String := "list";

   Html_Bindings : aliased constant ASF.Factory.Binding_Array
     := (2 => (Name      => OUTPUT_TAG'Access,
               Component => Create_Output'Access,
               Tag       => Create_Component_Node'Access),
         0 => (Name      => LABEL_TAG'Access,
               Component => Create_Label'Access,
               Tag       => Create_Component_Node'Access),
         1 => (Name      => LIST_TAG'Access,
               Component => Create_List'Access,
               Tag       => Create_Component_Node'Access)
        );

   Html_Factory : aliased constant ASF.Factory.Factory_Bindings
     := (URI => URI'Access, Bindings => Html_Bindings'Access);

   --  ------------------------------
   --  Get the HTML component factory.
   --  ------------------------------
   function Definition return ASF.Factory.Factory_Bindings_Access is
   begin
      return Html_Factory'Access;
   end Definition;

begin
   ASF.Factory.Check (Html_Factory);
end ASF.Components.Html.Factory;
