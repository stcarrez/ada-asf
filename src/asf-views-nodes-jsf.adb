-----------------------------------------------------------------------
--  views.nodes.jsf -- JSF Core Tag Library
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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

with ASF.Converters;
with ASF.Validators.Texts;
with ASF.Validators.Numbers;
with ASF.Components.Holders;
with ASF.Components.Core.Views;
package body ASF.Views.Nodes.Jsf is

   use ASF;
   use EL.Objects;

   --  ------------------------------
   --  Converter Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Converter Tag
   --  ------------------------------
   function Create_Converter_Tag_Node (Name       : Unbounded_String;
                                       Line       : Views.Line_Info;
                                       Parent     : Views.Nodes.Tag_Node_Access;
                                       Attributes : Views.Nodes.Tag_Attribute_Array_Access)
                                       return Views.Nodes.Tag_Node_Access is

      use ASF.Views.Nodes;

      Node : constant Converter_Tag_Node_Access := new Converter_Tag_Node;
      Conv : constant Tag_Attribute_Access := Find_Attribute (Attributes,
                                                              "converterId");
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
      if Conv = null then
         Node.Error ("Missing 'converterId' attribute");
      else
         Node.Converter := EL.Objects.To_Object (Conv.Value);
      end if;
      return Node.all'Access;
   end Create_Converter_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.  Get the specified converter and
   --  add it to the parent component.  This operation does not create any
   --  new UIComponent.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Converter_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class) is
      use ASF.Components.Holders;
      use type ASF.Converters.Converter_Access;

      Cvt : constant Converters.Converter_Access := Context.Get_Converter (Node.Converter);
   begin
      if not (Parent.all in Value_Holder'Class) then
         Node.Error ("Parent component is not an instance of Value_Holder");
         return;
      end if;

      if Cvt = null then
         Node.Error ("Converter was not found");
         return;
      end if;

      declare
         VH : constant access Value_Holder'Class := Value_Holder'Class (Parent.all)'Access;
      begin
         VH.Set_Converter (Converter => Cvt);
      end;
   end Build_Components;

   --  ------------------------------
   --  Validator Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Validator Tag
   --  ------------------------------
   function Create_Validator_Tag_Node (Name       : Unbounded_String;
                                       Line       : Views.Line_Info;
                                       Parent     : Views.Nodes.Tag_Node_Access;
                                       Attributes : Views.Nodes.Tag_Attribute_Array_Access)
                                       return Views.Nodes.Tag_Node_Access is

      use ASF.Views.Nodes;

      Node : constant Validator_Tag_Node_Access := new Validator_Tag_Node;
      Vid  : constant Tag_Attribute_Access := Find_Attribute (Attributes,
                                                              "validatorId");
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
      if Vid = null then
         Node.Error ("Missing 'validatorId' attribute");
      else
         Node.Validator := EL.Objects.To_Object (Vid.Value);
      end if;
      return Node.all'Access;
   end Create_Validator_Tag_Node;

   --  ------------------------------
   --  Get the specified validator and add it to the parent component.
   --  This operation does not create any new UIComponent.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Validator_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class) is
      use ASF.Components.Holders;
      use type ASF.Validators.Validator_Access;

      V      : Validators.Validator_Access;
      Shared : Boolean;
   begin
      if not (Parent.all in Editable_Value_Holder'Class) then
         Node.Error ("Parent component is not an instance of Editable_Value_Holder");
         return;
      end if;

      Validator_Tag_Node'Class (Node.all).Get_Validator (Context, V, Shared);
      if V = null then
         Node.Error ("Validator was not found");
         return;
      end if;

      declare
         VH : constant access Editable_Value_Holder'Class
           := Editable_Value_Holder'Class (Parent.all)'Access;
      begin
         VH.Add_Validator (Validator => V, Shared => Shared);
      end;
   end Build_Components;

   --  ------------------------------
   --  Get the validator instance that corresponds to the validator tag.
   --  Returns in <b>Validator</b> the instance if it exists and indicate
   --  in <b>Shared</b> whether it must be freed or not when the component is deleted.
   --  ------------------------------
   procedure Get_Validator (Node      : in Validator_Tag_Node;
                            Context   : in out Contexts.Facelets.Facelet_Context'Class;
                            Validator : out Validators.Validator_Access;
                            Shared    : out Boolean) is
   begin
      Validator := Context.Get_Validator (Node.Validator);
      Shared    := True;
   end Get_Validator;

   --  ------------------------------
   --  Range Validator Tag
   --  ------------------------------

   --  Create the Range_Validator Tag
   function Create_Range_Validator_Tag_Node (Name       : Unbounded_String;
                                             Line       : Views.Line_Info;
                                             Parent     : Views.Nodes.Tag_Node_Access;
                                             Attributes : Views.Nodes.Tag_Attribute_Array_Access)
                                             return Views.Nodes.Tag_Node_Access is

      use ASF.Views.Nodes;

      Node : constant Range_Validator_Tag_Node_Access := new Range_Validator_Tag_Node;
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
      Node.Minimum := Find_Attribute (Attributes, "minimum");
      Node.Maximum := Find_Attribute (Attributes, "maximum");
      if Node.Minimum = null and Node.Maximum = null then
         Node.Error ("Missing 'minimum' or 'maximum' attribute");
      end if;
      return Node.all'Access;
   end Create_Range_Validator_Tag_Node;

   --  Get the validator instance that corresponds to the range validator.
   --  Returns in <b>Validator</b> the validator instance if it exists and indicate
   --  in <b>Shared</b> whether it must be freed or not when the component is deleted.
   overriding
   procedure Get_Validator (Node      : in Range_Validator_Tag_Node;
                            Context   : in out Contexts.Facelets.Facelet_Context'Class;
                            Validator : out Validators.Validator_Access;
                            Shared    : out Boolean) is
      use type ASF.Validators.Validator_Access;

      Min : Long_Long_Integer := Long_Long_Integer'First;
      Max : Long_Long_Integer := Long_Long_Integer'Last;
   begin
      --  Get the minimum and maximum attributes.
      begin
         if Node.Minimum /= null then
            Min := EL.Objects.To_Long_Long_Integer (Get_Value (Node.Minimum.all, Context));
         end if;
         if Node.Maximum /= null then
            Max := EL.Objects.To_Long_Long_Integer (Get_Value (Node.Maximum.all, Context));
         end if;

      exception
         when Constraint_Error =>
            Node.Error ("Invalid minimum or maximum value");
      end;
      Shared := False;
      if Max < Min then
         Node.Error ("Minimum ({0}) should be less than maximum ({1})",
                     Long_Long_Integer'Image (Min), Long_Long_Integer'Image (Max));
         return;
      end if;

      Validator := Validators.Numbers.Create_Range_Validator (Minimum => Min,
                                                              Maximum => Max);
   end Get_Validator;

   --  ------------------------------
   --  Length Validator Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Length_Validator Tag.  Verifies that the XML node defines
   --  the <b>minimum</b> or the <b>maximum</b> or both attributes.
   --  ------------------------------
   function Create_Length_Validator_Tag_Node (Name       : Unbounded_String;
                                              Line       : Views.Line_Info;
                                              Parent     : Views.Nodes.Tag_Node_Access;
                                              Attributes : Views.Nodes.Tag_Attribute_Array_Access)
                                              return Views.Nodes.Tag_Node_Access is

      use ASF.Views.Nodes;

      Node : constant Length_Validator_Tag_Node_Access := new Length_Validator_Tag_Node;
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
      Node.Minimum := Find_Attribute (Attributes, "minimum");
      Node.Maximum := Find_Attribute (Attributes, "maximum");
      if Node.Minimum = null and Node.Maximum = null then
         Node.Error ("Missing 'minimum' or 'maximum' attribute");
      end if;
      return Node.all'Access;
   end Create_Length_Validator_Tag_Node;

   --  ------------------------------
   --  Get the validator instance that corresponds to the validator tag.
   --  Returns in <b>Validator</b> the instance if it exists and indicate
   --  in <b>Shared</b> whether it must be freed or not when the component is deleted.
   --  ------------------------------
   overriding
   procedure Get_Validator (Node      : in Length_Validator_Tag_Node;
                            Context   : in out Contexts.Facelets.Facelet_Context'Class;
                            Validator : out Validators.Validator_Access;
                            Shared    : out Boolean) is
      use type ASF.Validators.Validator_Access;

      Min : Natural := 0;
      Max : Natural := Natural'Last;
   begin
      --  Get the minimum and maximum attributes.
      begin
         if Node.Minimum /= null then
            Min := Natural (EL.Objects.To_Integer (Get_Value (Node.Minimum.all, Context)));
         end if;
         if Node.Maximum /= null then
            Max := Natural (EL.Objects.To_Integer (Get_Value (Node.Maximum.all, Context)));
         end if;

      exception
         when Constraint_Error =>
            Node.Error ("Invalid minimum or maximum value");
      end;
      Shared := False;
      if Max < Min then
         Node.Error ("Minimum ({0}) should be less than maximum ({1})",
                     Natural'Image (Min), Natural'Image (Max));
         return;
      end if;

      Validator := Validators.Texts.Create_Length_Validator (Minimum => Min,
                                                             Maximum => Max);
   end Get_Validator;

   --  ------------------------------
   --  Attribute Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Attribute Tag
   --  ------------------------------
   function Create_Attribute_Tag_Node (Name       : Unbounded_String;
                                       Line       : Views.Line_Info;
                                       Parent     : Views.Nodes.Tag_Node_Access;
                                       Attributes : Views.Nodes.Tag_Attribute_Array_Access)
                                       return Views.Nodes.Tag_Node_Access is

      use ASF.Views.Nodes;

      Node : constant Attribute_Tag_Node_Access := new Attribute_Tag_Node;
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
      Node.Attr_Name := Find_Attribute (Attributes, "name");
      Node.Value     := Find_Attribute (Attributes, "value");
      if Node.Attr_Name = null then
         Node.Error ("Missing 'name' attribute");
      end if;
      if Node.Value = null then
         Node.Error ("Missing 'value' attribute");
      end if;
      return Node.all'Access;
   end Create_Attribute_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.
   --  Adds the attribute to the component node.
   --  This operation does not create any new UIComponent.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Attribute_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class) is
      use EL.Expressions;
   begin
      if Node.Attr_Name /= null and Node.Value /= null then
         declare
            Name  : constant EL.Objects.Object := Get_Value (Node.Attr_Name.all, Context);
         begin
            Node.Attr.Name := EL.Objects.To_Unbounded_String (Name);
            if Node.Value.Binding /= null then
               declare
                  Expr : constant EL.Expressions.Expression
                    := ASF.Views.Nodes.Reduce_Expression (Node.Value.all, Context);
               begin
                  Parent.Set_Attribute (Def => Node.Attr'Access, Value => Expr);
               end;
            else
               Parent.Set_Attribute (Def   => Node.Attr'Access,
                                     Value => Get_Value (Node.Value.all, Context));
            end if;
         end;
      end if;
   end Build_Components;

   --  ------------------------------
   --  Facet Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Facet Tag
   --  ------------------------------
   function Create_Facet_Tag_Node (Name       : Unbounded_String;
                                   Line       : Views.Line_Info;
                                   Parent     : Views.Nodes.Tag_Node_Access;
                                   Attributes : Views.Nodes.Tag_Attribute_Array_Access)
                                   return Views.Nodes.Tag_Node_Access is
      use ASF.Views.Nodes;

      Node : constant Facet_Tag_Node_Access := new Facet_Tag_Node;
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
      Node.Facet_Name := Find_Attribute (Attributes, "name");
      if Node.Facet_Name = null then
         Node.Error ("Missing 'name' attribute");
      end if;
      return Node.all'Access;
   end Create_Facet_Tag_Node;


   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the facet component of the given parent.  Calls recursively the
   --  method to create children.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Facet_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class) is
   begin
      null;
   end Build_Components;

   --  ------------------------------
   --  Metadata Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Metadata Tag
   --  ------------------------------
   function Create_Metadata_Tag_Node (Name       : Unbounded_String;
                                      Line       : Views.Line_Info;
                                      Parent     : Views.Nodes.Tag_Node_Access;
                                      Attributes : Views.Nodes.Tag_Attribute_Array_Access)
                                      return Views.Nodes.Tag_Node_Access is
      use ASF.Views.Nodes;

      Node : constant Metadata_Tag_Node_Access := new Metadata_Tag_Node;
   begin
      Initialize (Node.all'Access, Name, Line, Parent, Attributes);
      return Node.all'Access;
   end Create_Metadata_Tag_Node;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as a metadata information
   --  facet for the UIView parent component.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Metadata_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class) is
      use ASF.Components.Core.Views;
   begin
      if not (Parent.all in UIView'Class) then
         Node.Error ("Parent component of <f:metadata> must be a <f:view>");
         return;
      end if;
      declare
         UI : constant UIViewMetaData_Access := new UIViewMetaData;
      begin
         UIView'Class (Parent.all).Set_Metadata (UI, Node);
         Build_Attributes (UI.all, Node.all, Context);
         UI.Initialize (UI.Get_Context.all);
         Node.Build_Children (UI.all'Access, Context);
      end;
   end Build_Components;

end ASF.Views.Nodes.Jsf;
