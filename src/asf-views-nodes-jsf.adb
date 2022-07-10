-----------------------------------------------------------------------
--  asf-views-nodes-jsf -- JSF Core Tag Library
--  Copyright (C) 2010 - 2020, 2022 Stephane Carrez
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
with GNAT.Regpat;
with Ada.Exceptions;
with Util.Beans.Objects;
with ASF.Converters;
with ASF.Converters.Dates;
with ASF.Validators.Texts;
with ASF.Validators.Numbers;
with ASF.Components.Holders;
with ASF.Components.Core.Views;
package body ASF.Views.Nodes.Jsf is

   --  Get the date conversion global format.
   function Get_Format (Node    : in Convert_Date_Time_Tag_Node;
                        Context : in Contexts.Facelets.Facelet_Context'Class)
                        return ASF.Converters.Dates.Format_Type;

   --  Get a dateStyle or a timeStyle attribute value.
   function Get_Date_Style (Node    : in Convert_Date_Time_Tag_Node;
                            Name    : in String;
                            Attr    : in Tag_Attribute_Access;
                            Context : in Contexts.Facelets.Facelet_Context'Class)
                            return ASF.Converters.Dates.Style_Type;

   --  ------------------------------
   --  Converter Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Converter Tag
   --  ------------------------------
   function Create_Converter_Tag_Node (Binding    : in Binding_Type;
                                       Line       : in Views.Line_Info;
                                       Parent     : in Views.Nodes.Tag_Node_Access;
                                       Attributes : in Views.Nodes.Tag_Attribute_Array_Access)
                                       return Views.Nodes.Tag_Node_Access is
      Node : constant Converter_Tag_Node_Access := new Converter_Tag_Node;
      Conv : constant Tag_Attribute_Access := Find_Attribute (Attributes,
                                                              "converterId");
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
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
         Node.Error ("Converter '{0}' was not found",
                     Util.Beans.Objects.To_String (Node.Converter));
         return;
      end if;

      declare
         VH : constant access Value_Holder'Class := Value_Holder'Class (Parent.all)'Access;
      begin
         VH.Set_Converter (Converter => Cvt);
      end;
   end Build_Components;

   --  ------------------------------
   --  Convert Date Time Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Converter Tag
   --  ------------------------------
   function Create_Convert_Date_Time_Tag_Node (Binding    : in Binding_Type;
                                               Line       : in Views.Line_Info;
                                               Parent     : in Views.Nodes.Tag_Node_Access;
                                               Attributes : in Nodes.Tag_Attribute_Array_Access)
                                               return Views.Nodes.Tag_Node_Access is
      Node : constant Convert_Date_Time_Tag_Node_Access := new Convert_Date_Time_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Date_Style := Find_Attribute (Attributes, "dateStyle");
      Node.Time_Style := Find_Attribute (Attributes, "timeStyle");
      Node.Locale     := Find_Attribute (Attributes, "locale");
      Node.Pattern    := Find_Attribute (Attributes, "pattern");
      Node.Format     := Find_Attribute (Attributes, "type");
      return Node.all'Access;
   end Create_Convert_Date_Time_Tag_Node;

   --  ------------------------------
   --  Get a dateStyle or a timeStyle attribute value.
   --  ------------------------------
   function Get_Date_Style (Node    : in Convert_Date_Time_Tag_Node;
                            Name    : in String;
                            Attr    : in Tag_Attribute_Access;
                            Context : in Contexts.Facelets.Facelet_Context'Class)
                            return ASF.Converters.Dates.Style_Type is
      Style : constant String := Get_Value (Attr, Context, "");
   begin
      if Style = "default" or else Style = "" then
         return ASF.Converters.Dates.DEFAULT;
      elsif Style = "short" then
         return ASF.Converters.Dates.SHORT;
      elsif Style = "medium" then
         return ASF.Converters.Dates.MEDIUM;
      elsif Style = "long" then
         return ASF.Converters.Dates.LONG;
      elsif Style = "full" then
         return ASF.Converters.Dates.FULL;
      else
         Node.Error ("Invalid attribute {0}: {1}", Name, Style);
         return ASF.Converters.Dates.DEFAULT;
      end if;
   end Get_Date_Style;

   --  ------------------------------
   --  Get the date conversion global format.
   --  ------------------------------
   function Get_Format (Node    : in Convert_Date_Time_Tag_Node;
                        Context : in Contexts.Facelets.Facelet_Context'Class)
                        return ASF.Converters.Dates.Format_Type is
      Format : constant String := Get_Value (Node.Format, Context, "");
   begin
      if Format = "both" then
         return ASF.Converters.Dates.BOTH;
      elsif Format = "time" then
         return ASF.Converters.Dates.TIME;
      elsif Format = "date" then
         return ASF.Converters.Dates.DATE;
      else
         Node.Error ("Invalid attribute type: {0}", Format);
         return ASF.Converters.Dates.BOTH;
      end if;
   end Get_Format;

   --  ------------------------------
   --  Build the component tree from the tag node and attach it as
   --  the last child of the given parent.  Calls recursively the
   --  method to create children.  Get the specified converter and
   --  add it to the parent component.  This operation does not create any
   --  new UIComponent.
   --  ------------------------------
   overriding
   procedure Build_Components (Node    : access Convert_Date_Time_Tag_Node;
                               Parent  : in UIComponent_Access;
                               Context : in out Contexts.Facelets.Facelet_Context'Class) is
      use ASF.Components.Holders;
      use ASF.Converters;

      C      : ASF.Converters.Dates.Date_Converter_Access;
      Locale : constant String := Get_Value (Node.Locale, Context, "");
   begin
      if not (Parent.all in Value_Holder'Class) then
         Node.Error ("Parent component is not an instance of Value_Holder");
         return;
      end if;

      if Node.Pattern /= null then
         C := Dates.Create_Date_Converter (Date    => Converters.Dates.DEFAULT,
                                           Time    => Converters.Dates.DEFAULT,
                                           Format  => Converters.Dates.CONVERTER_PATTERN,
                                           Locale  => Locale,
                                           Pattern => Get_Value (Node.Pattern, Context, ""));

      elsif Node.Format /= null then
         C := Dates.Create_Date_Converter (Date    => Get_Date_Style (Node.all, "dateStyle",
                                                                      Node.Date_Style, Context),
                                           Time    => Get_Date_Style (Node.all, "timeStyle",
                                                                      Node.Time_Style, Context),
                                           Format  => Get_Format (Node.all, Context),
                                           Locale  => Locale,
                                           Pattern => "");

      elsif Node.Date_Style /= null then
         C := Dates.Create_Date_Converter (Date    => Get_Date_Style (Node.all, "dateStyle",
                                                                      Node.Date_Style, Context),
                                           Time    => Converters.Dates.DEFAULT,
                                           Format  => Converters.Dates.DATE,
                                           Locale  => Locale,
                                           Pattern => "");

      elsif Node.Time_Style /= null then
         C := Dates.Create_Date_Converter (Date    => Converters.Dates.DEFAULT,
                                           Time    => Get_Date_Style (Node.all, "timeStyle",
                                                                      Node.Time_Style, Context),
                                           Format  => ASF.Converters.Dates.TIME,
                                           Locale  => Locale,
                                           Pattern => "");
      else
         C := Dates.Create_Date_Converter (Date    => Converters.Dates.DEFAULT,
                                           Time    => Converters.Dates.DEFAULT,
                                           Format  => ASF.Converters.Dates.BOTH,
                                           Locale  => Locale,
                                           Pattern => "");
      end if;
      declare
         VH : constant access Value_Holder'Class
           := Value_Holder'Class (Parent.all)'Access;
      begin
         VH.Set_Converter (Converter => C.all'Access,
                           Release => True);
      end;
   end Build_Components;

   --  ------------------------------
   --  Validator Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Validator Tag
   --  ------------------------------
   function Create_Validator_Tag_Node (Binding    : in Binding_Type;
                                       Line       : in Views.Line_Info;
                                       Parent     : in Views.Nodes.Tag_Node_Access;
                                       Attributes : in Views.Nodes.Tag_Attribute_Array_Access)
                                       return Views.Nodes.Tag_Node_Access is
      Node : constant Validator_Tag_Node_Access := new Validator_Tag_Node;
      Vid  : constant Tag_Attribute_Access := Find_Attribute (Attributes,
                                                              "validatorId");
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
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

   --  ------------------------------
   --  Create the Range_Validator Tag
   --  ------------------------------
   function Create_Range_Validator_Tag_Node (Binding    : in Binding_Type;
                                             Line       : in Views.Line_Info;
                                             Parent     : in Views.Nodes.Tag_Node_Access;
                                             Attributes : in Nodes.Tag_Attribute_Array_Access)
                                             return Views.Nodes.Tag_Node_Access is
      Node : constant Range_Validator_Tag_Node_Access := new Range_Validator_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Minimum := Find_Attribute (Attributes, "minimum");
      Node.Maximum := Find_Attribute (Attributes, "maximum");
      if Node.Minimum = null and then Node.Maximum = null then
         Node.Error ("Missing 'minimum' or 'maximum' attribute");
      end if;
      return Node.all'Access;
   end Create_Range_Validator_Tag_Node;

   --  ------------------------------
   --  Get the validator instance that corresponds to the range validator.
   --  Returns in <b>Validator</b> the validator instance if it exists and indicate
   --  in <b>Shared</b> whether it must be freed or not when the component is deleted.
   --  ------------------------------
   overriding
   procedure Get_Validator (Node      : in Range_Validator_Tag_Node;
                            Context   : in out Contexts.Facelets.Facelet_Context'Class;
                            Validator : out Validators.Validator_Access;
                            Shared    : out Boolean) is
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
   function Create_Length_Validator_Tag_Node (Binding    : in Binding_Type;
                                              Line       : in Views.Line_Info;
                                              Parent     : in Views.Nodes.Tag_Node_Access;
                                              Attributes : in Nodes.Tag_Attribute_Array_Access)
                                              return Views.Nodes.Tag_Node_Access is
      Node : constant Length_Validator_Tag_Node_Access := new Length_Validator_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Minimum := Find_Attribute (Attributes, "minimum");
      Node.Maximum := Find_Attribute (Attributes, "maximum");
      if Node.Minimum = null and then Node.Maximum = null then
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
   --  Regex Validator Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Regex_Validator Tag.  Verifies that the XML node defines
   --  the <b>pattern</b> and that it is a valid regular expression.
   --  ------------------------------
   function Create_Regex_Validator_Tag_Node (Binding    : in Binding_Type;
                                             Line       : in Views.Line_Info;
                                             Parent     : in Views.Nodes.Tag_Node_Access;
                                             Attributes : in Nodes.Tag_Attribute_Array_Access)
                                            return Views.Nodes.Tag_Node_Access is
      Node : constant Regex_Validator_Tag_Node_Access := new Regex_Validator_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Pattern := Find_Attribute (Attributes, "pattern");
      if Node.Pattern = null then
         Node.Error ("Missing 'pattern' attribute");
      end if;
      return Node.all'Access;
   end Create_Regex_Validator_Tag_Node;

   --  ------------------------------
   --  Get the validator instance that corresponds to the validator tag.
   --  Returns in <b>Validator</b> the instance if it exists and indicate
   --  in <b>Shared</b> whether it must be freed or not when the component is deleted.
   --  ------------------------------
   overriding
   procedure Get_Validator (Node      : in Regex_Validator_Tag_Node;
                            Context   : in out Contexts.Facelets.Facelet_Context'Class;
                            Validator : out Validators.Validator_Access;
                            Shared    : out Boolean) is
   begin
      Shared := False;
      if Node.Pattern /= null then
         declare
            Value   : constant EL.Objects.Object := Get_Value (Node.Pattern.all, Context);
            Regexp  : constant String := EL.Objects.To_String (Value);
            Pattern : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile (Regexp);
         begin
            Validator := Validators.Texts.Create_Regex_Validator (Pattern => Pattern);
            return;
         end;
      end if;
      Validator := Validators.Texts.Create_Regex_Validator
        (Pattern => GNAT.Regpat.Compile ("[^.]*"));

   exception
      when E : GNAT.Regpat.Expression_Error =>
         Node.Error ("Invalid pattern: " & Ada.Exceptions.Exception_Message (E));
         Validator := Validators.Texts.Create_Regex_Validator
           (Pattern => GNAT.Regpat.Compile ("[^.]*"));
   end Get_Validator;

   --  ------------------------------
   --  Attribute Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Attribute Tag
   --  ------------------------------
   function Create_Attribute_Tag_Node (Binding    : in Binding_Type;
                                       Line       : in Views.Line_Info;
                                       Parent     : in Views.Nodes.Tag_Node_Access;
                                       Attributes : in Views.Nodes.Tag_Attribute_Array_Access)
                                       return Views.Nodes.Tag_Node_Access is
      Attr : constant Tag_Attribute_Access := Find_Attribute (Attributes, "name");
      Node : Attribute_Tag_Node_Access;
   begin
      Node := new Attribute_Tag_Node;
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
      Node.Attr_Name := Attr;
      Node.Value     := Find_Attribute (Attributes, "value");
      if Node.Attr_Name = null then
         Node.Error ("Missing 'name' attribute");
      else
         Node.Attr.Name := Attr.Value;
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
      if Node.Attr_Name /= null and then Node.Value /= null then
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
      end if;
   end Build_Components;

   --  ------------------------------
   --  Facet Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Facet Tag
   --  ------------------------------
   function Create_Facet_Tag_Node (Binding    : in Binding_Type;
                                   Line       : in Views.Line_Info;
                                   Parent     : in Views.Nodes.Tag_Node_Access;
                                   Attributes : in Views.Nodes.Tag_Attribute_Array_Access)
                                   return Views.Nodes.Tag_Node_Access is
      Node : constant Facet_Tag_Node_Access := new Facet_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
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
      if Node.Facet_Name /= null then
         declare
            Facet : constant UIComponent_Access := new UIComponent;
            Name  : constant Util.Beans.Objects.Object := Get_Value (Node.Facet_Name.all, Context);
         begin
            Node.Build_Children (Facet, Context);
            Parent.Add_Facet (Util.Beans.Objects.To_String (Name), Facet.all'Access, Node);
         end;
      end if;
   end Build_Components;

   --  ------------------------------
   --  Metadata Tag
   --  ------------------------------

   --  ------------------------------
   --  Create the Metadata Tag
   --  ------------------------------
   function Create_Metadata_Tag_Node (Binding    : in Binding_Type;
                                      Line       : in Views.Line_Info;
                                      Parent     : in Views.Nodes.Tag_Node_Access;
                                      Attributes : in Views.Nodes.Tag_Attribute_Array_Access)
                                      return Views.Nodes.Tag_Node_Access is
      Node : constant Metadata_Tag_Node_Access := new Metadata_Tag_Node;
   begin
      Initialize (Node.all'Access, Binding, Line, Parent, Attributes);
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
