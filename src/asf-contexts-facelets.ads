-----------------------------------------------------------------------
--  contexts-facelets -- Contexts for facelets
--  Copyright (C) 2009, 2010, 2011, 2018 Stephane Carrez
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

with Util.Strings;
with EL.Objects;
with EL.Contexts;
with EL.Expressions;
with EL.Functions;
with ASF.Components.Base;
with ASF.Converters;
with ASF.Validators;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
limited with ASF.Views.Nodes.Facelets;
package ASF.Contexts.Facelets is

   use ASF.Components;
   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Facelet context
   --  ------------------------------
   --  The <b>Facelet_Context</b> defines a context used exclusively when
   --  building the component tree from the facelet nodes.  It allows to
   --  compose the component tree by using other facelet fragments.
   type Facelet_Context is abstract tagged private;

   type Facelet_Context_Access is access all Facelet_Context'Class;

   --  Get the EL context for evaluating expressions.
   function Get_ELContext (Context : in Facelet_Context)
                           return EL.Contexts.ELContext_Access;

   --  Set the EL context for evaluating expressions.
   procedure Set_ELContext (Context   : in out Facelet_Context;
                            ELContext : in EL.Contexts.ELContext_Access);

   --  Get the function mapper associated with the EL context.
   function Get_Function_Mapper (Context : in Facelet_Context)
     return EL.Functions.Function_Mapper_Access;

   --  Set the attribute having given name with the value.
   procedure Set_Attribute (Context : in out Facelet_Context;
                            Name    : in String;
                            Value   : in EL.Objects.Object);

   --  Set the attribute having given name with the value.
   procedure Set_Attribute (Context : in out Facelet_Context;
                            Name    : in Unbounded_String;
                            Value   : in EL.Objects.Object);

   --  Set the attribute having given name with the expression.
   procedure Set_Variable (Context : in out Facelet_Context;
                           Name    : in Unbounded_String;
                           Value   : in EL.Expressions.Expression);

   --  Set the attribute having given name with the expression.
   procedure Set_Variable (Context : in out Facelet_Context;
                           Name    : in String;
                           Value   : in EL.Expressions.Expression);

   --  Include the facelet from the given source file.
   --  The included views appended to the parent component tree.
   procedure Include_Facelet (Context : in out Facelet_Context;
                              Source  : in String;
                              Parent  : in Base.UIComponent_Access);

   --  Include the definition having the given name.
   procedure Include_Definition (Context : in out Facelet_Context;
                                 Name    : in Unbounded_String;
                                 Parent  : in Base.UIComponent_Access;
                                 Found   : out Boolean);

   --  Push into the current facelet context the <ui:define> nodes contained in
   --  the composition/decorate tag.
   procedure Push_Defines (Context : in out Facelet_Context;
                           Node : access ASF.Views.Nodes.Facelets.Composition_Tag_Node);

   --  Pop from the current facelet context the <ui:define> nodes.
   procedure Pop_Defines (Context : in out Facelet_Context);

   --  Set the path to resolve relative facelet paths and get the previous path.
   procedure Set_Relative_Path (Context  : in out Facelet_Context;
                                Path     : in String;
                                Previous : out Unbounded_String);

   --  Set the path to resolve relative facelet paths.
   procedure Set_Relative_Path (Context  : in out Facelet_Context;
                                Path     : in Unbounded_String);

   --  Resolve the facelet relative path
   function Resolve_Path (Context : Facelet_Context;
                          Path    : String) return String;

   --  Get a converter from a name.
   --  Returns the converter object or null if there is no converter.
   function Get_Converter (Context : in Facelet_Context;
                           Name    : in EL.Objects.Object)
                           return ASF.Converters.Converter_Access is abstract;

   --  Get a validator from a name.
   --  Returns the validator object or null if there is no validator.
   function Get_Validator (Context : in Facelet_Context;
                           Name    : in EL.Objects.Object)
                           return ASF.Validators.Validator_Access is abstract;

private

   type Composition_Tag_Node is access all ASF.Views.Nodes.Facelets.Composition_Tag_Node'Class;

   package Defines_Vector is
     new Ada.Containers.Vectors (Index_Type   => Natural,
                                 Element_Type => Composition_Tag_Node);

   type Facelet_Context is abstract tagged record
      --  The expression context;
      Context : EL.Contexts.ELContext_Access := null;
      Defines : Defines_Vector.Vector;
      Path    : Unbounded_String;
      Inserts : Util.Strings.String_Set.Set;
   end record;

end ASF.Contexts.Facelets;
