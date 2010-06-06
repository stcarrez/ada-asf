-----------------------------------------------------------------------
--  contexts-facelets -- Contexts for facelets
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

with EL.Objects;
with EL.Contexts;
with ASF.Components;
with Ada.Strings.Unbounded;
package ASF.Contexts.Facelets is

   use ASF.Components;
   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Facelet context
   --  ------------------------------
   --  The <b>Facelet_Context</b> defines a context used exclusively when
   --  building the component tree from the facelet nodes.  It allows to
   --  compose the component tree by using other facelet fragments.
   type Facelet_Context is tagged private;

   type Facelet_Context_Access is access all Facelet_Context'Class;

   --  Get the EL context for evaluating expressions.
   function Get_ELContext (Context : in Facelet_Context)
                           return EL.Contexts.ELContext_Access;

   --  Set the EL context for evaluating expressions.
   procedure Set_ELContext (Context   : in out Facelet_Context;
                            ELContext : in EL.Contexts.ELContext_Access);

   --  Set the attribute having given name with the value.
   procedure Set_Attribute (Context : in out Facelet_Context;
                            Name    : in String;
                            Value   : in EL.Objects.Object);

   --  Set the attribute having given name with the value.
   procedure Set_Attribute (Context : in out Facelet_Context;
                            Name    : in Unbounded_String;
                            Value   : in EL.Objects.Object);

   --  Include the facelet from the given source file.
   --  The included views appended to the parent component tree.
   procedure Include_Facelet (Context : in out Facelet_Context;
                              Source  : in String;
                              Parent  : in UIComponent_Access);

   --  Include the definition having the given name.
   procedure Include_Definition (Context : in out Facelet_Context;
                                 Name    : in String;
                                 Parent  : in UIComponent_Access);
private

   type Facelet_Context is tagged record
      --  The expression context;
      Context : EL.Contexts.ELContext_Access;
   end record;

end ASF.Contexts.Facelets;
