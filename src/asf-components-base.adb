-----------------------------------------------------------------------
--  asf-components-base -- Component tree
--  Copyright (C) 2009 - 2022 Stephane Carrez
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
with Util.Log.Loggers;

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with ASF.Views.Nodes;
with ASF.Contexts.Writer;
with ASF.Contexts.Writer.String;
with ASF.Components.Utils;
with ASF.Components.Core;
with ASF.Components.Core.Views;
with ASF.Events.Faces;

with EL.Variables;
with EL.Contexts.Default;
with ASF.Applications.Messages;
with ASF.Applications.Messages.Factory;
package body ASF.Components.Base is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Components.Base");

   procedure Free_Component is
     new Ada.Unchecked_Deallocation (Object => UIComponent'Class,
                                     Name   => UIComponent_Access);

   procedure Free_Attribute is
     new Ada.Unchecked_Deallocation (Object => UIAttribute,
                                     Name   => UIAttribute_Access);

   --  Get the UIAttribute associated with the given name.
   --  Returns null if there is no UIAttribute with such name.
   function Get_Attribute (UI   : in UIComponent;
                           Name : in String) return UIAttribute_Access;

   --  ------------------------------
   --  Get the parent component.
   --  Returns null if the node is the root element.
   --  ------------------------------
   function Get_Parent (UI : UIComponent) return UIComponent_Access is
   begin
      return UI.Parent;
   end Get_Parent;

   --  ------------------------------
   --  Return a client-side identifier for this component, generating
   --  one if necessary.
   --  ------------------------------
   function Get_Client_Id (UI : UIComponent) return Unbounded_String is
   begin
      return UI.Id;
   end Get_Client_Id;

   --  ------------------------------
   --  Returns True if the client-side identifier was generated automatically.
   --  ------------------------------
   function Is_Generated_Id (UI : in UIComponent) return Boolean is
   begin
      return UI.Id_Generated;
   end Is_Generated_Id;

   --  ------------------------------
   --  Returns True if the component has a client-side identifier matching the given name.
   --  ------------------------------
   function Is_Client_Id (UI : in UIComponent;
                          Id : in String) return Boolean is
   begin
      return UI.Id = Id;
   end Is_Client_Id;

   --  ------------------------------
   --  Get the list of children.
   --  ------------------------------
   function Get_Children (UI : UIComponent) return UIComponent_List is
      Result : UIComponent_List;
   begin
      Result.Child := UI.First_Child;
      return Result;
   end Get_Children;

   --  ------------------------------
   --  Get the number of children.
   --  ------------------------------
   function Get_Children_Count (UI : UIComponent) return Natural is
      Result : Natural := 0;
      Child  : UIComponent_Access := UI.First_Child;
   begin
      while Child /= null loop
         Result := Result + 1;
         Child := Child.Next;
      end loop;
      return Result;
   end Get_Children_Count;

   --  ------------------------------
   --  Get the first child component.
   --  Returns null if the component has no children.
   --  ------------------------------
   function Get_First_Child (UI : UIComponent) return UIComponent_Access is
   begin
      return UI.First_Child;
   end Get_First_Child;

   --  ------------------------------
   --  Get the tag node that created this component.
   --  ------------------------------
   function Get_Tag (UI : in UIComponent'Class) return access ASF.Views.Nodes.Tag_Node'Class is
   begin
      return UI.Tag;
   end Get_Tag;

   --  ------------------------------
   --  Initialize the component when restoring the view.
   --  The default initialization gets the client ID and allocates it if necessary.
   --  ------------------------------
   procedure Initialize (UI      : in out UIComponent;
                         Context : in out Faces_Context'Class) is
      --  Then, look in the static attributes
      Attr : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute ("id");
      Id   : Natural;
   begin
      if Attr = null then
         Context.Create_Unique_Id (Id);
         UI.Id := To_Unbounded_String ("id" & Util.Strings.Image (Id));
         UI.Id_Generated := True;
      else
         UI.Id := EL.Objects.To_Unbounded_String (ASF.Views.Nodes.Get_Value (Attr.all, UI));
         UI.Id_Generated := False;
      end if;
   end Initialize;

   procedure Append (UI    : in UIComponent_Access;
                     Child : in UIComponent_Access;
                     Tag   : access ASF.Views.Nodes.Tag_Node'Class) is
   begin
      Child.Tag    := Tag;
      Child.Parent := UI;
      Child.Next   := null;
      if UI.Last_Child = null then
         UI.First_Child := Child;
      else
         UI.Last_Child.Next := Child;
      end if;
      UI.Last_Child := Child;
   end Append;

   --  ------------------------------
   --  Search within the component tree for the {@link UIComponent} with
   --  an <code>id</code> that matches the specified search expression.
   --  ------------------------------
   function Find_Child (UI : in UIComponent'Class;
                        Id : in String) return UIComponent_Access is
      Child : UIComponent_Access := UI.First_Child;
   begin
      while Child /= null loop
         if Child.Is_Client_Id (Id) then
            return Child;
         end if;
         if Child.First_Child /= null then
            declare
               Result : constant UIComponent_Access := Child.Find_Child (Id);
            begin
               if Result /= null then
                  return Result;
               end if;
            end;
         end if;
         Child := Child.Next;
      end loop;
      return null;
   end Find_Child;

   --  ------------------------------
   --  Get the number of facets that this component contains.
   --  ------------------------------
   function Get_Facet_Count (UI : in UIComponent'Class) return Natural is
   begin
      if UI.Facets = null then
         return 0;
      else
         return Natural (UI.Facets.Length);
      end if;
   end Get_Facet_Count;

   --  ------------------------------
   --  Get the facet identified by the given name.
   --  Returns null if there is no such facet.
   --  ------------------------------
   function Get_Facet (UI   : in UIComponent'Class;
                       Name : in String) return UIComponent_Access is
   begin
      if UI.Facets = null then
         return null;
      else
         declare
            Pos : constant Component_Maps.Cursor := UI.Facets.Find (Name);
         begin
            if Component_Maps.Has_Element (Pos) then
               Log.Debug ("Get facet {0}", Name);
               return Component_Maps.Element (Pos);
            else
               Log.Debug ("Facet {0}, not found", Name);
               return null;
            end if;
         end;
      end if;
   end Get_Facet;

   --  ------------------------------
   --  Add the facet represented by the root component <b>Facet</b> under the name <b>Name</b>.
   --  The facet component is added to the facet map and it get be retrieved later on by
   --  using the <b>Get_Facet</b> operation.  The facet component will be destroyed when this
   --  component is deleted.
   --  ------------------------------
   procedure Add_Facet (UI    : in out UIComponent'Class;
                        Name  : in String;
                        Facet : in UIComponent_Access;
                        Tag   : access ASF.Views.Nodes.Tag_Node'Class) is
      Pos : Component_Maps.Cursor;
   begin
      Log.Debug ("Adding facet {0}", Name);

      if UI.Facets = null then
         UI.Facets := new Component_Maps.Map;
      end if;
      Pos := UI.Facets.Find (Name);
      if Component_Maps.Has_Element (Pos) then
         declare
            Facet : UIComponent_Access := Component_Maps.Element (Pos);
         begin
            Free_Component (Facet);
         end;
         UI.Log_Error ("Facet {0} already part of the component tree.", Name);
         UI.Facets.Replace_Element (Pos, Facet);
      else
         UI.Facets.Insert (Name, Facet);
      end if;
      Facet.Tag    := Tag;
      Facet.Parent := UI'Unchecked_Access;
      Facet.Next   := null;
   end Add_Facet;

   --  ------------------------------
   --  Search for and return the {@link UIComponent} with an <code>id</code>
   --  that matches the specified search expression (if any), according to
   --  the algorithm described below.
   --   o look first in the sub-tree representing the parent node.
   --   o if not found, move to the parent's node
   --  Returns null if the component was not found in the view.
   --  ------------------------------
   function Find (UI : in UIComponent;
                  Id : in String) return UIComponent_Access is
      Ignore : UIComponent_Access := null;
      Parent : UIComponent_Access := UI.Parent;
      Node   : UIComponent_Access;
      Result : UIComponent_Access;
   begin
      while Parent /= null loop
         if Parent.Is_Client_Id (Id) then
            return Parent;
         end if;

         --  Search the children recursively but skip the previous sub-tree we come from.
         Node := Parent.First_Child;
         while Node /= null loop
            if Node /= Ignore then
               if Node.Is_Client_Id (Id) then
                  return Node;
               end if;
               Result := Node.Find_Child (Id => Id);
               if Result /= null then
                  return Result;
               end if;
            end if;
            Node := Node.Next;
         end loop;

         --  Move up to the parent and ignore this sub-tree now.
         Ignore := Parent;
         Parent := Parent.Parent;
      end loop;
      return null;
   end Find;

   function Get_Context (UI : in UIComponent)
                         return ASF.Contexts.Faces.Faces_Context_Access is
      pragma Unreferenced (UI);
   begin
      return ASF.Contexts.Faces.Current;
   end Get_Context;

   --  ------------------------------
   --  Check whether the component and its children must be rendered.
   --  ------------------------------
   function Is_Rendered (UI : UIComponent;
                         Context : Faces_Context'Class) return Boolean is
      Attr : constant EL.Objects.Object := UI.Get_Attribute (Context, RENDERED_NAME);
   begin
      if EL.Objects.Is_Null (Attr) then
         return True;
      end if;
      return EL.Objects.To_Boolean (Attr);
   end Is_Rendered;

   --  ------------------------------
   --  Set whether the component is rendered.
   --  ------------------------------
   procedure Set_Rendered (UI       : in out UIComponent;
                           Rendered : in Boolean) is
   begin
      null;
   end Set_Rendered;

   --  ------------------------------
   --  Get the UIAttribute associated with the given name.
   --  Returns null if there is no UIAttribute with such name.
   --  ------------------------------
   function Get_Attribute (UI   : in UIComponent;
                           Name : in String) return UIAttribute_Access is
      use type ASF.Views.Nodes.Tag_Attribute;

      Attribute : UIAttribute_Access := UI.Attributes;
   begin
      --  Look first in the dynamic attribute list (owned by this UIComponent)
      while Attribute /= null loop
         if Attribute.Definition.all = Name then
            return Attribute;
         end if;
         Attribute := Attribute.Next_Attr;
      end loop;
      return null;
   end Get_Attribute;

   function Get_Attribute (UI      : UIComponent;
                           Context : Faces_Context'Class;
                           Name    : String) return EL.Objects.Object is
      Attribute : constant UIAttribute_Access := Get_Attribute (UI, Name);
   begin
      if Attribute /= null then
         begin
            --  The attribute value can be a constant or an expression.
            if not EL.Objects.Is_Null (Attribute.Value) then
               return Attribute.Value;
            else
               return Attribute.Expr.Get_Value (Context.Get_ELContext.all);
            end if;

         exception
            when EL.Variables.No_Variable =>
               UI.Tag.Error ("Variable not found in expression: {0}",
                             Attribute.Expr.Get_Expression);
               return EL.Objects.Null_Object;

            when E : others =>
               Log.Error ("Evaluation error for '" & Attribute.Expr.Get_Expression & "'", E);
               UI.Tag.Error ("Exception raised when evaluating expression: {0}",
                             Attribute.Expr.Get_Expression);
               return EL.Objects.Null_Object;
         end;
      end if;

      --  Then, look in the static attributes
      declare
         Attr : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute (Name);
      begin
         if Attr = null then
            return EL.Objects.Null_Object;
         end if;
         return ASF.Views.Nodes.Get_Value (Attr.all, UI);
      end;
   end Get_Attribute;

   --  ------------------------------
   --  Get the attribute tag
   --  ------------------------------
   function Get_Attribute (UI      : UIComponent;
                           Name    : String)
                           return access ASF.Views.Nodes.Tag_Attribute is
   begin
      if UI.Tag = null then
         return null;
      else
         return UI.Tag.Get_Attribute (Name);
      end if;
   end Get_Attribute;

   --  ------------------------------
   --  Get the attribute value as a boolean.
   --  If the attribute does not exist, returns the default.
   --  ------------------------------
   function Get_Attribute (UI      : in UIComponent;
                           Name    : in String;
                           Context : in Faces_Context'Class;
                           Default : in Boolean := False) return Boolean is
      Attr : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, Name);
   begin
      if Util.Beans.Objects.Is_Null (Attr) then
         return Default;
      else
         return Util.Beans.Objects.To_Boolean (Attr);
      end if;
   end Get_Attribute;

   --  ------------------------------
   --  Get the attribute value as a boolean.
   --  If the attribute does not exist, returns the default.
   --  ------------------------------
   function Get_Attribute (UI      : in UIComponent;
                           Name    : in String;
                           Context : in Faces_Context'Class;
                           Default : in Integer := 0) return Integer is
      Attr : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, Name);
   begin
      if Util.Beans.Objects.Is_Null (Attr) then
         return Default;
      else
         return Util.Beans.Objects.To_Integer (Attr);
      end if;
   end Get_Attribute;

   --  ------------------------------
   --  Get the attribute value as a string.
   --  If the attribute does not exist, returns the default.
   --  ------------------------------
   function Get_Attribute (UI      : in UIComponent;
                           Name    : in String;
                           Context : in Faces_Context'Class;
                           Default : in String := "") return String is
      Attr : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, Name);
   begin
      if Util.Beans.Objects.Is_Null (Attr) then
         return Default;
      else
         return Util.Beans.Objects.To_String (Attr);
      end if;
   end Get_Attribute;

   procedure Set_Attribute (UI    : in out UIComponent;
                            Name  : in String;
                            Value : in EL.Objects.Object) is
   begin
      null;
   end Set_Attribute;

   procedure Set_Attribute (UI    : in out UIComponent;
                            Def   : access ASF.Views.Nodes.Tag_Attribute;
                            Value : in EL.Expressions.Expression) is
      use type ASF.Views.Nodes.Tag_Attribute;

      Attribute : UIAttribute_Access := UI.Attributes;
   begin
      while Attribute /= null loop
         if Def.all = Attribute.Definition.all then --  Attr_Name = Name then
            Attribute.Expr := Value;
            Attribute.Value := EL.Objects.Null_Object;
            return;
         end if;
         Attribute := Attribute.Next_Attr;
      end loop;
      Attribute := new UIAttribute;
      Attribute.Definition := Def;
      Attribute.Value := EL.Objects.Null_Object;
      Attribute.Expr  := Value;
      Attribute.Next_Attr := UI.Attributes;
      UI.Attributes := Attribute;
   end Set_Attribute;

   procedure Set_Attribute (UI    : in out UIComponent;
                            Def   : access ASF.Views.Nodes.Tag_Attribute;
                            Value : in EL.Objects.Object) is
      use type ASF.Views.Nodes.Tag_Attribute;

      Attribute : UIAttribute_Access := UI.Attributes;
   begin
      while Attribute /= null loop
         if Def.all = Attribute.Definition.all then
            Attribute.Value := Value;
            return;
         end if;
         Attribute := Attribute.Next_Attr;
      end loop;
      Attribute := new UIAttribute;
      Attribute.Definition := Def;
      Attribute.Value := Value;
      Attribute.Next_Attr := UI.Attributes;
      UI.Attributes := Attribute;
   end Set_Attribute;

   --  ------------------------------
   --  Get the <b>label</b> attribute from the component.  If the attribute is
   --  empty, returns the client id.
   --  ------------------------------
   function Get_Label (UI      : in UIComponent'Class;
                       Context : in Faces_Context'Class) return Util.Beans.Objects.Object is
      Result : constant Util.Beans.Objects.Object := UI.Get_Attribute (Context, LABEL_NAME);
   begin
      if not Util.Beans.Objects.Is_Null (Result) then
         return Result;
      else
         return Util.Beans.Objects.To_Object (UI.Get_Client_Id);
      end if;
   end Get_Label;

   --  ------------------------------
   --  Get the expression
   --  ------------------------------
   function Get_Expression (UI   : in UIComponent;
                                  Name : in String) return EL.Expressions.Expression is
      Attribute : constant UIAttribute_Access := Get_Attribute (UI, Name);
   begin
      if Attribute /= null then
         begin
            --  The attribute value can be a constant or an expression.
            if EL.Objects.Is_Null (Attribute.Value) then
               return Attribute.Expr;
            end if;

         exception
            when EL.Variables.No_Variable =>
               UI.Tag.Error ("Variable not found in expression: {0}",
                             Attribute.Expr.Get_Expression);

            when E : others =>
               Log.Error ("Evaluation error for '" & Attribute.Expr.Get_Expression & "'", E);
               UI.Tag.Error ("Exception raised when evaluating expression: {0}",
                             Attribute.Expr.Get_Expression);
         end;
      else
         --  Then, look in the static attributes
         declare
            Value : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute (Name);
         begin
            if Value /= null then
               return ASF.Views.Nodes.Get_Expression (Value.all);
            end if;
         end;
      end if;

      raise EL.Expressions.Invalid_Expression with "No value expression for: " & Name;
   end Get_Expression;

   --  ------------------------------
   --  Get the value expression
   --  ------------------------------
   function Get_Value_Expression (UI   : in UIComponent;
                                  Name : in String) return EL.Expressions.Value_Expression is
      Attribute : constant UIAttribute_Access := Get_Attribute (UI, Name);
   begin
      if Attribute /= null then
         begin
            --  The attribute value can be a constant or an expression.
            if EL.Objects.Is_Null (Attribute.Value) then
               return EL.Expressions.Create_Expression (Attribute.Expr);
            end if;

         exception
            when EL.Variables.No_Variable =>
               UI.Tag.Error ("Variable not found in expression: {0}",
                             Attribute.Expr.Get_Expression);

            when E : others =>
               Log.Error ("Evaluation error for '" & Attribute.Expr.Get_Expression & "'", E);
               UI.Tag.Error ("Exception raised when evaluating expression: {0}",
                             Attribute.Expr.Get_Expression);
         end;
      else
         --  Then, look in the static attributes
         declare
            Value : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute (Name);
         begin
            if Value /= null then
               return ASF.Views.Nodes.Get_Value_Expression (Value.all);
            end if;
         end;
      end if;

      raise EL.Expressions.Invalid_Expression with "No value expression for: " & Name;
   end Get_Value_Expression;

   --  ------------------------------
   --  Get the method expression
   --  Raise an Invalid_Expression if the method expression is invalid.
   --  ------------------------------
   function Get_Method_Expression (UI  : in UIComponent;
                                  Name : in String) return EL.Expressions.Method_Expression is
      Attribute : constant UIAttribute_Access := Get_Attribute (UI, Name);
   begin
      if Attribute /= null then
         begin
            --  The attribute value can be a constant or an expression.
            if EL.Objects.Is_Null (Attribute.Value) then
               return EL.Expressions.Create_Expression (Attribute.Expr);
            end if;

         exception
            when EL.Variables.No_Variable =>
               UI.Tag.Error ("Variable not found in expression: {0}",
                             Attribute.Expr.Get_Expression);

            when E : others =>
               Log.Error ("Evaluation error for '" & Attribute.Expr.Get_Expression & "'", E);
               UI.Tag.Error ("Exception raised when evaluating expression: {0}",
                             Attribute.Expr.Get_Expression);
         end;
      else
         --  Then, look in the static attributes
         declare
            Value : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute (Name);
         begin
            if Value /= null then
               return ASF.Views.Nodes.Get_Method_Expression (Value.all);
            end if;
         end;
      end if;

      raise EL.Expressions.Invalid_Expression with "No method expression for: " & Name;
   end Get_Method_Expression;

   --  ------------------------------
   --  Add a message for the component.  Look for the message attribute identified
   --  by <b>Name</b> on the <b>UI</b> component.  Add this message in the faces context
   --  and associated with the component client id.  Otherwise, add the default
   --  message whose bundle key is identified by <b>default</b>.
   --  ------------------------------
   procedure Add_Message (UI      : in UIComponent'Class;
                          Name    : in String;
                          Default : in String;
                          Context : in out Faces_Context'Class) is
      Args : constant ASF.Utils.Object_Array (1 .. 0) := (others => <>);
   begin
      UI.Add_Message (Name, Default, Args, Context);
   end Add_Message;

   --  ------------------------------
   --  Add a message for the component.  Look for the message attribute identified
   --  by <b>Name</b> on the <b>UI</b> component.  Add this message in the faces context
   --  and associated with the component client id.  Otherwise, add the default
   --  message whose bundle key is identified by <b>default</b>.
   --  ------------------------------
   procedure Add_Message (UI      : in UIComponent'Class;
                          Name    : in String;
                          Default : in String;
                          Arg1    : in Util.Beans.Objects.Object;
                          Context : in out Faces_Context'Class) is
      Args : constant ASF.Utils.Object_Array (1 .. 1) := (1 => Arg1);
   begin
      UI.Add_Message (Name, Default, Args, Context);
   end Add_Message;

   --  ------------------------------
   --  Add a message for the component.  Look for the message attribute identified
   --  by <b>Name</b> on the <b>UI</b> component.  Add this message in the faces context
   --  and associated with the component client id.  Otherwise, add the default
   --  message whose bundle key is identified by <b>default</b>.
   --  ------------------------------
   procedure Add_Message (UI      : in UIComponent'Class;
                          Name    : in String;
                          Default : in String;
                          Arg1    : in Util.Beans.Objects.Object;
                          Arg2    : in Util.Beans.Objects.Object;
                          Context : in out Faces_Context'Class) is
      Args : constant ASF.Utils.Object_Array (1 .. 2) := (1 => Arg1, 2 => Arg2);
   begin
      UI.Add_Message (Name, Default, Args, Context);
   end Add_Message;

   --  ------------------------------
   --  Add a message for the component.  Look for the message attribute identified
   --  by <b>Name</b> on the <b>UI</b> component.  Add this message in the faces context
   --  and associated with the component client id.  Otherwise, use the default
   --  message whose bundle key is identified by <b>default</b>.  The message is
   --  formatted with the arguments passed in <b>Args</b>.
   --  ------------------------------
   procedure Add_Message (UI      : in UIComponent'Class;
                          Name    : in String;
                          Default : in String;
                          Args    : in ASF.Utils.Object_Array;
                          Context : in out Faces_Context'Class) is
      Id  : constant String := To_String (UI.Get_Client_Id);
      Msg : constant EL.Objects.Object := UI.Get_Attribute (Name => Name, Context => Context);
      Message : ASF.Applications.Messages.Message;
   begin
      if EL.Objects.Is_Null (Msg) then
         Message := ASF.Applications.Messages.Factory.Get_Message (Context    => Context,
                                                                   Message_Id => Default,
                                                                   Args       => Args);
      else
         --  The message is already localized, we just need to set and format it.
         ASF.Applications.Messages.Set_Severity (Message, ASF.Applications.Messages.ERROR);
         ASF.Applications.Messages.Format_Summary (Message, EL.Objects.To_String (Msg), Args);
      end if;
      Context.Add_Message (Client_Id => Id,
                           Message   => Message);
   end Add_Message;

   procedure Encode_Begin (UI      : in UIComponent;
                           Context : in out Faces_Context'Class) is
   begin
      null;
   end Encode_Begin;

   procedure Encode_Children (UI      : in UIComponent;
                              Context : in out Faces_Context'Class) is
      Child : UIComponent_Access;
   begin
      --  Do not render the children if the component is not rendered.
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      Child := UI.First_Child;
      while Child /= null loop
         Child.Encode_All (Context);
         Child := Child.Next;
      end loop;
   end Encode_Children;

   --  ------------------------------
   --  Encode the children components in a local buffer and after the rendering execute
   --  the <b>Process</b> procedure with the generated content.
   --  If this component is not rendered, do nothing.
   --  ------------------------------
   procedure Wrap_Encode_Children (UI      : in UIComponent;
                                   Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                                   Process : not null
                                   access procedure (Content : in Unbounded_String)) is
      Child : UIComponent_Access := UI.First_Child;
   begin
      if not UI.Is_Rendered (Context) then
         return;

      elsif Child = null then
         Process (Null_Unbounded_String);

      else
         --  Replace temporarily the response writer by a local buffer.
         --  Make sure that if an exception is raised, the original response writer is restored.
         declare
            Writer : constant Contexts.Writer.Response_Writer_Access
              := Context.Get_Response_Writer;
            Buffer : aliased ASF.Contexts.Writer.String.String_Writer;
         begin
            Context.Set_Response_Writer (Buffer'Unchecked_Access);
            while Child /= null loop
               Child.Encode_All (Context);
               Child := Child.Next;
            end loop;
            Context.Set_Response_Writer (Writer);

            Process (Buffer.Get_Response);

         exception
            when others =>
               Context.Set_Response_Writer (Writer);
               raise;
         end;
      end if;
   end Wrap_Encode_Children;

   procedure Wrap_Encode_Children (UI      : in UIComponent;
                                   Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                                   Process : not null access procedure (Content : in String)) is
      Child : UIComponent_Access := UI.First_Child;
   begin
      if not UI.Is_Rendered (Context) then
         return;

      elsif Child = null then
         Process ("");

      else
         --  Replace temporarily the response writer by a local buffer.
         --  Make sure that if an exception is raised, the original response writer is restored.
         declare
            Writer : constant Contexts.Writer.Response_Writer_Access
              := Context.Get_Response_Writer;
            Buffer : aliased ASF.Contexts.Writer.String.String_Writer;
         begin
            Context.Set_Response_Writer (Buffer'Unchecked_Access);
            while Child /= null loop
               Child.Encode_All (Context);
               Child := Child.Next;
            end loop;
            Context.Set_Response_Writer (Writer);

            Process (Ada.Strings.Unbounded.To_String (Buffer.Get_Response));

         exception
            when others =>
               Context.Set_Response_Writer (Writer);
               raise;
         end;
      end if;
   end Wrap_Encode_Children;

   procedure Encode_End (UI      : in UIComponent;
                         Context : in out Faces_Context'Class) is
   begin
      null;
   end Encode_End;

   procedure Encode_All (UI      : in UIComponent'Class;
                         Context : in out Faces_Context'Class) is
   begin
      UI.Encode_Begin (Context);
      UI.Encode_Children (Context);
      UI.Encode_End (Context);
   end Encode_All;

   procedure Decode (UI      : in out UIComponent;
                     Context : in out Faces_Context'Class) is
   begin
      null;
   end Decode;

   procedure Decode_Children (UI      : in UIComponent'Class;
                              Context : in out Faces_Context'Class) is
      Child : UIComponent_Access;
   begin
      Child := UI.First_Child;
      while Child /= null loop
         Child.Process_Decodes (Context);
         Child := Child.Next;
      end loop;
   end Decode_Children;

   procedure Process_Decodes (UI      : in out UIComponent;
                              Context : in out Faces_Context'Class) is
   begin
      --  Do not decode the component nor its children if the component is not rendered.
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      UI.Decode_Children (Context);
      UIComponent'Class (UI).Decode (Context);
   end Process_Decodes;

   --  ------------------------------
   --  Perform the component tree processing required by the <b>Process Validations</b>
   --  phase of the request processing lifecycle for all facets of this component,
   --  all children of this component, and this component itself, as follows:
   --  <ul>
   --    <li>If this component <b>rendered</b> property is false, skip further processing.
   --    <li>Call the <b>Process_Validators</b> of all facets and children.
   --  <ul>
   --  ------------------------------
   procedure Process_Validators (UI      : in out UIComponent;
                                 Context : in out Faces_Context'Class) is
      Child : UIComponent_Access;
   begin
      --  Do not process validation of the component nor its children
      --  if this component is not rendered.
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      Child := UI.First_Child;
      while Child /= null loop
         Child.Process_Validators (Context);
         Child := Child.Next;
      end loop;
   end Process_Validators;

   procedure Process_Updates (UI      : in out UIComponent;
                              Context : in out Faces_Context'Class) is
      Child : UIComponent_Access;
   begin
      --  Do not decode the component nor its children if the component is not rendered.
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      Child := UI.First_Child;
      while Child /= null loop
         Child.Process_Updates (Context);
         Child := Child.Next;
      end loop;
   end Process_Updates;

   --  ------------------------------
   --  Queue an event for broadcast at the end of the current request
   --  processing lifecycle phase.  The default implementation in
   --  delegates this call to the parent component.  The <b>UIViewRoot</b>
   --  component is in charge of queueing events.  The event object
   --  will be freed after being dispatched.
   --  ------------------------------
   procedure Queue_Event (UI    : in out UIComponent;
                          Event : not null access ASF.Events.Faces.Faces_Event'Class) is
   begin
      if UI.Parent = null then
         Log.Error ("The component tree does not have a UIView root component. Event ignored.");
      else
         UI.Parent.Queue_Event (Event);
      end if;
   end Queue_Event;

   --  ------------------------------
   --  Broadcast the event to the event listeners installed on this component.
   --  Listeners are called in the order in which they were added.
   --  ------------------------------
   procedure Broadcast (UI      : in out UIComponent;
                        Event   : not null access ASF.Events.Faces.Faces_Event'Class;
                        Context : in out Faces_Context'Class) is
      pragma Unreferenced (UI, Event, Context);
   begin
      Log.Error ("Event dispatched to a component that cannot handle it");
   end Broadcast;

   --  ------------------------------
   --  Finalize the object.
   --  ------------------------------
   overriding
   procedure Finalize (UI : in out UIComponent) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Component_Maps.Map,
                                         Name   => Component_Map_Access);
   begin
      --  If this component has some facets, we have to delete them.
      if UI.Facets /= null then
         loop
            declare
               Iter : Component_Maps.Cursor := UI.Facets.First;
               Item : UIComponent_Access;
            begin
               exit when not Component_Maps.Has_Element (Iter);
               Item := Component_Maps.Element (Iter);
               Free_Component (Item);
               UI.Facets.Delete (Iter);
            end;
         end loop;
         Free (UI.Facets);
      end if;

      --  Release the dynamic attributes.
      declare
         A : UIAttribute_Access := UI.Attributes;
      begin
         while A /= null loop
            UI.Attributes := A.Next_Attr;
            Free_Attribute (A);
            A := UI.Attributes;
         end loop;
      end;

      --  And release the children of this component recursively.
      declare
         C : UIComponent_Access := UI.First_Child;
      begin
         while C /= null loop
            UI.First_Child := C.Next;
            Free_Component (C);
            C := UI.First_Child;
         end loop;
      end;
   end Finalize;

   --  ------------------------------
   --  Iterate over the children of the component and execute
   --  the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate (UI : in UIComponent'Class) is
      Child : UIComponent_Access := UI.First_Child;
   begin
      while Child /= null loop
         Process (Child);
         Child := Child.Next;
      end loop;
   end Iterate;

   --  ------------------------------
   --  Iterate over the attributes defined on the component and
   --  execute the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate_Attributes (UI : in UIComponent'Class) is
      Attribute : UIAttribute_Access := UI.Attributes;

      procedure Process_Tag_Attribute (Attr : in ASF.Views.Nodes.Tag_Attribute_Access);

      procedure Process_Tag_Attribute (Attr : in ASF.Views.Nodes.Tag_Attribute_Access) is
         A : UIAttribute;
         N : UIAttribute_Access := UI.Attributes;
      begin
         while N /= null loop
            if N.Definition = Attr then
               return;
            end if;
            N := N.Next_Attr;
         end loop;
         A.Definition := Attr;
         Process (ASF.Views.Nodes.Get_Name (Attr.all), A);
      end Process_Tag_Attribute;

      procedure Iterate_Tag_Attributes is new
        ASF.Views.Nodes.Iterate_Attributes (Process_Tag_Attribute);

   begin
      --  Iterate first over the component modified attributes.
      while Attribute /= null loop
         Process (ASF.Views.Nodes.Get_Name (Attribute.Definition.all), Attribute.all);
         Attribute := Attribute.Next_Attr;
      end loop;

      Iterate_Tag_Attributes (UI.Tag.all);
   end Iterate_Attributes;

   --  ------------------------------
   --  Get the attribute value.
   --  ------------------------------
   function Get_Value (Attr : UIAttribute;
                       UI   : UIComponent'Class) return EL.Objects.Object is

      procedure Handle_Exception (E : in Ada.Exceptions.Exception_Occurrence);

      procedure Handle_Exception (E : in Ada.Exceptions.Exception_Occurrence) is
      begin
         ASF.Views.Nodes.Error (Attr.Definition.all, "Evaluation error: {0}",
                                Ada.Exceptions.Exception_Message (E));
      end Handle_Exception;

   begin
      if not EL.Objects.Is_Null (Attr.Value) then
         return Attr.Value;

      elsif not Attr.Expr.Is_Null then
         declare
            Ctx     : constant EL.Contexts.ELContext_Access := UI.Get_Context.Get_ELContext;
            Context : EL.Contexts.Default.Guarded_Context (Handle_Exception'Access, Ctx);
         begin
            return Attr.Expr.Get_Value (Context);
         end;

      else
         return ASF.Views.Nodes.Get_Value (Attr.Definition.all, UI);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Report an error message in the logs caused by an invalid configuration or
   --  setting on the component.
   --  ------------------------------
   procedure Log_Error (UI      : in UIComponent'Class;
                        Message : in String;
                        Arg1    : in String := "";
                        Arg2    : in String := "";
                        Arg3    : in String := "") is
   begin
      Log.Error (Utils.Get_Line_Info (UI) & ": " & Message, Arg1, Arg2, Arg3);
   end Log_Error;

   --  ------------------------------
   --  Get the root component from the <b>UI</b> component tree.
   --  After the operation, the <b>UI</b> component tree will contain no
   --  nodes.
   --  If the <b>Root</b> pointer is not null, first deletes recursively
   --  the component tree.
   --  ------------------------------
   procedure Steal_Root_Component (UI   : in out UIComponent'Class;
                                   Root : in out UIComponent_Access) is
      procedure Move_Siblings (Tree : in UIComponent_Access);

      --  Move siblings of the component at end of children list.
      procedure Move_Siblings (Tree : in UIComponent_Access) is
         Node, Prev : UIComponent_Access;
      begin
         Node := Tree.Next;
         Tree.Next := null;
         if Tree.Last_Child = null then
            Tree.First_Child := Node;
         else
            Tree.Last_Child.Next := Node;
         end if;

         --  And reparent these left nodes.
         while Node /= null loop
            Prev := Node;
            Node.Parent := Tree;
            Node := Node.Next;
         end loop;
         Tree.Last_Child := Prev;
      end Move_Siblings;

   begin
      if Root /= null then
         Free_Component (Root);
      end if;
      if UI.First_Child = null then
         Root := null;
      elsif UI.First_Child.Next = null
        and then UI.First_Child.all in ASF.Components.Core.Views.UIView'Class
      then
         Root := UI.First_Child;
         Root.Parent    := null;
         UI.First_Child := null;
         UI.Last_Child  := null;
      else
         declare
            View : Core.Views.UIView_Access;
            Tree : UIComponent_Access := UI.First_Child;
            Node : UIComponent_Access := Tree;
            Prev : UIComponent_Access := null;
         begin
            while Node /= null and then not (Node.all in Core.Views.UIView'Class) loop
               Prev := Node;
               Node := Node.Next;
            end loop;
            if Node /= null then
               View := Core.Views.UIView'Class (Node.all)'Access;

               --  Move the left components below the <f:view> component.
               if Prev /= null then
                  Prev.Next := null;

                  --  Reparent the first left node to the real <f:view> root component
                  --  and make it the root of the left tree before the <f:view>.
                  Tree.Parent := View.all'Access;
                  View.Set_Before_View (Tree);

                  --  Move other left nodes at end of children list of our new left tree.
                  --  This is necessary to correctly release them.
                  Move_Siblings (Tree);
                  Node := View.all'Access;
               end if;

               --  Move the right components below the <f:view> component.
               if Node.Next /= null then
                  Tree := Node.Next;

                  --  Reparent the first right node to the real <f:view> root component
                  --  and make it the root of the right tree after the <f:view>.
                  Tree.Parent := View.all'Access;
                  View.Set_After_View (Tree);

                  Move_Siblings (Tree);
                  Node := View.all'Access;
               end if;

            else
               View := new Components.Core.Views.UIView;
               View.Set_Content_Type ("text/html");

               Root := View.all'Access;
               Root.First_Child := UI.First_Child;
               Root.Last_Child  := UI.Last_Child;

               Node := UI.First_Child;
               --  Reparent the children.
               while Node /= null loop
                  Node.Parent := Root;
                  Node := Node.Next;
               end loop;
            end if;

            Root := View.all'Access;
            Root.Parent    := null;
            UI.First_Child := null;
            UI.Last_Child  := null;
         end;
      end if;
   end Steal_Root_Component;

   --  ------------------------------
   --  Get an iterator to scan the component children.
   --  ------------------------------
   function First (UI : in UIComponent'Class) return Cursor is
   begin
      return Cursor '(Child => UI.First_Child);
   end First;

   --  ------------------------------
   --  Returns True if the iterator points to a valid child.
   --  ------------------------------
   function Has_Element (Pos : in Cursor) return Boolean is
   begin
      return Pos.Child /= null;
   end Has_Element;

   --  ------------------------------
   --  Get the child component pointed to by the iterator.
   --  ------------------------------
   function Element (Pos : in Cursor) return UIComponent_Access is
   begin
      return Pos.Child;
   end Element;

   --  ------------------------------
   --  Move to the next child.
   --  ------------------------------
   procedure Next (Pos : in out Cursor) is
   begin
      if Pos.Child /= null then
         Pos.Child := Pos.Child.Next;
      end if;
   end Next;

end ASF.Components.Base;
