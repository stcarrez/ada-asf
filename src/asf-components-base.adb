-----------------------------------------------------------------------
--  components -- Component tree
--  Copyright (C) 2009, 2010, 2011, 2012 Stephane Carrez
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
with ASF.Converters;
with ASF.Contexts.Writer;
with ASF.Contexts.Writer.String;
with ASF.Components.Utils;
with ASF.Components.Core;
with ASF.Components.Core.Views;

with EL.Variables;
with EL.Contexts.Default;
with ASF.Applications.Messages;
with ASF.Applications.Messages.Factory;
package body ASF.Components.Base is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Components.Base");

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


   function Get_Attribute (UI      : UIComponent;
                           Context : Faces_Context'Class;
                           Name    : String) return EL.Objects.Object is
      Attribute : UIAttribute_Access := UI.Attributes;
   begin
      --  Look first in the dynamic attribute list (owned by this UIComponent)
      while Attribute /= null loop
         declare
            Attr_Name : constant String := ASF.Views.Nodes.Get_Name (Attribute.Definition.all);
         begin
            if Attr_Name = Name then
               --  The attribute value can be a constant or an expression.
               if not EL.Objects.Is_Null (Attribute.Value) then
                  return Attribute.Value;
               else
                  return Attribute.Expr.Get_Value (Context.Get_ELContext.all);
               end if;
            end if;

         exception
            when EL.Variables.No_Variable =>
               UI.Tag.Error ("Variable not found in expression: {0}",
                             Attribute.Expr.Get_Expression);
               return EL.Objects.Null_Object;
         end;
         Attribute := Attribute.Next_Attr;
      end loop;

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
      Attribute : UIAttribute_Access := UI.Attributes;
      Name      : constant String := ASF.Views.Nodes.Get_Name (Def.all);
   begin
      while Attribute /= null loop
         declare
            Attr_Name : constant String := ASF.Views.Nodes.Get_Name (Attribute.Definition.all);
         begin
            if Attr_Name = Name then
               Attribute.Expr := Value;
               Attribute.Value := EL.Objects.Null_Object;
               return;
            end if;
         end;
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
      Attribute : UIAttribute_Access := UI.Attributes;
      Name      : constant String := ASF.Views.Nodes.Get_Name (Def.all);
   begin
      while Attribute /= null loop
         declare
            Attr_Name : constant String := ASF.Views.Nodes.Get_Name (Attribute.Definition.all);
         begin
            if Attr_Name = Name then
               Attribute.Value := Value;
               return;
            end if;
         end;
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
   --  Get the value expression
   --  ------------------------------
   function Get_Value_Expression (UI   : in UIComponent;
                                  Name : in String) return EL.Expressions.Value_Expression is
      Value : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute (Name);
   begin
      if Value = null then
         raise EL.Expressions.Invalid_Expression with "No value expression for: " & Name;
      end if;

      return ASF.Views.Nodes.Get_Value_Expression (Value.all);
   end Get_Value_Expression;

   --  ------------------------------
   --  Get the method expression
   --  Raise an Invalid_Expression if the method expression is invalid.
   --  ------------------------------
   function Get_Method_Expression (UI  : in UIComponent;
                                  Name : in String) return EL.Expressions.Method_Expression is
      Value : constant access ASF.Views.Nodes.Tag_Attribute := UI.Get_Attribute (Name);
   begin
      if Value = null then
         raise EL.Expressions.Invalid_Expression with "No method expression for: " & Name;
      end if;

      return ASF.Views.Nodes.Get_Method_Expression (Value.all);
   end Get_Method_Expression;

   --  ------------------------------
   --  Get the converter associated with the component
   --  ------------------------------
   function Get_Converter (UI      : in UIComponent;
                           Context : in Faces_Context'Class)
                           return access ASF.Converters.Converter'Class is
      Name : constant EL.Objects.Object
        := UIComponent'Class (UI).Get_Attribute (Name    => CONVERTER_NAME,
                                                 Context => Context);
   begin
      return Context.Get_Converter (Name);
   end Get_Converter;

   --  ------------------------------
   --  Convert the string into a value.  If a converter is specified on the component,
   --  use it to convert the value.
   --  ------------------------------
   function Convert_Value (UI      : in UIComponent;
                           Value   : in String;
                           Context : in Faces_Context'Class) return EL.Objects.Object is
      Convert : constant access ASF.Converters.Converter'Class
        := UIComponent'Class (UI).Get_Converter (Context);
   begin
      if Convert = null then
         return EL.Objects.To_Object (Value);
      else
         return Convert.To_Object (Context   => Context,
                                   Component => UI,
                                   Value     => Value);
      end if;
   end Convert_Value;

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
      Id  : constant String := To_String (UI.Get_Client_Id);
      Msg : constant EL.Objects.Object := UI.Get_Attribute (Name => Name, Context => Context);
   begin
      if EL.Objects.Is_Null (Msg) then
         Context.Add_Message (Client_Id => Id, Message => Default);
      else
         Context.Add_Message (Client_Id => Id, Message => EL.Objects.To_String (Msg));
      end if;
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
         Message := ASF.Applications.Messages.Factory.Get_Message
           (Context    => Context,
            Message_Id => EL.Objects.To_String (Msg),
            Args       => Args);
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
      Log.Info (Utils.Get_Line_Info (UI) & ": " & Message, Arg1, Arg2, Arg3);
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
   begin
      if Root /= null then
         Delete (Root);
      end if;
      if UI.First_Child = null then
         Root := null;
      elsif UI.First_Child.Next = null
        and then UI.First_Child.all in ASF.Components.Core.Views.UIView'Class then
         Root := UI.First_Child;
         Root.Parent    := null;
         UI.First_Child := null;
         UI.Last_Child  := null;
      else
         declare
            View  : constant Core.Views.UIView_Access := new Components.Core.Views.UIView;
            Child : UIComponent_Access := UI.First_Child;
         begin
            View.Set_Content_Type ("text/html");

            Root := View.all'Access;
            Root.First_Child := UI.First_Child;
            Root.Last_Child  := UI.Last_Child;
            Root.Parent      := null;

            --  Reparent the children.
            while Child /= null loop
               Child.Parent := Root;
               Child := Child.Next;
            end loop;
         end;
         UI.First_Child := null;
         UI.Last_Child  := null;
      end if;
   end Steal_Root_Component;

   procedure Free_Component is
     new Ada.Unchecked_Deallocation (Object => UIComponent'Class,
                                     Name   => UIComponent_Access);

   procedure Free_Attribute is
     new Ada.Unchecked_Deallocation (Object => UIAttribute,
                                     Name   => UIAttribute_Access);

   --  ------------------------------
   --  Delete the component tree recursively.
   --  ------------------------------
   procedure Delete (UI : in out UIComponent_Access) is
   begin
      if UI /= null then
         declare
            C : UIComponent_Access := UI.First_Child;
         begin
            while C /= null loop
               UI.First_Child := C.Next;
               Delete (C);
               C := UI.First_Child;
            end loop;
         end;
         declare
            A : UIAttribute_Access := UI.Attributes;
         begin
            while A /= null loop
               UI.Attributes := A.Next_Attr;
               Free_Attribute (A);
               A := UI.Attributes;
            end loop;
         end;
         Free_Component (UI);
      end if;
   end Delete;

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
