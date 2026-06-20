-----------------------------------------------------------------------
--  asf-components-utils-json_ld -- JSON-LD generation support
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Beans.Objects;
with Util.Beans.Objects.Time;
with ASF.Contexts.Writer;
with Util.Dates.ISO8601;
package body ASF.Components.Utils.JSON_LD is

   package UBO renames Util.Beans.Objects;
   use type UBO.Data_Type;
   procedure Write_Attributes (UI      : in ASF.Components.Core.UIComponentBase'Class;
                               Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                               Stream  : in Output_Stream_Access);

   procedure Write_Attributes (UI      : in ASF.Components.Core.UIComponentBase'Class;
                               Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                               Stream  : in Output_Stream_Access) is
      Jid   : constant UBO.Object := UI.Get_Attribute (Context, "jid");
      Jtype : constant UBO.Object := UI.Get_Attribute (Context, "jtype");
   begin
      if not UBO.Is_Null (Jid) then
         Stream.Write_Attribute ("@id", Jid);
      end if;
      if not UBO.Is_Null (Jtype) then
         Stream.Write_Attribute ("@type", Jtype);
      end if;
   end Write_Attributes;

   procedure Initialize (UI : in out UIJSON) is
   begin
      UI.Buffer.Initialize (Size => 10000);
      UI.Print.Initialize (UI.Buffer'Unchecked_Access);
      UI.Output.Initialize (UI.Print'Unchecked_Access);
      UI.Stream := UI.Output'Unchecked_Access;
      UI.Output.Start_Document;
      UI.Output.Start_Entity ("");
   end Initialize;

   --  Flush the javascript queue
   overriding
   procedure Encode_Begin (UI      : in UIJSON;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Attr   : constant String := UI.Get_Attribute ("type", Context);
      begin
         if Attr'Length = 0 then
            UI.Stream.Write_Attribute ("@context", "https://schema.org");
         end if;
         Write_Attributes (UI, Context, UI.Stream);
      end;
   end Encode_Begin;

   overriding
   procedure Encode_End (UI      : in UIJSON;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Writer : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
         Attr   : constant String := UI.Get_Attribute ("type", Context);
      begin
         UI.Stream.End_Entity ("");
         UI.Stream.End_Document;
         UI.Stream.Flush;
         Writer.Start_Element ("script");
         if Attr'Length > 0 then
            Writer.Write_Attribute ("type", Attr);
         else
            Writer.Write_Attribute ("type", "application/ld+json");
         end if;
         Writer.Write_Raw (Util.Streams.Texts.To_String (UI.Buffer));
         Writer.End_Element ("script");
      end;
   end Encode_End;

   overriding
   procedure Finalize (UI : in out UIJSON) is
   begin
      null;
   end Finalize;

   --  ------------------------------
   --  Get the mail message instance.
   --  ------------------------------
   function Get_Stream (UI : in UIBase) return Output_Stream_Access is
      use type ASF.Components.Base.UIComponent_Access;

      Parent : ASF.Components.Base.UIComponent_Access := UI.Get_Parent;
   begin
      while Parent /= null loop
         if Parent.all in UIJSON'Class then
            return UIJSON'Class (Parent.all).Stream;
         end if;
         Parent := Parent.Get_Parent;
      end loop;

      return null;
   end Get_Stream;

   --  ------------------------------
   --  Render the input component.  Starts the DL/DD list and write the input
   --  component with the possible associated error message.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIProperty;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Stream  : constant Output_Stream_Access := UI.Get_Stream;
      begin
         if Stream = null then
            UI.Log_Error ("component has no <util:json> parent");
            return;
         end if;
         declare
            Name  : constant UBO.Object := UI.Get_Attribute (Context, "name");
            Value : constant UBO.Object := UI.Get_Attribute (Context, "value");
         begin
            if UBO.Get_Type (Value) = UBO.TYPE_TIME then
               Stream.Write_Entity
                 (UBO.To_String (Name),
                  Util.Dates.ISO8601.Image (UBO.Time.To_Time (Value),
                    Util.Dates.ISO8601.SECOND_TZ));
            elsif not UBO.Is_Null (Value) then
               Stream.Write_Entity (UBO.To_String (Name), Value);
               return;
            end if;
         end;
      end;
   end Encode_Begin;

   overriding
   procedure Encode_Children (UI      : in UIObject;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Stream  : constant Output_Stream_Access := UI.Get_Stream;
      begin
         if Stream = null then
            UI.Log_Error ("component has no <util:json> parent");
            return;
         end if;
         declare
            Name  : constant String := UI.Get_Attribute ("name", Context);
         begin
            Stream.Start_Entity (Name);
            Write_Attributes (UI, Context, Stream);

            ASF.Components.Core.UIComponentBase (UI).Encode_Children (Context);
            Stream.End_Entity (Name);
         end;
      end;
   end Encode_Children;

   overriding
   procedure Encode_Children (UI      : in UIArray;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Stream  : constant Output_Stream_Access := UI.Get_Stream;
      begin
         if Stream = null then
            UI.Log_Error ("component has no <util:json> parent");
            return;
         end if;
         declare
            Name  : constant String := UI.Get_Attribute ("name", Context);
         begin
            Stream.Start_Array (Name);
            ASF.Components.Core.UIComponentBase (UI).Encode_Children (Context);
            Stream.End_Array (Name);
         end;
      end;
   end Encode_Children;

end ASF.Components.Utils.JSON_LD;
