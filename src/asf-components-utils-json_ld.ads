-----------------------------------------------------------------------
--  asf-components-utils-json_ld -- JSON-LD generation support
--  Copyright (C) 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Components.Core;
with ASF.Contexts.Faces;
with Util.Serialize.IO.JSON;
private with Util.Streams.Buffered;
private with Util.Streams.Texts;
package ASF.Components.Utils.JSON_LD is

   subtype Output_Stream is Util.Serialize.IO.JSON.Output_Stream;
   type Output_Stream_Access is access all Output_Stream'Class;

   --  ------------------------------
   --  UIRoot
   --  ------------------------------
   --  The `UIJSON` component is the root component of a JSON-LD generation.
   --  When it is rendered, it emits a `<script type='xxx'>` element
   --  that contains the JSON representing evaluation of children.
   type UIJSON is new ASF.Components.Core.UIComponentBase with private;
   type UIJSON_Access is access all UIJSON'Class;

   overriding
   procedure Encode_Begin (UI      : in UIJSON;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   overriding
   procedure Encode_End (UI      : in UIJSON;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   overriding
   procedure Initialize (UI : in out UIJSON);

   overriding
   procedure Finalize (UI : in out UIJSON);

   --  ------------------------------
   --  UIBase
   --  ------------------------------
   --  The `UIBase` component is the base component for JSON generation
   --  it is defined to provide the `Get_Stream` function that gives access
   --  to the JSON stream being constructed.
   type UIBase is new ASF.Components.Core.UIComponentBase with null record;

   function Get_Stream (UI : in UIBase) return Output_Stream_Access;

   --  ------------------------------
   --  UIProperty
   --  ------------------------------
   --  The `UIProperty` emits a json property in the JSON stream.
   type UIProperty is new UIBase with null record;

   overriding
   procedure Encode_Begin (UI      : in UIProperty;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  ------------------------------
   --  UIObject
   --  ------------------------------
   --  The `UIObject` emits a json object that could contain other
   --  properties, objects and arrays for the generation of the JSON stream.
   type UIObject is new UIBase with null record;

   overriding
   procedure Encode_Children (UI      : in UIObject;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  ------------------------------
   --  UIArray
   --  ------------------------------
   --  The `UIArray` emits a JSON array in the JSON stream.
   type UIArray is new UIBase with null record;

   overriding
   procedure Encode_Children (UI      : in UIArray;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

private

   type UIJSON is new ASF.Components.Core.UIComponentBase with record
      Buffer : aliased Util.Streams.Buffered.Output_Buffer_Stream;
      Print  : aliased Util.Streams.Texts.Print_Stream;
      Output : aliased Util.Serialize.IO.JSON.Output_Stream;
      Stream : Output_Stream_Access;
   end record;

end ASF.Components.Utils.JSON_LD;
