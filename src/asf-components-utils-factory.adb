-----------------------------------------------------------------------
--  core-factory -- Factory for Core UI Components
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014 Stephane Carrez
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
with Ada.Calendar;
with Ada.Strings.Maps;
with ASF.Views.Nodes;
with ASF.Components.Utils.Files;
with ASF.Components.Utils.Flush;
with ASF.Components.Utils.Scripts;
with ASF.Components.Utils.Escapes;
with ASF.Components.Utils.Beans;
with ASF.Components.Html.Messages;
with Util.Dates.ISO8601;
with Util.Beans.Objects.Time;
with Util.Strings.Transforms; use Util.Strings;

package body ASF.Components.Utils.Factory is

   use ASF.Components.Base;

   function Create_File return UIComponent_Access;
   function Create_Flush return UIComponent_Access;
   function Create_Script return UIComponent_Access;
   function Create_Escape return UIComponent_Access;
   function Create_Set return UIComponent_Access;

   --  -------------------------
   --  ------------------------------
   --  Create a UIFile component
   --  ------------------------------
   function Create_File return UIComponent_Access is
   begin
      return new ASF.Components.Utils.Files.UIFile;
   end Create_File;

   --  ------------------------------
   --  Create a UIFlush component
   --  ------------------------------
   function Create_Flush return UIComponent_Access is
   begin
      return new ASF.Components.Utils.Flush.UIFlush;
   end Create_Flush;

   --  ------------------------------
   --  Create a UIScript component
   --  ------------------------------
   function Create_Script return UIComponent_Access is
   begin
      return new ASF.Components.Utils.Scripts.UIScript;
   end Create_Script;

   --  ------------------------------
   --  Create a UIEscape component
   --  ------------------------------
   function Create_Escape return UIComponent_Access is
   begin
      return new ASF.Components.Utils.Escapes.UIEscape;
   end Create_Escape;

   --  ------------------------------
   --  Create a UISetBean component
   --  ------------------------------
   function Create_Set return UIComponent_Access is
   begin
      return new ASF.Components.Utils.Beans.UISetBean;
   end Create_Set;

   use ASF.Views.Nodes;

   URI        : aliased constant String := "http://code.google.com/p/ada-asf/util";
   ESCAPE_TAG : aliased constant String := "escape";
   FILE_TAG   : aliased constant String := "file";
   FLUSH_TAG  : aliased constant String := "flush";
   SCRIPT_TAG : aliased constant String := "script";
   SET_TAG    : aliased constant String := "set";

   Core_Bindings : aliased constant ASF.Factory.Binding_Array
     := (1 => (Name      => ESCAPE_TAG'Access,
               Component => Create_Escape'Access,
               Tag       => Create_Component_Node'Access),
         2 => (Name      => FILE_TAG'Access,
               Component => Create_File'Access,
               Tag       => Create_Component_Node'Access),
         3 => (Name      => FLUSH_TAG'Access,
               Component => Create_Flush'Access,
               Tag       => Create_Component_Node'Access),
         4 => (Name      => SCRIPT_TAG'Access,
               Component => Create_Script'Access,
               Tag       => Create_Component_Node'Access),
         5 => (Name      => SET_TAG'Access,
               Component => Create_Set'Access,
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

   --  Truncate the string representation represented by <b>Value</b> to
   --  the length specified by <b>Size</b>.
   function Escape_Javascript (Value : EL.Objects.Object) return EL.Objects.Object;

   --  Escape the string using XML escape rules.
   function Escape_Xml (Value : EL.Objects.Object) return EL.Objects.Object;

   --  Translate the value into an ISO8606 date.
   function To_ISO8601 (Value : in EL.Objects.Object) return EL.Objects.Object;

   --  Encode the string for URL.
   function Url_Encode (Value : in EL.Objects.Object) return EL.Objects.Object;

   procedure Set_Functions (Mapper : in out EL.Functions.Function_Mapper'Class) is
   begin
      Mapper.Set_Function (Name      => "escapeJavaScript",
                           Namespace => URI,
                           Func      => Escape_Javascript'Access);
      Mapper.Set_Function (Name      => "escapeXml",
                           Namespace => URI,
                           Func      => Escape_Xml'Access);
      Mapper.Set_Function (Name      => "iso8601",
                           Namespace => URI,
                           Func      => To_ISO8601'Access);
      Mapper.Set_Function (Name      => "hasMessage",
                           Namespace => URI,
                           Func      => ASF.Components.Html.Messages.Has_Message'Access,
                           Optimize  => False);
      Mapper.Set_Function (Name      => "urlEncode",
                           Namespace => URI,
                           Func      => Url_Encode'Access);
   end Set_Functions;

   function Escape_Javascript (Value : EL.Objects.Object) return EL.Objects.Object is
      Result  : Ada.Strings.Unbounded.Unbounded_String;
      Content : constant String := EL.Objects.To_String (Value);
   begin
      Transforms.Escape_Javascript (Content => Content,
                                    Into    => Result);
      return EL.Objects.To_Object (Result);
   end Escape_Javascript;

   function Escape_Xml (Value : EL.Objects.Object) return EL.Objects.Object is
      Result  : Ada.Strings.Unbounded.Unbounded_String;
      Content : constant String := EL.Objects.To_String (Value);
   begin
      Transforms.Escape_Xml (Content => Content,
                             Into    => Result);
      return EL.Objects.To_Object (Result);
   end Escape_Xml;

   --  ------------------------------
   --  Translate the value into an ISO8606 date.
   --  ------------------------------
   function To_ISO8601 (Value : in EL.Objects.Object) return EL.Objects.Object is
      D : constant Ada.Calendar.Time := Util.Beans.Objects.Time.To_Time (Value);
      S : constant String := Util.Dates.ISO8601.Image (D);
   begin
      return Util.Beans.Objects.To_Object (S);
   end To_ISO8601;

   use Ada.Strings.Maps;

   Conversion     : constant String (1 .. 16) := "0123456789ABCDEF";
   Url_Encode_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (Span => (Low  => Character'Val (0),
                                          High => ' '))
     or
       Ada.Strings.Maps.To_Set (Span => (Low => Character'Val (128),
                                         High => Character'Val (255)))
     or
       Ada.Strings.Maps.To_Set (":/?#[]@!$&'""()*+,;=");

   --  ------------------------------
   --  Encode the string for URL.
   --  ------------------------------
   function Url_Encode (Value : in EL.Objects.Object) return EL.Objects.Object is
      S   : constant String := Util.Beans.Objects.To_String (Value);
      T   : String (1 .. S'Length * 3);
      Pos : Positive := 1;
      C   : Character;
   begin
      for I in S'Range loop
         C := S (I);
         if Ada.Strings.Maps.Is_In (C, Url_Encode_Set) then
            T (Pos) := '%';
            T (Pos + 1) := Conversion (1 + Character'Pos (C) / 16);
            T (Pos + 2) := Conversion (1 + Character'Pos (C) mod 16);
            Pos := Pos + 3;
         else
            T (Pos) := C;
            Pos := Pos + 1;
         end if;
      end loop;
      return Util.Beans.Objects.To_Object (T (1 .. Pos - 1));
   end Url_Encode;

end ASF.Components.Utils.Factory;
