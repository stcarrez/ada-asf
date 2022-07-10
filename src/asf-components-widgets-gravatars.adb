-----------------------------------------------------------------------
--  components-widgets-gravatars -- Gravatar Components
--  Copyright (C) 2013, 2022 Stephane Carrez
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

with Util.Strings.Transforms;
with GNAT.MD5;

with ASF.Utils;
with ASF.Contexts.Writer;
package body ASF.Components.Widgets.Gravatars is

   IMAGE_ATTRIBUTE_NAMES : Util.Strings.String_Set.Set;

   --  ------------------------------
   --  Given an Email address, return the Gravatar link to the user image.
   --  (See http://en.gravatar.com/site/implement/hash/ and
   --  http://en.gravatar.com/site/implement/images/)
   --  ------------------------------
   function Get_Link (Email  : in String;
                      Secure : in Boolean := False) return String is
      E : constant String := Util.Strings.Transforms.To_Lower_Case (Email);
      C : constant GNAT.MD5.Message_Digest := GNAT.MD5.Digest (E);
   begin
      if Secure then
         return "https://secure.gravatar.com/avatar/" & C;
      else
         return "http://www.gravatar.com/avatar/" & C;
      end if;
   end Get_Link;

   --  ------------------------------
   --  Render an image with the source link created from an email address to the Gravatars service.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIGravatar;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Writer   : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if UI.Is_Rendered (Context) then
         Writer.Start_Element ("img");
         UI.Render_Attributes (Context, IMAGE_ATTRIBUTE_NAMES, Writer);
         declare
            Email   : constant String := UI.Get_Attribute ("email", Context, "");
            Size    : Integer := UI.Get_Attribute ("size", Context, 80);
            Default : constant String := UI.Get_Attribute ("default", Context, "identicon");
            Secure  : constant Boolean := UI.Get_Attribute ("secure", Context, False);
            D       : constant String := Util.Strings.Image (Size);
            Alt     : constant String := UI.Get_Attribute ("alt", Context, "");
         begin
            if Size <= 0 or else Size > 2048 then
               Size := 80;
            end if;
            Writer.Write_Attribute ("width", D);
            Writer.Write_Attribute ("height", D);
            if Alt'Length > 0 then
               Writer.Write_Attribute ("alt", Alt);
            else
               Writer.Write_Attribute ("alt", Email);
            end if;
            Writer.Write_Attribute ("src", Get_Link (Email, Secure)
                                    & "?d=" & Default & "&s=" & D);
         end;
         Writer.End_Element ("img");
      end if;
   end Encode_Begin;

begin
   Utils.Set_Text_Attributes (IMAGE_ATTRIBUTE_NAMES);
end ASF.Components.Widgets.Gravatars;
