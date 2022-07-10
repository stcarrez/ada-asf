-----------------------------------------------------------------------
--  components-widgets-progress -- Simple progress bar
--  Copyright (C) 2021, 2022 Stephane Carrez
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

with Util.Beans.Objects;
with ASF.Components.Base;
with ASF.Contexts.Writer;
with Ada.Text_IO.Editing;
package body ASF.Components.Widgets.Progress is

   use Util.Beans.Objects;

   package Formatter is
      new Ada.Text_IO.Editing.Decimal_Output (Num => Progress_Type);

   Format : constant Ada.Text_IO.Editing.Picture := Ada.Text_IO.Editing.To_Picture ("ZZ9.9");

   function Get_Progress (UI : in UIProgressBar;
                          Context : in ASF.Contexts.Faces.Faces_Context'Class)
                          return Progress_Type is
      Value_Obj : constant Object := UI.Get_Attribute (Context, VALUE_ATTR_NAME);
      Min_Obj   : constant Object := UI.Get_Attribute (Context, MIN_VALUE_ATTR_NAME);
      Max_Obj   : constant Object := UI.Get_Attribute (Context, MAX_VALUE_ATTR_NAME);
      Value     : Long_Long_Float := To_Long_Long_Float (Value_Obj);
      Min_Val   : Long_Long_Float := 0.0;
      Max_Val   : Long_Long_Float := 0.0;
      Div       : Long_Long_Float;
   begin
      if not Is_Null (Min_Obj) then
         Min_Val := To_Long_Long_Float (Min_Obj);
      end if;
      if not Is_Null (Max_Obj) then
         Max_Val := To_Long_Long_Float (Max_Obj);
      end if;
      if Max_Val < Min_Val then
         Base.Log_Error (UI, "progress min value ({0}) is < max value ({1})",
                         To_String (Min_Obj),
                         To_String (Max_Obj));
      end if;
      Div := Max_Val - Min_Val;
      if Div <= 0.0 then
         return 0.0;
      end if;
      Value := Value - Min_Val;
      if Value >= Div then
         return 100.0;
      else
         return Progress_Type (100.0 * Value / Div);
      end if;
   end Get_Progress;

   --  ------------------------------
   --  Render the tab start.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIProgressBar;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if UI.Is_Rendered (Context) then
         Writer.Start_Element ("div");
         declare
            Style     : constant Object := UI.Get_Attribute (Context, "style");
            Class     : constant Object := UI.Get_Attribute (Context, "styleClass");
            Title     : constant Object := UI.Get_Attribute (Context, "title");
            Direction : constant Object := UI.Get_Attribute (Context, DIRECTION_ATTR_NAME);
            Progress  : constant Progress_Type := UI.Get_Progress (Context);
            Image     : constant String := Formatter.Image (Progress, Format);
            Pos       : Positive := Image'First;
            Vertical  : constant Boolean := To_String (Direction) = "vertical";
         begin
            while Pos < Image'Last and then Image (Pos) = ' ' loop
               Pos := Pos + 1;
            end loop;
            if not UI.Is_Generated_Id then
               Writer.Write_Attribute ("id", UI.Get_Client_Id);
            end if;
            if not Is_Null (Class) then
               Writer.Write_Attribute ("class", Class);
            else
               Writer.Write_Attribute ("class", "asf-progress-bar");
            end if;
            if not Is_Null (Style) then
               Writer.Write_Attribute ("style", Style);
            end if;
            if not Is_Null (Title) then
               Writer.Write_Attribute ("title", Title);
            end if;
            Writer.Start_Element ("span");
            if Vertical then
               Writer.Write_Attribute ("class", "asf-progress-status-vertical");
               Writer.Write_Attribute ("style",
                                       "height:" & Image (Pos .. Image'Last) & "%");
            else
               Writer.Write_Attribute ("class", "asf-progress-status-horizontal");
               Writer.Write_Attribute ("style",
                                       "width:" & Image (Pos .. Image'Last) & "%");
            end if;
         end;
      end if;
   end Encode_Begin;

   --  ------------------------------
   --  Render the tab close.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIProgressBar;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if UI.Is_Rendered (Context) then
         Writer.End_Element ("span");
         Writer.End_Element ("div");
      end if;
   end Encode_End;

end ASF.Components.Widgets.Progress;
