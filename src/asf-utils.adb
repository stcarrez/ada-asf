-----------------------------------------------------------------------
--  html -- ASF HTML Components
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
package body ASF.Utils is

   use EL.Objects;

   TITLE_ATTR         : aliased constant String := "title";
   STYLE_ATTR         : aliased constant String := "style";
   STYLE_CLASS_ATTR   : aliased constant String := "styleClass";
   DIR_ATTR           : aliased constant String := "dir";
   LANG_ATTR          : aliased constant String := "lang";

   ACCESS_KEY_ATTR    : aliased constant String := "accesskey";
   ON_BLUR_ATTR       : aliased constant String := "onblur";
   ON_CLICK_ATTR      : aliased constant String := "onclick";
   ON_DBLCLICK_ATTR   : aliased constant String := "ondblclick";
   ON_FOCUS_ATTR      : aliased constant String := "onfocus";
   ON_KEYDOWN_ATTR    : aliased constant String := "onkeydown";
   ON_KEYUP_ATTR      : aliased constant String := "onkeyup";
   ON_MOUSE_DOWN_ATTR : aliased constant String := "onmousedown";
   ON_MOUSE_MOVE_ATTR : aliased constant String := "onmousemove";
   ON_MOUSE_OUT_ATTR  : aliased constant String := "onmouseout";
   ON_MOUSE_OVER_ATTR : aliased constant String := "onmouseover";
   ON_MOUSE_UP_ATTR   : aliased constant String := "onmouseup";

   TABINDEX_ATTR      : aliased constant String := "tabindex";

   SIZE_ATTR          : aliased constant String := "size";

   --  ------------------------------
   --  Add in the <b>names</b> set, the basic text attributes that can be set
   --  on HTML elements (dir, lang, style, title).
   --  ------------------------------
   procedure Set_Text_Attributes (Names : in out Util.Strings.String_Set.Set) is
   begin
      Names.Insert (STYLE_CLASS_ATTR'Access);
      Names.Insert (TITLE_ATTR'Access);
      Names.Insert (DIR_ATTR'Access);
      Names.Insert (LANG_ATTR'Access);
      Names.Insert (STYLE_ATTR'Access);
   end Set_Text_Attributes;

   --  ------------------------------
   --  Add in the <b>names</b> set, the onXXX attributes that can be set
   --  on HTML elements (accesskey, tabindex, onXXX).
   --  ------------------------------
   procedure Set_Interactive_Attributes (Names : in out Util.Strings.String_Set.Set) is
   begin
      Names.Insert (ACCESS_KEY_ATTR'Access);
      Names.Insert (TABINDEX_ATTR'Access);
      Names.Insert (ON_BLUR_ATTR'Access);
      Names.Insert (ON_MOUSE_UP_ATTR'Access);
      Names.Insert (ON_MOUSE_OVER_ATTR'Access);
      Names.Insert (ON_MOUSE_OUT_ATTR'Access);
      Names.Insert (ON_MOUSE_MOVE_ATTR'Access);
      Names.Insert (ON_MOUSE_DOWN_ATTR'Access);
      Names.Insert (ON_KEYUP_ATTR'Access);
      Names.Insert (ON_KEYDOWN_ATTR'Access);
      Names.Insert (ON_FOCUS_ATTR'Access);
      Names.Insert (ON_DBLCLICK_ATTR'Access);
      Names.Insert (ON_CLICK_ATTR'Access);
   end Set_Interactive_Attributes;

   --  ------------------------------
   --  Add in the <b>names</b> set, the size attributes that can be set
   --  on HTML elements.
   --  ------------------------------
   procedure Set_Input_Attributes (Names : in out Util.Strings.String_Set.Set) is
   begin
      Names.Insert (SIZE_ATTR'Access);
   end Set_Input_Attributes;

end ASF.Utils;
