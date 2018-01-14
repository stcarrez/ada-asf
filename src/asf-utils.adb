-----------------------------------------------------------------------
--  html -- ASF HTML Components
--  Copyright (C) 2009, 2010, 2011, 2012, 2015, 2018 Stephane Carrez
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
package body ASF.Utils is

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
   ON_CHANGE_ATTR     : aliased constant String := "onchange";

   ON_RESET_ATTR      : aliased constant String := "onreset";
   ON_SUBMIT_ATTR     : aliased constant String := "onsubmit";
   ENCTYPE_ATTR       : aliased constant String := "enctype";

   ON_LOAD_ATTR       : aliased constant String := "onload";
   ON_UNLOAD_ATTR     : aliased constant String := "onunload";

   TABINDEX_ATTR      : aliased constant String := "tabindex";

   AUTOCOMPLETE_ATTR  : aliased constant String := "autocomplete";
   SIZE_ATTR          : aliased constant String := "size";
   MAXLENGTH_ATTR     : aliased constant String := "maxlength";
   ALT_ATTR           : aliased constant String := "alt";
   DISABLED_ATTR      : aliased constant String := "disabled";
   DIRNAME_ATTR       : aliased constant String := "dirname";
   READONLY_ATTR      : aliased constant String := "readonly";
   PLACEHOLDER_ATTR   : aliased constant String := "placeholder";
   PATTERN_ATTR       : aliased constant String := "pattern";
   AUTOFOCUS_ATTR     : aliased constant String := "autofocus";
   LIST_ATTR          : aliased constant String := "list";
   FORMENCTYPE_ATTR   : aliased constant String := "formenctype";
   FORMMETHOD_ATTR    : aliased constant String := "formmethod";
   FORMACTION_ATTR    : aliased constant String := "formaction";
   FORMTARGET_ATTR    : aliased constant String := "formtarget";
   WRAP_ATTR          : aliased constant String := "wrap";

   ACCEPT_ATTR        : aliased constant String := "accept";

   ROWS_ATTR          : aliased constant String := "rows";
   COLS_ATTR          : aliased constant String := "cols";

   CHARSET_ATTR       : aliased constant String := "charset";
   SHAPE_ATTR         : aliased constant String := "shape";
   REV_ATTR           : aliased constant String := "rev";
   COORDS_ATTR        : aliased constant String := "coords";
   TARGET_ATTR        : aliased constant String := "target";
   HREFLANG_ATTR      : aliased constant String := "hreflang";

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
      Names.Insert (ON_CHANGE_ATTR'Access);
   end Set_Interactive_Attributes;

   --  ------------------------------
   --  Add in the <b>names</b> set, the size attributes that can be set
   --  on HTML elements.
   --  ------------------------------
   procedure Set_Input_Attributes (Names : in out Util.Strings.String_Set.Set) is
   begin
      Names.Insert (SIZE_ATTR'Access);
      Names.Insert (AUTOCOMPLETE_ATTR'Access);
      Names.Insert (MAXLENGTH_ATTR'Access);
      Names.Insert (ALT_ATTR'Access);
      Names.Insert (DISABLED_ATTR'Access);
      Names.Insert (READONLY_ATTR'Access);
      Names.Insert (PLACEHOLDER_ATTR'Access);
      Names.Insert (PATTERN_ATTR'Access);
      Names.Insert (AUTOFOCUS_ATTR'Access);
      Names.Insert (LIST_ATTR'Access);
      Names.Insert (FORMENCTYPE_ATTR'Access);
      Names.Insert (FORMMETHOD_ATTR'Access);
      Names.Insert (FORMACTION_ATTR'Access);
      Names.Insert (FORMTARGET_ATTR'Access);
   end Set_Input_Attributes;

   --  ------------------------------
   --  Add in the <b>names</b> set, the size attributes that can be set
   --  on <textarea> elements.
   --  ------------------------------
   procedure Set_Textarea_Attributes (Names : in out Util.Strings.String_Set.Set) is
   begin
      Names.Insert (AUTOFOCUS_ATTR'Access);
      Names.Insert (COLS_ATTR'Access);
      Names.Insert (DIRNAME_ATTR'Access);
      Names.Insert (DISABLED_ATTR'Access);
      Names.Insert (MAXLENGTH_ATTR'Access);
      Names.Insert (PLACEHOLDER_ATTR'Access);
      Names.Insert (READONLY_ATTR'Access);
      Names.Insert (ROWS_ATTR'Access);
      Names.Insert (WRAP_ATTR'Access);
   end Set_Textarea_Attributes;

   --  ------------------------------
   --  Add in the <b>names</b> set, the online and onunload attributes that can be set
   --  on <body> elements.
   --  ------------------------------
   procedure Set_Body_Attributes (Names : in out Util.Strings.String_Set.Set) is
   begin
      Names.Insert (ON_LOAD_ATTR'Access);
      Names.Insert (ON_UNLOAD_ATTR'Access);
   end Set_Body_Attributes;

   --  ------------------------------
   --  Add in the <b>names</b> set, the dir, lang attributes that can be set
   --  on <head> elements.
   --  ------------------------------
   procedure Set_Head_Attributes (Names : in out Util.Strings.String_Set.Set) is
   begin
      Names.Insert (DIR_ATTR'Access);
      Names.Insert (LANG_ATTR'Access);
   end Set_Head_Attributes;

   --------------------
   --  Add in the <b>names</b> set, the onreset and onsubmit attributes that can be set
   --  on <form> elements.
   --  ------------------------------
   procedure Set_Form_Attributes (Names : in out Util.Strings.String_Set.Set) is
   begin
      Names.Insert (ON_RESET_ATTR'Access);
      Names.Insert (ON_SUBMIT_ATTR'Access);
      Names.Insert (ENCTYPE_ATTR'Access);
   end Set_Form_Attributes;

   --------------------
   --  Add in the <b>names</b> set, the attributes which are specific to a link.
   --  ------------------------------
   procedure Set_Link_Attributes (Names : in out Util.Strings.String_Set.Set) is
   begin
      Names.Insert (CHARSET_ATTR'Access);
      Names.Insert (SHAPE_ATTR'Access);
      Names.Insert (REV_ATTR'Access);
      Names.Insert (COORDS_ATTR'Access);
      Names.Insert (TARGET_ATTR'Access);
      Names.Insert (HREFLANG_ATTR'Access);
   end Set_Link_Attributes;

   --  ------------------------------
   --  Add in the <b>names</b> set, the attributes which are specific to an input file.
   --  ------------------------------
   procedure Set_File_Attributes (Names : in out Util.Strings.String_Set.Set) is
   begin
      Names.Insert (ACCEPT_ATTR'Access);
   end Set_File_Attributes;

end ASF.Utils;
