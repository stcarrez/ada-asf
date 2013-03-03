-----------------------------------------------------------------------
--  asf-utils -- Various utility operations for ASF
--  Copyright (C) 2009, 2010, 2011, 2012, 2013 Stephane Carrez
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

with Ada.Strings.Unbounded;

with Util.Texts.Formats;
with Util.Beans.Objects;

package ASF.Utils is

   pragma Preelaborate;

   --  Add in the <b>names</b> set, the basic text attributes that can be set
   --  on HTML elements (dir, lang, style, title).
   procedure Set_Text_Attributes (Names : in out Util.Strings.String_Set.Set);

   --  Add in the <b>names</b> set, the onXXX attributes that can be set
   --  on HTML elements (accesskey, tabindex, onXXX).
   procedure Set_Interactive_Attributes (Names : in out Util.Strings.String_Set.Set);

   --  Add in the <b>names</b> set, the size attributes that can be set
   --  on HTML elements.
   procedure Set_Input_Attributes (Names : in out Util.Strings.String_Set.Set);

   --  Add in the <b>names</b> set, the size attributes that can be set
   --  on <textarea> elements.
   procedure Set_Textarea_Attributes (Names : in out Util.Strings.String_Set.Set);

   --  Add in the <b>names</b> set, the online and onunload attributes that can be set
   --  on <body> elements.
   procedure Set_Body_Attributes (Names : in out Util.Strings.String_Set.Set);

   --  Add in the <b>names</b> set, the dir, lang attributes that can be set
   --  on <head> elements.
   procedure Set_Head_Attributes (Names : in out Util.Strings.String_Set.Set);

   --  Add in the <b>names</b> set, the onreset and onsubmit attributes that can be set
   --  on <form> elements.
   procedure Set_Form_Attributes (Names : in out Util.Strings.String_Set.Set);

   --  Add in the <b>names</b> set, the attributes which are specific to a link.
   procedure Set_Link_Attributes (Names : in out Util.Strings.String_Set.Set);

   --  Add in the <b>names</b> set, the attributes which are specific to an input file.
   procedure Set_File_Attributes (Names : in out Util.Strings.String_Set.Set);

   type Object_Array is array (Positive range <>) of Util.Beans.Objects.Object;

   package Formats is
     new Util.Texts.Formats (Stream     => Ada.Strings.Unbounded.Unbounded_String,
                             Char       => Character,
                             Input      => String,
                             Value      => Util.Beans.Objects.Object,
                             Value_List => Object_Array,
                             Put        => Ada.Strings.Unbounded.Append,
                             To_Input   => Util.Beans.Objects.To_String);

end ASF.Utils;
