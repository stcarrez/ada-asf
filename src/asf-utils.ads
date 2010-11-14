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

with Util.Strings;
package ASF.Utils is

   --  Add in the <b>names</b> set, the basic text attributes that can be set
   --  on HTML elements (dir, lang, style, title).
   procedure Set_Text_Attributes (Names : in out Util.Strings.String_Set.Set);

   --  Add in the <b>names</b> set, the onXXX attributes that can be set
   --  on HTML elements (accesskey, tabindex, onXXX).
   procedure Set_Interactive_Attributes (Names : in out Util.Strings.String_Set.Set);

   --  Add in the <b>names</b> set, the size attributes that can be set
   --  on HTML elements.
   procedure Set_Input_Attributes (Names : in out Util.Strings.String_Set.Set);

end ASF.Utils;
