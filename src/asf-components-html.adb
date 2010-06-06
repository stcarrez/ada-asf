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
with ASF.Contexts.Writer;
with EL.Objects;
package body ASF.Components.Html is

   use EL.Objects;
   use ASF.Contexts.Writer;

   procedure Render_Attributes (UI      : in UIHtmlComponent;
                                Context : in out Faces_Context'Class;
                                Writer  : in ResponseWriter_Access) is
      Style  : constant Object := UI.Get_Attribute (Context, "style");
      Class  : constant Object := UI.Get_Attribute (Context, "styleClass");
   begin
      Writer.Write_Attribute ("id", UI.Get_Client_Id);
      if not Is_Null (Class) then
         Writer.Write_Attribute ("class", Class);
      end if;
      if not Is_Null (Style) then
         Writer.Write_Attribute ("style", Style);
      end if;
   end Render_Attributes;

end ASF.Components.Html;
