-----------------------------------------------------------------------
--  html -- ASF HTML Components
--  Copyright (C) 2009, 2010, 2011, 2013 Stephane Carrez
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
with ASF.Components.Base;
package body ASF.Components.Html is

   use EL.Objects;

   procedure Render_Attributes (UI      : in UIHtmlComponent;
                                Context : in out Faces_Context'Class;
                                Writer  : in Response_Writer_Access) is
      Style  : constant Object := UI.Get_Attribute (Context, "style");
      Class  : constant Object := UI.Get_Attribute (Context, "styleClass");
      Title  : constant Object := UI.Get_Attribute (Context, "title");
   begin
      if not UI.Is_Generated_Id then
         Writer.Write_Attribute ("id", UI.Get_Client_Id);
      end if;
      if not Is_Null (Class) then
         Writer.Write_Attribute ("class", Class);
      end if;
      if not Is_Null (Style) then
         Writer.Write_Attribute ("style", Style);
      end if;
      if not Is_Null (Title) then
         Writer.Write_Attribute ("title", Title);
      end if;
   end Render_Attributes;

   --  ------------------------------
   --  Render the attributes which are defined on the component and which are
   --  in the list specified by <b>names</b>.
   --  ------------------------------
   procedure Render_Attributes (UI       : in UIHtmlComponent;
                                Context  : in out Faces_Context'Class;
                                Names    : in Util.Strings.String_Set.Set;
                                Writer   : in Response_Writer_Access;
                                Write_Id : in Boolean := True) is

      pragma Unreferenced (Context);

      procedure Process_Attribute (Name : in String;
                                   Attr : in UIAttribute);

      procedure Process_Attribute (Name : in String;
                                   Attr : in UIAttribute) is
      begin
         if Names.Contains (Name'Unrestricted_Access) then
            declare
               Value : constant Object := Base.Get_Value (Attr, UI);
            begin
               if Name = "styleClass" then
                  Writer.Write_Attribute ("class", Value);
               else
                  Writer.Write_Attribute (Name, Value);
               end if;
            end;
         end if;
      end Process_Attribute;

      procedure Write_Attributes is new Base.Iterate_Attributes (Process_Attribute);

   begin
      if Write_Id and then not UI.Is_Generated_Id then
         Writer.Write_Attribute ("id", UI.Get_Client_Id);
      end if;
      Write_Attributes (UI);
   end Render_Attributes;

end ASF.Components.Html;
