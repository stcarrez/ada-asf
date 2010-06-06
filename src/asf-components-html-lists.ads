-----------------------------------------------------------------------
--  html.lists -- List of items
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
package ASF.Components.Html.Lists is

   type UIList is new UIHtmlComponent with private;

   --  Get the value to write on the output.
   function Get_Value (UI    : in UIList) return EL.Objects.Object;

   --  Set the value to write on the output.
   procedure Set_Value (UI    : in out UIList;
                        Value : in EL.Objects.Object);

   --  Get the variable name
   function Get_Var (UI : in UIList) return String;

   procedure Encode_Children (UI      : in UIList;
                              Context : in out Faces_Context'Class);

private

   type UIList is new UIHtmlComponent with record
      Value : EL.Objects.Object;
   end record;

end ASF.Components.Html.Lists;
