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
package ASF.Components.Html.Text is

   --  ------------------------------
   --  Output Component
   --  ------------------------------
   type UIOutput is new UIHtmlComponent with private;

   --  Get the value to write on the output.
   function Get_Value (UI    : in UIOutput) return EL.Objects.Object;

   --  Set the value to write on the output.
   procedure Set_Value (UI    : in out UIOutput;
                        Value : in EL.Objects.Object);

   procedure Write_Output (UI      : in UIOutput;
                           Context : in out Faces_Context'Class;
                           Value   : in EL.Objects.Object);

   procedure Encode_Begin (UI      : in UIOutput;
                           Context : in out Faces_Context'Class);

   --  ------------------------------
   --  Label Component
   --  ------------------------------
   type UILabel is new UIOutput with private;

   procedure Encode_Begin (UI      : in UILabel;
                           Context : in out Faces_Context'Class);

   procedure Encode_End (UI      : in UILabel;
                         Context : in out Faces_Context'Class);

   --  ------------------------------
   --  OutputFormat Component
   --  ------------------------------
   --
   type UIOutputFormat is new UIOutput with private;

   procedure Encode_Begin (UI      : in UIOutputFormat;
                           Context : in out Faces_Context'Class);

private

   type UIOutput is new UIHtmlComponent with record
      Value : EL.Objects.Object;
   end record;

   type UILabel is new UIOutput with null record;

   type UIOutputFormat is new UIOutput with null record;

end ASF.Components.Html.Text;
