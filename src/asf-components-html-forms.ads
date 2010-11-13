-----------------------------------------------------------------------
--  html.forms -- ASF HTML Form Components
--  Copyright (C) 2010 Stephane Carrez
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
with ASF.Components.Html.Text;
with EL.Objects;
package ASF.Components.Html.Forms is

   --  ------------------------------
   --  Input Component
   --  ------------------------------
   type UIInput is new Text.UIOutput with private;

   overriding
   procedure Encode_Begin (UI      : in UIInput;
                           Context : in out Faces_Context'Class);

   --  ------------------------------
   --  Button Component
   --  ------------------------------
   type UICommand is new UIHtmlComponent with private;

   overriding
   procedure Encode_Begin (UI      : in UICommand;
                           Context : in out Faces_Context'Class);

   --  Get the value to write on the output.
   function Get_Value (UI    : in UICommand) return EL.Objects.Object;

   --  Set the value to write on the output.
   procedure Set_Value (UI    : in out UICommand;
                        Value : in EL.Objects.Object);

   --  ------------------------------
   --  Label Component
   --  ------------------------------
   type UIForm is new UIHtmlComponent with private;

   --  Get the action URL to set on the HTML form
   function Get_Action (UI      : in UIForm;
                        Context : in Faces_Context'Class) return String;

   overriding
   procedure Encode_Begin (UI      : in UIForm;
                           Context : in out Faces_Context'Class);

   overriding
   procedure Encode_End (UI      : in UIForm;
                         Context : in out Faces_Context'Class);

   overriding
   procedure Decode (UI      : in out UIForm;
                     Context : in out Faces_Context'Class);

   overriding
   procedure Process_Decodes (UI      : in out UIForm;
                              Context : in out Faces_Context'Class);

private

   type UIInput is new Text.UIOutput with record
      Value : EL.Objects.Object;
   end record;

   type UICommand is new UIHtmlComponent with record
      Value : EL.Objects.Object;
   end record;

   type UIForm is new UIHtmlComponent with record
      Is_Submitted : Boolean := False;
   end record;

end ASF.Components.Html.Forms;
