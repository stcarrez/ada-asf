-----------------------------------------------------------------------
--  html -- ASF HTML Components
--  Copyright (C) 2009, 2010, 2011, 2017, 2018, 2022 Stephane Carrez
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

with ASF.Components.Holders;
with ASF.Converters;

--  The <b>Text</b> package implements various components used to print text outputs.
--
--  See JSR 314 - JavaServer Faces Specification 4.1.10 UIOutput
--
--  The <b>UILabel</b> and <b>UIOutputFormat</b> components is ASF implementation of
--  the JavaServer Faces outputLabel and outputFormat components
--  (implemented with UIOutput in Java).
package ASF.Components.Html.Text is

   --  ------------------------------
   --  Output Component
   --  ------------------------------
   type UIOutput is new UIHtmlComponent and Holders.Value_Holder with private;

   --  Get the local value of the component without evaluating
   --  the associated Value_Expression.
   overriding
   function Get_Local_Value (UI : in UIOutput) return EL.Objects.Object;

   --  Get the value of the component.  If the component has a local
   --  value which is not null, returns it.  Otherwise, if we have a Value_Expression
   --  evaluate and returns the value.
   overriding
   function Get_Value (UI    : in UIOutput) return EL.Objects.Object;

   --  Set the value to write on the output.
   overriding
   procedure Set_Value (UI    : in out UIOutput;
                        Value : in EL.Objects.Object);

   --  Get the converter that is registered on the component.
   overriding
   function Get_Converter (UI : in UIOutput)
                           return ASF.Converters.Converter_Access;

   --  Set the converter to be used on the component.
   overriding
   procedure Set_Converter (UI        : in out UIOutput;
                            Converter : in ASF.Converters.Converter_Access;
                            Release   : in Boolean := False);

   --  Get the converter associated with the component
   function Get_Converter (UI      : in UIOutput;
                           Context : in Faces_Context'Class)
                           return access ASF.Converters.Converter'Class;

   overriding
   procedure Finalize (UI : in out UIOutput);

   --  Get the value of the component and apply the To_String converter on it if there is one.
   function Get_Formatted_Value (UI      : in UIOutput;
                                 Context : in Contexts.Faces.Faces_Context'Class) return String;

   --  Format the value by applying the To_String converter on it if there is one.
   function Get_Formatted_Value (UI      : in UIOutput;
                                 Value   : in Util.Beans.Objects.Object;
                                 Context : in Contexts.Faces.Faces_Context'Class) return String;

   procedure Write_Output (UI      : in UIOutput;
                           Context : in out Contexts.Faces.Faces_Context'Class;
                           Value   : in String);

   overriding
   procedure Encode_Begin (UI      : in UIOutput;
                           Context : in out Contexts.Faces.Faces_Context'Class);

   --  ------------------------------
   --  Label Component
   --  ------------------------------
   type UIOutputLabel is new UIOutput with private;

   overriding
   procedure Encode_Begin (UI      : in UIOutputLabel;
                           Context : in out Contexts.Faces.Faces_Context'Class);

   overriding
   procedure Encode_End (UI      : in UIOutputLabel;
                         Context : in out Contexts.Faces.Faces_Context'Class);

   --  ------------------------------
   --  OutputFormat Component
   --  ------------------------------
   --
   type UIOutputFormat is new UIOutput with private;

   overriding
   procedure Encode_Begin (UI      : in UIOutputFormat;
                           Context : in out Contexts.Faces.Faces_Context'Class);

private

   type UIOutput is new UIHtmlComponent and Holders.Value_Holder with record
      Value             : EL.Objects.Object;
      Converter         : ASF.Converters.Converter_Access := null;
      Release_Converter : Boolean := False;
   end record;

   type UIOutputLabel is new UIOutput with null record;

   type UIOutputFormat is new UIOutput with null record;

end ASF.Components.Html.Text;
