-----------------------------------------------------------------------
--  components-holders -- Value holder interfaces
--  Copyright (C) 2009, 2010, 2011, 2013, 2017 Stephane Carrez
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

--  The <bASF.Components.Holders</b> defines the value holder interfaces
--  which allow to plug a converter and one or several validators.
--  These interfaces must be implemented by UIOutput component and UIInput
--  component to participate in the JSF/ASF lifecycle (validation phase).
with EL.Objects;
with ASF.Converters;
with ASF.Validators;
package ASF.Components.Holders is

   --  ------------------------------
   --  Value Holder
   --  ------------------------------
   type Value_Holder is limited interface;

   --  Get the local value of the component without evaluating
   --  the associated Value_Expression.
   function Get_Local_Value (Holder : in Value_Holder) return EL.Objects.Object is abstract;

   --  Get the value of the component.  If the component has a local
   --  value which is not null, returns it.  Otherwise, if we have a Value_Expression
   --  evaluate and returns the value.
   function Get_Value (Holder : in Value_Holder) return EL.Objects.Object is abstract;

   --  Set the value of the component.
   procedure Set_Value (Holder : in out Value_Holder;
                        Value  : in EL.Objects.Object) is abstract;

   --  Get the converter that is registered on the component.
   function Get_Converter (Holder : in Value_Holder)
                           return ASF.Converters.Converter_Access is abstract;

   --  Set the converter to be used on the component.
   procedure Set_Converter (Holder    : in out Value_Holder;
                            Converter : in ASF.Converters.Converter_Access;
                            Release   : in Boolean := False) is abstract;

   --  ------------------------------
   --  Editable Value Holder
   --  ------------------------------
   --  The <b>Editable_Value_Holder</b> extends the <b>Value_Holder</b> to provide
   --  operations used when a value can be modified.
   type Editable_Value_Holder is limited interface and Value_Holder;

   --  Add the validator to be used on the component.  The ASF implementation limits
   --  to 5 the number of validators that can be set on a component (See UIInput).
   --  The validator instance will be freed when the editable value holder is deleted
   --  unless <b>Shared</b> is true.
   procedure Add_Validator (Holder    : in out Editable_Value_Holder;
                            Validator : in ASF.Validators.Validator_Access;
                            Shared    : in Boolean := False) is abstract;

   --  ------------------------------
   --  List Holder
   --  ------------------------------
   --  The <b>List_Holder</b> is an interface used by the data scroller to get some information
   --  about how a list is displayed.
   type List_Holder is limited interface;
   type List_Holder_Access is access all List_Holder'Class;

   --  Get the number of rows in the list.
   function Get_Row_Count (List : in List_Holder) return Natural is abstract;

   --  Get the number of rows per page.
   function Get_Row_Per_Page (List : in List_Holder) return Positive is abstract;

   --  Get the current page.
   function Get_Current_Page (List : in List_Holder) return Positive is abstract;

   --  Set the current page number.
   procedure Set_Current_Page (List : in out List_Holder;
                               Page : in Positive) is abstract;

end ASF.Components.Holders;
