-----------------------------------------------------------------------
--  components-core -- ASF Core Components
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
with Ada.Finalization;
with ASF.Views.Nodes;
package ASF.Components.Core is

   type UIComponentBase is new UIComponent with private;

   --  Return a client-side identifier for this component, generating
   --  one if necessary.
   overriding
   function Get_Client_Id (UI : UIComponentBase) return Unbounded_String;

   overriding
   function Get_Attribute (UI      : UIComponentBase;
                           Context : Faces_Context'Class;
                           Name    : String) return EL.Objects.Object;

   --  Get the attribute tag
   overriding
   function Get_Attribute (UI      : UIComponentBase;
                           Name    : String) return access ASF.Views.Nodes.Tag_Attribute;

   type UIViewRoot is new Ada.Finalization.Limited_Controlled with private;
   type UIViewRoot_Access is access all UIViewRoot'Class;

   --  Get the root node of the view.
   function Get_Root (UI : UIViewRoot) return UIComponent_Access;

   --  Set the root node of the view.
   procedure Set_Root (UI   : in out UIViewRoot;
                       Root : in out UIComponent'Class);

   --  ------------------------------
   --  View component
   --  ------------------------------
   type UIView is new UIComponentBase with private;
   type UIView_Access is access all UIView'Class;

   overriding
   procedure Encode_Begin (UI      : in UIView;
                           Context : in out Faces_Context'Class);

   type UIText is new UIComponent with private;
   type UIText_Access is access all UIText'Class;

   procedure Encode_Begin (UI      : in UIText;
                           Context : in out Faces_Context'Class);

   function Create_UIText (Tag : ASF.Views.Nodes.Text_Tag_Node_Access)
                           return UIComponent_Access;


   --  ------------------------------
   --  Abstract Leaf component
   --  ------------------------------
   --  The <b>UILeaf</b> component is an abstract component intended to be used
   --  for components without children.
   type UILeaf is new UIComponentBase with private;

   overriding
   procedure Encode_Children (UI      : in UILeaf;
                              Context : in out Faces_Context'Class);

   overriding
   procedure Encode_Begin (UI      : in UILeaf;
                           Context : in out Faces_Context'Class);

   overriding
   procedure Encode_End (UI      : in UILeaf;
                         Context : in out Faces_Context'Class);

   --  ------------------------------
   --  Component Parameter
   --  ------------------------------
   type UIParameter is new UILeaf with private;
   type UIParameter_Access is access all UIParameter'Class;

   type UIParameter_Access_Array is array (Natural range <>) of UIParameter_Access;

   --  Get the parameter name
   function Get_Name (UI      : UIParameter;
                      Context : Faces_Context'Class) return String;

   --  Get the parameter value
   function Get_Value (UI      : UIParameter;
                       Context : Faces_Context'Class) return EL.Objects.Object;

   --  Get the list of parameters associated with a component.
   function Get_Parameters (UI : UIComponent'Class) return UIParameter_Access_Array;

private

   procedure Finalize (Object : in out UIViewRoot);
   --
   type UIComponentBase is new UIComponent with null record;

   type UIViewRoot is new Ada.Finalization.Limited_Controlled with record
      Root : UIComponent_Access;
   end record;

   type UIView is new UIComponentBase with record
      N : Natural;
   end record;

   type UIText is new UIComponent with record
      Text : ASF.Views.Nodes.Text_Tag_Node_Access;
   end record;

   type UILeaf is new UIComponentBase with null record;

   type UIParameter is new UILeaf with record
      N : Natural;
   end record;

end ASF.Components.Core;
