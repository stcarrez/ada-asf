-----------------------------------------------------------------------
--  html-links -- ASF HTML Links Components
--  Copyright (C) 2009, 2010, 2012, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package ASF.Components.Html.Links is

   --  ------------------------------
   --  Output Link Component
   --  ------------------------------
   type UIOutputLink is new UIHtmlComponent with private;

   --  Get the link to be rendered in the <b>href</b> attribute.
   function Get_Link (UI      : in UIOutputLink;
                      Context : in Faces_Context'Class) return String;

   --  Get the value to write on the output.
   function Get_Value (UI    : in UIOutputLink) return EL.Objects.Object;

   --  Set the value to write on the output.
   procedure Set_Value (UI    : in out UIOutputLink;
                        Value : in EL.Objects.Object);

   --  Encode the beginning of the link.
   overriding
   procedure Encode_Begin (UI      : in UIOutputLink;
                           Context : in out Faces_Context'Class);

   --  Encode the end of the link.
   overriding
   procedure Encode_End (UI      : in UIOutputLink;
                         Context : in out Faces_Context'Class);

private

   type UIOutputLink is new UIHtmlComponent with record
      Value : EL.Objects.Object;
   end record;

end ASF.Components.Html.Links;
