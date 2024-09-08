-----------------------------------------------------------------------
--  components-widgets-gravatars -- Gravatar Components
--  Copyright (C) 2013 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Components.Html;
with ASF.Contexts.Faces;
package ASF.Components.Widgets.Gravatars is

   --  Given an Email address, return the Gravatar link to the user image.
   --  (See http://en.gravatar.com/site/implement/hash/ and
   --  http://en.gravatar.com/site/implement/images/)
   function Get_Link (Email  : in String;
                      Secure : in Boolean := False) return String;

   --  ------------------------------
   --  UIGravatar
   --  ------------------------------
   --  The <b>UIGravatar</b> component displays a small image whose link is created
   --  from a user email address.
   type UIGravatar is new ASF.Components.Html.UIHtmlComponent with null record;

   --  Render an image with the source link created from an email address to the Gravatars service.
   overriding
   procedure Encode_Begin (UI      : in UIGravatar;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Components.Widgets.Gravatars;
