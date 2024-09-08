-----------------------------------------------------------------------
--  components-widgets-selects -- Select component with jQuery Chosen
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Contexts.Writer;
with ASF.Components.Base;
package body ASF.Components.Widgets.Selects is

   --  ------------------------------
   --  Render the chosen configuration script.
   --  ------------------------------
   overriding
   procedure Encode_End (UI      : in UIChosen;
                         Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      use type ASF.Components.Base.UIComponent_Access;

      procedure Process (Content : in String);

      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
      Facets  : ASF.Components.Base.UIComponent_Access;

      procedure Process (Content : in String) is
      begin
         Writer.Queue_Script (Content);
      end Process;

   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      Writer.Queue_Script ("$('#");
      Writer.Queue_Script (UI.Get_Client_Id);
      Writer.Queue_Script ("').chosen({");

      --  If there is an options facet, render it now.
      Facets := UI.Get_Facet (OPTIONS_FACET_NAME);
      if Facets /= null then
         Facets.Wrap_Encode_Children (Context, Process'Access);
      end if;
      Writer.Queue_Script ("})");

      Facets := UI.Get_Facet (EVENTS_FACET_NAME);
      if Facets /= null then
         Facets.Wrap_Encode_Children (Context, Process'Access);
      end if;

      Writer.Queue_Script (";");
   end Encode_End;

end ASF.Components.Widgets.Selects;
