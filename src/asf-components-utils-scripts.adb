-----------------------------------------------------------------------
--  components-utils-scripts -- Javascript queue
--  Copyright (C) 2011, 2012, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body ASF.Components.Utils.Scripts is

   --  ------------------------------
   --  Write the content that was collected by rendering the inner children.
   --  Write the content in the Javascript queue.
   --  ------------------------------
   overriding
   procedure Write_Content (UI      : in UIScript;
                            Writer  : in out Contexts.Writer.Response_Writer'Class;
                            Content : in String;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (UI, Context);
   begin
      Writer.Queue_Script (Content);
   end Write_Content;

   --  ------------------------------
   --  If the component provides a src attribute, render the <script src='xxx'></script>
   --  tag with an optional async attribute.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIScript;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;
      declare
         Src    : constant String := UI.Get_Attribute (Context => Context, Name => "src");
         Writer : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
      begin
         if Src'Length > 0 then
            Writer.Queue_Include_Script (URL   => Src,
                                         Async => UI.Get_Attribute (Context => Context,
                                                                    Name    => "async"));
         end if;
      end;
   end Encode_Begin;

end ASF.Components.Utils.Scripts;
