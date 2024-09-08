-----------------------------------------------------------------------
--  components-utils-escape -- Escape generated content produced by component children
--  Copyright (C) 2011, 2012, 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Strings.Transforms;
package body ASF.Components.Utils.Escapes is

   --  ------------------------------
   --  Write the content that was collected by rendering the inner children.
   --  Escape the content using Javascript escape rules.
   --  ------------------------------
   procedure Write_Content (UI      : in UIEscape;
                            Writer  : in out Contexts.Writer.Response_Writer'Class;
                            Content : in String;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Result : Ada.Strings.Unbounded.Unbounded_String;
      Mode   : constant String := UI.Get_Attribute (ESCAPE_MODE_NAME, Context);
   begin
      if Mode = "xml" then
         Util.Strings.Transforms.Escape_Xml (Content => Content,
                                             Into    => Result);
      else
         Util.Strings.Transforms.Escape_Java (Content => Content,
                                              Into    => Result);
      end if;
      Writer.Write (Result);
   end Write_Content;

   --  ------------------------------
   --  Encode the children components in the javascript queue.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UIEscape;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is

      procedure Process (Content : in String);

      procedure Process (Content : in String) is
         Writer : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
      begin
         UIEscape'Class (UI).Write_Content (Writer.all, Content, Context);
      end Process;

   begin
      UI.Wrap_Encode_Children (Context, Process'Access);
   end Encode_Children;

end ASF.Components.Utils.Escapes;
