-----------------------------------------------------------------------
--  components-utils-escape -- Escape generated content produced by component children
--  Copyright (C) 2011, 2012 Stephane Carrez
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
      pragma Unreferenced (UI, Context);

      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Util.Strings.Transforms.Escape_Javascript (Content => Content,
                                                 Into    => Result);
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
