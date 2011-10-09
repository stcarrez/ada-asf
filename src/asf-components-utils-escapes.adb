-----------------------------------------------------------------------
--  components-utils-escape -- Escape generated content produced by component children
--  Copyright (C) 2011 Stephane Carrez
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
with ASF.Contexts.Writer.String;
package body ASF.Components.Utils.Escapes is

   --  ------------------------------
   --  Write the content that was collected by rendering the inner children.
   --  Escape the content using Javascript escape rules.
   --  ------------------------------
   procedure Write_Content (UI      : in UIEscape;
                            Writer  : in out Contexts.Writer.Response_Writer'Class;
                            Content : in Ada.Strings.Unbounded.Unbounded_String;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (UI, Context);

      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Util.Strings.Transforms.Escape_Javascript (Content => To_String (Content),
                                                 Into    => Result);
      Writer.Write (Result);
   end Write_Content;

   --  ------------------------------
   --  Encode the children components in the javascript queue.
   --  ------------------------------
   overriding
   procedure Encode_Children (UI      : in UIEscape;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class) is

   begin
      if not UI.Is_Rendered (Context) then
         return;
      end if;

      --  Replace temporarily the response writer by a local buffer.  The buffer content is then
      --  appended in the response javascript queue.
      --  Make sure that if an exception is raised, the original response writer is restored.
      declare
         Writer : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
         Buffer : aliased ASF.Contexts.Writer.String.String_Writer;
      begin
         Context.Set_Response_Writer (Buffer'Unchecked_Access);
         ASF.Components.Core.UIComponentBase (UI).Encode_Children (Context);
         Context.Set_Response_Writer (Writer);

         UIEscape'Class (UI).Write_Content (Writer.all, Buffer.Get_Response, Context);
      exception
         when others =>
            Context.Set_Response_Writer (Writer);
            raise;
      end;
   end Encode_Children;

end ASF.Components.Utils.Escapes;
