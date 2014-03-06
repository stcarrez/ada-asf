-----------------------------------------------------------------------
--  components-utils-escape -- Escape generated content produced by component children
--  Copyright (C) 2011, 2012, 2014 Stephane Carrez
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
with ASF.Components.Core;
with ASF.Contexts.Faces;
with ASF.Contexts.Writer;

package ASF.Components.Utils.Escapes is

   --  Name of the attribute that control the escape mode.
   --  When the attribute is set to 'xml', the content is escaped using XML
   --  escape rules.  Otherwise, the content is escaped using Javascript rules.
   ESCAPE_MODE_NAME : constant String := "mode";

   --  ------------------------------
   --  UIEscape
   --  ------------------------------
   --  The <b>UIEscape</b> component catches the rendering of child components to
   --  perform specific escape actions on the content.
   type UIEscape is new ASF.Components.Core.UIComponentBase with private;

   --  Write the content that was collected by rendering the inner children.
   --  Escape the content using Javascript escape rules.
   procedure Write_Content (UI      : in UIEscape;
                            Writer  : in out Contexts.Writer.Response_Writer'Class;
                            Content : in String;
                            Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Encode the children components in a local buffer.
   overriding
   procedure Encode_Children (UI      : in UIEscape;
                              Context : in out ASF.Contexts.Faces.Faces_Context'Class);

private

   type UIEscape is new ASF.Components.Core.UIComponentBase with null record;

end ASF.Components.Utils.Escapes;

