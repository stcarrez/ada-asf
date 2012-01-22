-----------------------------------------------------------------------
--  components-utils-files -- Include raw files in the output
--  Copyright (C) 2012 Stephane Carrez
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
with Util.Streams;

with ASF.Components.Core;
with ASF.Contexts.Faces;
package ASF.Components.Utils.Files is

   --  ------------------------------
   --  UIFile
   --  ------------------------------
   --  The <b>UIFile</b> component allows to include an external file in a view.
   --  The file is identified by the <b>src</b> attribute.  It is searched in the
   --  application search path.
   type UIFile is new ASF.Components.Core.UILeaf with null record;

   --  Get the resource path that must be included.
   --  The resource path is identified by the <b>src</b> attribute.
   function Get_Resource (UI      : in UIFile;
                          Context : in ASF.Contexts.Faces.Faces_Context'Class) return String;

   --  Copy the stream represented by <b>From</b> in the output stream associated with
   --  the context <b>Context</b>.  When <b>Escape</b> is True, escape any special character
   --  using HTML escape rules.
   procedure Copy (UI      : in UIFile;
                   From    : in out Util.Streams.Input_Stream'Class;
                   Escape  : in Boolean;
                   Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Include in the output stream the resource identified by the <b>Get_Resource</b>
   --  function.
   overriding
   procedure Encode_Begin (UI      : in UIFile;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Components.Utils.Files;
