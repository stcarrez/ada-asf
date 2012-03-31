-----------------------------------------------------------------------
--  components-utils-scripts -- Javascript queue
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

end ASF.Components.Utils.Scripts;
