-----------------------------------------------------------------------
--  asf-contexts-writer-string -- A simple string writer
--  Copyright (C) 2009, 2010, 2011, 2017, 2022 Stephane Carrez
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

--  Implements a <b>ResponseWriter</b> that puts the result in a string.
--  The response content can be retrieved after the response is rendered.
package body ASF.Contexts.Writer.String is

   overriding
   procedure Initialize (Stream : in out String_Writer) is
      Output : ASF.Streams.Print_Stream;
   begin
      Stream.Content.Initialize (Size => 256 * 1024);
      Output.Initialize (Stream.Content'Unchecked_Access);
      Stream.Initialize ("text/xml", "utf-8", Output);
   end Initialize;

   --  ------------------------------
   --  Get the response
   --  ------------------------------
   function Get_Response (Stream : in String_Writer) return Unbounded_String is
      use Util.Streams;
   begin
      return To_Unbounded_String (Texts.To_String (Stream.Content));
   end Get_Response;

end ASF.Contexts.Writer.String;
