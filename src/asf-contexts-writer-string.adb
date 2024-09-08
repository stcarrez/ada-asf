-----------------------------------------------------------------------
--  asf-contexts-writer-string -- A simple string writer
--  Copyright (C) 2009, 2010, 2011, 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
