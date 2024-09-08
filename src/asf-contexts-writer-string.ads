-----------------------------------------------------------------------
--  asf-contexts-writer-string -- A simple string writer
--  Copyright (C) 2009, 2010, 2011, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Streams.Texts;

--  Implements a <b>ResponseWriter</b> that puts the result in a string.
--  The response content can be retrieved after the response is rendered.
package ASF.Contexts.Writer.String is

   --  ------------------------------
   --  String Writer
   --  ------------------------------
   type String_Writer is new Response_Writer with private;

   overriding
   procedure Initialize (Stream : in out String_Writer);

   --  Get the response
   function Get_Response (Stream : in String_Writer) return Unbounded_String;

private

   type String_Writer is new Response_Writer with record
      Content  : aliased Util.Streams.Texts.Print_Stream;
   end record;

end ASF.Contexts.Writer.String;
