-----------------------------------------------------------------------
--  asf.responses -- ASF Requests
--  Copyright (C) 2010 Stephane Carrez
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

--  The <b>ASF.Responses</b> package is an Ada implementation of
--  the Java servlet response (JSR 315 5. The Response).
package body ASF.Responses.Mockup is

   --  ------------------------------
   --  Adds the specified cookie to the response. This method can be called multiple
   --  times to set more than one cookie.
   --  ------------------------------
   procedure Add_Cookie (Resp : in out Response;
                         Cookie : in String) is
   begin
      null;
   end Add_Cookie;

   --  ------------------------------
   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   --  ------------------------------
   function Contains_Header (Resp : in Response;
                             Name : in String) return Boolean is
      Pos : constant Util.Strings.Maps.Cursor := Resp.Headers.Find (Name);
   begin
      return Util.Strings.Maps.Has_Element (Pos);
   end Contains_Header;

   --  ------------------------------
   --  Iterate over the response headers and executes the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate_Headers (Resp    : in Response;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is

      procedure Process_Wrapper (Position : in Util.Strings.Maps.Cursor);

      procedure Process_Wrapper (Position : in Util.Strings.Maps.Cursor) is
      begin
         Process.all (Name  => Util.Strings.Maps.Key (Position),
                      Value => Util.Strings.Maps.Element (Position));
      end Process_Wrapper;

   begin
      Resp.Headers.Iterate (Process => Process_Wrapper'Access);
   end Iterate_Headers;

   --  ------------------------------
   --  Sets a response header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   --  ------------------------------
   procedure Set_Header (Resp  : in out Response;
                         Name  : in String;
                         Value : in String) is
   begin
      Resp.Headers.Include (Name, Value);
   end Set_Header;

   --  ------------------------------
   --  Adds a response header with the given name and value.
   --  This method allows response headers to have multiple values.
   --  ------------------------------
   procedure Add_Header (Resp  : in out Response;
                         Name  : in String;
                         Value : in String) is
   begin
      Resp.Headers.Insert (Name, Value);
   end Add_Header;

   --  ------------------------------
   --  Get the content written to the mockup output stream.
   --  ------------------------------
   procedure Read_Content (Resp : in out Response;
                           Into : out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Resp.Content.Read (Into => Into);
   end Read_Content;

   --  ------------------------------
   --  Clear the response content.
   --  This operation removes any content held in the output stream, clears the status,
   --  removes any header in the response.
   --  ------------------------------
   procedure Clear (Resp : in out Response) is
      Content : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Resp.Read_Content (Content);
      Resp.Status := SC_OK;
      Resp.Headers.Clear;
   end Clear;

   --  ------------------------------
   --  Initialize the response mockup output stream.
   --  ------------------------------
   overriding
   procedure Initialize (Resp : in out Response) is
   begin
      Resp.Content.Initialize (128 * 1024);
      Resp.Stream := Resp.Content'Unchecked_Access;
   end Initialize;

end ASF.Responses.Mockup;
