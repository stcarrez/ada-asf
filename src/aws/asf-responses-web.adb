-----------------------------------------------------------------------
--  asf.responses.web -- ASF Responses with AWS server
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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

with AWS.Headers;
with AWS.Messages;
with AWS.Response.Set;
with AWS.Containers.Tables;
package body ASF.Responses.Web is

   procedure Initialize (Resp : in out Response) is
   begin
      Resp.Content.Initialize (Size   => 256 * 1024,
                               Output => Resp'Unchecked_Access,
                               Input  => null);
      Resp.Stream := Resp.Content'Unchecked_Access;
   end Initialize;

   --  ------------------------------
   --  Write the buffer array to the output stream.
   --  ------------------------------
   procedure Write (Stream : in out Response;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
   begin
      AWS.Response.Set.Append_Body (D    => Stream.Data,
                                    Item => Buffer);
   end Write;

   --  ------------------------------
   --  Flush the buffer (if any) to the sink.
   --  ------------------------------
   procedure Flush (Stream : in out Response) is
   begin
      null;
   end Flush;

   function To_Status_Code (Status : in Natural) return AWS.Messages.Status_Code is
      use AWS.Messages;
   begin
      case Status is
         when 100 =>
            return S100;
         when 101 =>
            return S101;
         when 200 =>
            return S200;
         when 201 =>
            return S201;
         when 202 =>
            return S202;
         when 400 =>
            return S400;
         when 401 =>
            return S401;
         when 402 =>
            return S402;
         when 403 =>
            return S403;
         when 404 =>
            return S404;
         when others =>
            return S500;
      end case;
   end To_Status_Code;

   --  ------------------------------
   --  Iterate over the response headers and executes the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate_Headers (Resp    : in Response;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is
      Headers : constant AWS.Headers.List := AWS.Response.Header (Resp.Data);
   begin
      AWS.Containers.Tables.Iterate_Names (AWS.Containers.Tables.Table_Type (Headers),
                                           ";", Process);
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
      if AWS.Response.Header (Resp.Data, Name)'Length = 0 then
         AWS.Response.Set.Add_Header (D     => Resp.Data,
                                      Name  => Name,
                                      Value => Value);
      end if;
   end Set_Header;

   --  ------------------------------
   --  Adds a response header with the given name and value.
   --  This method allows response headers to have multiple values.
   --  ------------------------------
   procedure Add_Header (Resp  : in out Response;
                         Name  : in String;
                         Value : in String) is
   begin
      AWS.Response.Set.Add_Header (D     => Resp.Data,
                                   Name  => Name,
                                   Value => Value);
   end Add_Header;

   --  ------------------------------
   --  Prepare the response data by collecting the status, content type and message body.
   --  ------------------------------
   procedure Build (Resp : in out Response) is
   begin
      AWS.Response.Set.Content_Type (D     => Resp.Data,
                                     Value => Resp.Get_Content_Type);
      AWS.Response.Set.Status_Code (D     => Resp.Data,
                                    Value => To_Status_Code (Resp.Get_Status));
      Resp.Content.Flush;
   end Build;

   --  ------------------------------
   --  Get the response data
   --  ------------------------------
   function Get_Data (Resp : in Response) return AWS.Response.Data is
   begin
      return Resp.Data;
   end Get_Data;

end ASF.Responses.Web;
