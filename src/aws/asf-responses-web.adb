-----------------------------------------------------------------------
--  asf.requests -- ASF Requests
--  Copyright (C) 2009, 2010 Stephane Carrez
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
with Ada.Streams;
with Util.Streams.Buffered;
with AWS.Messages;
package body ASF.Responses.Web is

   procedure Initialize (Resp : in out Response) is
   begin
      Resp.Content.Initialize (256 * 1024);
      Resp.Stream := Resp.Content'Unchecked_Access;
   end Initialize;

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

   function Get_Data (Resp : in Response) return AWS.Response.Data is
      use Ada.Streams;

      Buffer : Util.Streams.Buffered.Buffer_Access := Resp.Content.Get_Buffer;
      Size   : Stream_Element_Offset := Stream_Element_Offset (Resp.Content.Get_Size);

   begin

      return AWS.Response.Build (Content_Type => Resp.Get_Content_Type,
                                 Message_Body => Buffer.all (Buffer'First .. Size),
                                 Status_Code  => To_Status_Code (Resp.Get_Status));
   end Get_Data;

end ASF.Responses.Web;
