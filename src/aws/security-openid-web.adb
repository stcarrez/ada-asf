-----------------------------------------------------------------------
--  openid -- Open ID 2.0 Support
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
with AWS.URL;
with AWS.Client;
with AWS.Resources;
with AWS.Response;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.SSL.Certificate;
with AWS.Status;
with AWS.Headers;
with AWS.Headers.Set;
package body Security.Openid.Web is

   use type AWS.Messages.Status_Code;

   overriding
   procedure Get_Request (Realm         : in Manager;
                          URI           : in String;
                          Accept_Format : in String;
                          Result        : out Unbounded_String) is
      Data    : AWS.Response.Data;
      Code    : AWS.Messages.Status_Code;
      Headers : AWS.Headers.List;
   begin
      AWS.Headers.Set.Add (Headers, "Accept", Accept_Format);
      Data := AWS.Client.Get (URL => URI, Headers => Headers);
      Code := AWS.Response.Status_Code (Data);

      if Code /= AWS.Messages.S200 then
         raise Service_Error with "Request failed with code "
           & AWS.Messages.Status_Code'Image (Code);
      end if;

      Result := AWS.Response.Message_Body (Data);
   end Get_Request;

   procedure Post_Request (Realm  : in Manager;
                           URI    : in String;
                           Params : in String;
                           Result : out Ada.Strings.Unbounded.Unbounded_String) is
      Data  : AWS.Response.Data;
      Code  : AWS.Messages.Status_Code;
   begin
      Data := AWS.Client.Post (URL => URI,
                               Data => Params);
      Code := AWS.Response.Status_Code (Data);
      Result := AWS.Response.Message_Body (Data);
   end Post_Request;

end Security.Openid.Web;
