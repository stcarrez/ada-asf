-----------------------------------------------------------------------
--  asf.requests -- ASF Requests
--  Copyright (C) 2009, 2010, 2011, 2012, 2013 Stephane Carrez
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
with AWS.Attachments.Extend;
with AWS.Containers.Tables;
with AWS.Parameters;

with Util.Strings;

with ASF.Parts.Web;
package body ASF.Requests.Web is

   function Get_Parameter (R : Request; Name : String) return String is
   begin
      return AWS.Status.Parameter (R.Data.all, Name);
   end Get_Parameter;

   --  ------------------------------
   --  Iterate over the request parameters and executes the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate_Parameters (Req     : in Request;
                                 Process : not null access
                                   procedure (Name  : in String;
                                              Value : in String)) is
      procedure Process_Wrapper (Name, Value : in String);

      procedure Process_Wrapper (Name, Value : in String) is
         Last : Natural := Value'First;
         Pos  : Natural;
      begin
         while Last <= Value'Last loop
            Pos := Util.Strings.Index (Value, ASCII.NUL, Last);
            if Pos > 0 then
               Process (Name, Value (Last .. Pos - 1));
               Last := Pos + 1;
            else
               Process (Name, Value (Last .. Value'Last));
               return;
            end if;
         end loop;
      end Process_Wrapper;

      P : constant AWS.Parameters.List := AWS.Status.Parameters (Req.Data.all);
   begin
      AWS.Containers.Tables.Iterate_Names (AWS.Containers.Tables.Table_Type (P),
                                           "" & ASCII.NUL, Process_Wrapper'Access);
   end Iterate_Parameters;

   --  ------------------------------
   --  Set the AWS data received to initialize the request object.
   --  ------------------------------
   procedure Set_Request (Req  : in out Request;
                          Data : access AWS.Status.Data) is
   begin
      Req.Data    := Data;
      Req.Headers := AWS.Status.Header (Data.all);
   end Set_Request;

   --  ------------------------------
   --  Returns the name of the HTTP method with which this request was made,
   --  for example, GET, POST, or PUT. Same as the value of the CGI variable
   --  REQUEST_METHOD.
   --  ------------------------------
   function Get_Method (Req : in Request) return String is
   begin
      return AWS.Status.Method (Req.Data.all);
   end Get_Method;

   --  ------------------------------
   --  Returns the name and version of the protocol the request uses in the form
   --  protocol/majorVersion.minorVersion, for example, HTTP/1.1. For HTTP servlets,
   --  the value returned is the same as the value of the CGI variable SERVER_PROTOCOL.
   --  ------------------------------
   function Get_Protocol (Req : in Request) return String is
   begin
      return AWS.Status.HTTP_Version (Req.Data.all);
   end Get_Protocol;

   --  ------------------------------
   --  Returns the part of this request's URL from the protocol name up to the query
   --  string in the first line of the HTTP request. The web container does not decode
   --  this String. For example:
   --  First line of HTTP request    Returned Value
   --  POST /some/path.html HTTP/1.1        /some/path.html
   --  GET http://foo.bar/a.html HTTP/1.0       /a.html
   --  HEAD /xyz?a=b HTTP/1.1       /xyz
   --  ------------------------------
   function Get_Request_URI (Req : in Request) return String is
   begin
      return AWS.Status.URI (Req.Data.all);
   end Get_Request_URI;

   --  ------------------------------
   --  Returns the value of the specified request header as a String. If the request
   --  did not include a header of the specified name, this method returns null.
   --  If there are multiple headers with the same name, this method returns the
   --  first head in the request. The header name is case insensitive. You can use
   --  this method with any request header.
   --  ------------------------------
   function Get_Header (Req  : in Request;
                        Name : in String) return String is
      Values : constant AWS.Headers.VString_Array := AWS.Headers.Get_Values (Req.Headers, Name);
   begin
      if Values'Length > 0 then
         return To_String (Values (Values'First));
      else
         return "";
      end if;
   end Get_Header;

   --  ------------------------------
   --  Iterate over the request headers and executes the <b>Process</b> procedure.
   --  ------------------------------
   procedure Iterate_Headers (Req     : in Request;
                              Process : not null access
                                procedure (Name  : in String;
                                           Value : in String)) is
   begin
      Req.Headers.Iterate_Names (Coupler => ", ",
                                 Process => Process);
   end Iterate_Headers;

   --  ------------------------------
   --  Returns all the values of the specified request header as an Enumeration
   --  of String objects.
   --
   --  Some headers, such as Accept-Language can be sent by clients as several headers
   --  each with a different value rather than sending the header as a comma
   --  separated list.
   --
   --  If the request did not include any headers of the specified name, this method
   --  returns an empty Enumeration. The header name is case insensitive. You can use
   --  this method with any request header.
   --  ------------------------------
   function Get_Headers (Req  : in Request;
                         Name : in String) return String is
   begin
      return AWS.Headers.Get_Values (Req.Headers, Name);
   end Get_Headers;

   --  ------------------------------
   --  Returns the Internet Protocol (IP) address of the client or last proxy that
   --  sent the request. For HTTP servlets, same as the value of the CGI variable
   --  REMOTE_ADDR.
   --  ------------------------------
   function Get_Remote_Addr (Req : in Request) return String is
   begin
      return AWS.Status.Peername (Req.Data.all);
   end Get_Remote_Addr;

   --  ------------------------------
   --  Get the number of parts included in the request.
   --  ------------------------------
   function Get_Part_Count (Req : in Request) return Natural is
   begin
      return AWS.Attachments.Count (AWS.Status.Attachments (Req.Data.all));
   end Get_Part_Count;

   --  ------------------------------
   --  Process the part at the given position and executes the <b>Process</b> operation
   --  with the part object.
   --  ------------------------------
   procedure Process_Part (Req      : in out Request;
                           Position : in Positive;
                           Process  : not null access
                             procedure (Data : in ASF.Parts.Part'Class)) is
      Attachs : constant AWS.Attachments.List := AWS.Status.Attachments (Req.Data.all);
   begin
      ASF.Parts.Web.Process_Part (AWS.Attachments.Get (Attachs, Position), Process);
   end Process_Part;

   --  ------------------------------
   --  Process the part identifed by <b>Id</b> and executes the <b>Process</b> operation
   --  with the part object.
   --  ------------------------------
   procedure Process_Part (Req      : in out Request;
                           Id       : in String;
                           Process  : not null access
                             procedure (Data : in ASF.Parts.Part'Class)) is
      procedure Process_Part (E : in AWS.Attachments.Element);

      procedure Process_Part (E : in AWS.Attachments.Element) is
         Name : constant String := AWS.Attachments.Extend.Get_Name (E);
      begin
         if Id = Name then
            ASF.Parts.Web.Process_Part (E, Process);
         end if;
      end Process_Part;

      Attachs : constant AWS.Attachments.List := AWS.Status.Attachments (Req.Data.all);
   begin
      AWS.Attachments.Iterate (Attachs, Process_Part'Access);
--        ASF.Parts.Web.Process_Part (AWS.Attachments.Get (Attachs, Id), Process);
   end Process_Part;

end ASF.Requests.Web;
