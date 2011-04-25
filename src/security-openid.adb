-----------------------------------------------------------------------
--  security-openid -- Open ID 2.0 Support
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
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Util.Log.Loggers;
with GNAT.SHA1;
package body Security.Openid is

   use Ada.Strings.Fixed;
   use Util.Log;

   Log : constant Util.Log.Loggers.Logger := Loggers.Create ("Security.Openid");

   --  ------------------------------
   --  Read-only Bean interface.
   --  ------------------------------

   --  ------------------------------
   --  Get the email address
   --  ------------------------------
   function Get_Email (Auth : Authentication) return String is
   begin
      return To_String (Auth.Email);
   end Get_Email;

   --  Initialize the OpenID realm.
   procedure Initialize (Realm     : in out Manager;
                         Name      : in String;
                         Return_To : in String) is
   begin
      Realm.Realm := To_Unbounded_String (Name);
      Realm.Return_To := To_Unbounded_String (Return_To);
   end Initialize;

   --  Discover the OpenID provider that must be used to authenticate the user.
   --  The <b>Name</b> can be an URL or an alias that identifies the provider.
   --  A cached OpenID provider can be returned.
   --  (See OpenID Section 7.3 Discovery)
   procedure Discover (Realm  : in out Manager;
                       Name   : in String;
                       Result : out End_Point) is

   begin
      Manager'Class (Realm).Discover_XRDS (URI    => Name,
                                           Result => Result);
   end Discover;

   --  ------------------------------
   --  Read the XRDS document from the URI and initialize the OpenID provider end point.
   --  ------------------------------
   procedure Discover_XRDS (Realm  : in out Manager;
                            URI    : in String;
                            Result : out End_Point) is
      Output : Unbounded_String;
   begin
      Log.Info ("Discover XRDS on {0}", URI);

      Manager'Class (Realm).Get_Request (URI           => URI,
                                         Accept_Format => "application/xrds+xml",
                                         Result        => Output);
      Manager'Class (Realm).Extract_XRDS (Content => To_String (Output),
                                          Result  => Result);
   end Discover_XRDS;

   function Extract (From      : String;
                     Start_Tag : String;
                     End_Tag   : String) return String is
      Pos  : Natural := Index (From, Start_Tag);
      Last : Natural;
      Url_Pos : Natural;
   begin
      if Pos = 0 then
         Pos := Index (From, Start_Tag (Start_Tag'First .. Start_Tag'Last - 1));
         if Pos = 0 then
            return "";
         end if;
         Pos := Index (From, ">", Pos + 1);
         if Pos = 0 then
            return "";
         end if;
         Url_Pos := Pos + 1;
      else
         Url_Pos := Pos + Start_Tag'Length;
      end if;
      Last := Index (From, End_Tag, Pos);
      if Last <= Pos then
         return "";
      end if;
      return From (Url_Pos .. Last - 1);
   end Extract;

   --  ------------------------------
   --  Extract from the XRDS content the OpenID provider URI.
   --  The default implementation is very basic as it returns the first <URI>
   --  available in the stream without validating the XRDS document.
   --  Raises the <b>Invalid_End_Point</b> exception if the URI cannot be found.
   --  ------------------------------
   procedure Extract_XRDS (Realm   : in out Manager;
                           Content : in String;
                           Result  : out End_Point) is
      pragma Unreferenced (Realm);

      URI : constant String := Extract (Content, "<URI>", "</URI>");
   begin
      if URI'Length = 0 then
         raise Invalid_End_Point with "Cannot extract the <URI> from the XRDS document";
      end if;
      Result.URL := To_Unbounded_String (URI);
   end Extract_XRDS;

   function Get_Association_Query return String is
   begin
      return "openid.ns=http://specs.openid.net/auth/2.0&"
        & "openid.mode=associate&"
        & "openid.session_type=no-encryption&"
        & "openid.assoc_type=HMAC-SHA1";
   end Get_Association_Query;

   --  ------------------------------
   --  Associate the application (relying party) with the OpenID provider.
   --  The association can be cached.
   --  (See OpenID Section 8 Establishing Associations)
   --  ------------------------------
   procedure Associate (Realm  : in out Manager;
                        OP     : in End_Point;
                        Result : out Association) is
      Output : Unbounded_String;
      Params : constant String := Get_Association_Query;
      Pos, Last, N : Natural;
   begin
      Manager'Class (Realm).Post_Request (URI    => To_String (OP.URL),
                                          Params => Params,
                                          Result => Output);
      Pos := 1;
      while Pos < Length (Output) loop
         N := Index (Output, ":", Pos);
         exit when N = 0;
         Last := Index (Output, "" & ASCII.LF, N);
         if Last = 0 then
            Last := Length (Output);
         else
            Last := Last - 1;
         end if;
         declare
            Key : constant String := Slice (Output, Pos, N - 1);
         begin
            if Key = "session_type" then
               Result.Session_Type := Unbounded_Slice (Output, N + 1, Last);
            elsif Key = "assoc_type" then
               Result.Assoc_Type := Unbounded_Slice (Output, N + 1, Last);
            elsif Key = "assoc_handle" then
               Result.Assoc_Handle := Unbounded_Slice (Output, N + 1, Last);
            elsif Key = "mac_key" then
               Result.Mac_Key := Unbounded_Slice (Output, N + 1, Last);
            elsif Key = "expires_in" then
               declare
                  Val : constant String := Slice (Output, N + 1, Last);
                  --                    Expires : Integer := Integer'Value (Val);
               begin
                  Ada.Text_IO.Put_Line ("Expires: |" & Val & "|");
                  Result.Expired := Ada.Calendar.Clock;
               end;
            elsif Key /= "ns" then
               Ada.Text_IO.Put_Line ("Key not recognized: " & Key);
            end if;
         end;
         Pos := Last + 2;
      end loop;
      Ada.Text_IO.Put_Line ("Result: " & To_String (Output));
   end Associate;

   function Get_Authentication_URL (Realm : in Manager;
                                    OP    : in End_Point;
                                    Assoc : in Association) return String is
      Result : Unbounded_String := OP.URL;
      Axa : constant String := "ax";
   begin
      if Index (Result, "?") > 0 then
         Append (Result, "&");
      else
         Append (Result, "?");
      end if;
      Append (Result, "openid.ns=http://specs.openid.net/auth/2.0");
      Append (Result, "&openid.claimed_id=http://specs.openid.net/auth/2.0/identifier_select");
      Append (Result, "&openid.identity=http://specs.openid.net/auth/2.0/identifier_select");
      Append (Result, "&openid.mode=checkid_setup");
      Append (Result, "&openid.ns." & Axa & "=http://openid.net/srv/ax/1.0");
      Append (Result, "&openid." & Axa & ".mode=fetch_request");
      Append (Result, "&openid." & Axa & ".type.email=http://axschema.org/contact/email");
      Append (Result, "&openid." & Axa & ".type.fullname=http://axschema.org/namePerson");
      Append (Result, "&openid." & Axa & ".type.language=http://axschema.org/pref/language");
      Append (Result, "&openid." & Axa & ".type.firstname=http://axschema.org/namePerson/first");
      Append (Result, "&openid." & Axa & ".type.lastname=http://axschema.org/namePerson/last");
      Append (Result, "&openid." & Axa & ".type.gender=http://axschema.org/person/gender");
      Append (Result, "&openid." & Axa & ".required=email,fullname,language,firstname,lastname,gender");
      Append (Result, "&openid.ns.sreg=http://openid.net/extensions/sreg/1.1");
      Append (Result, "&openid.sreg.required=email,fullname,gender,country,nickname");
      Append (Result, "&openid.return_to=");
      Append (Result, Realm.Return_To);
      Append (Result, "&openid.assoc_handle=");
      Append (Result, Assoc.Assoc_Handle);
      Append (Result, "&openid.realm=");
      Append (Result, Realm.Realm);
      return To_String (Result);
   end Get_Authentication_URL;

   function Get_Full_Name (Request : in ASF.Requests.Request'Class;
                           Axa     : String) return String is
   begin
      return Request.Get_Parameter ("openid." & Axa & ".value.fullname");
   end Get_Full_Name;

   function Get_Last_Name (Request : in ASF.Requests.Request'Class;
                           Axa     : String) return String is
   begin
      return Request.Get_Parameter ("openid." & Axa & ".value.lastname");
   end Get_Last_Name;

   procedure Set_Result (Result  : in out Authentication;
                         Status  : in Auth_Result;
                         Message : in String) is
   begin
      if Status /= AUTHENTICATED then
         Log.Error ("OpenID verification failed: {0}", Message);
      else
         Log.Info ("OpenID verification: {0}", Message);
      end if;
      Result.Status := Status;
   end Set_Result;

   procedure Extract_Value (Into    : in out Unbounded_String;
                            Request : in ASF.Requests.Request'Class;
                            Name    : in String) is
   begin
      if Length (Into) = 0 then
         Into := To_Unbounded_String (Request.Get_Parameter (Name));
      end if;
   end Extract_Value;

   procedure Extract_Profile (Prefix  : in String;
                              Request : in ASF.Requests.Request'Class;
                              Result  : in out Authentication) is
   begin
      Extract_Value (Result.Email, Request, Prefix & ".email");
      Extract_Value (Result.Nickname, Request, Prefix & ".nickname");
      Extract_Value (Result.Gender, Request, Prefix & ".gender");
      Extract_Value (Result.Country, Request, Prefix & ".country");
      Extract_Value (Result.Language, Request, Prefix & ".language");
      Extract_Value (Result.Full_Name, Request, Prefix & ".fullname");
      Extract_Value (Result.Timezone, Request, Prefix & ".timezone");
   end Extract_Profile;

   --  ------------------------------
   --  Verify the authentication result
   --  ------------------------------
   procedure Verify (Realm   : in out Manager;
                     Assoc   : in Association;
                     Request : in ASF.Requests.Request'Class;
                     Result  : out Authentication) is
      Value  : Unbounded_String := To_Unbounded_String (Request.Get_Parameter ("openid.mode"));
   begin
      --  Step 1: verify the response status
      if Value = "cancel" then
         Set_Result (Result, CANCEL, "Authentication refused");
         return;
      end if;

      if Value = "setup_needed" then
         Set_Result (Result, SETUP_NEEDED, "Setup is needed");
         return;
      end if;

      if Value /= "id_res" then
         Set_Result (Result, UNKNOWN, "Setup is needed");
         return;
      end if;

      --  OpenID Section: 11.1.  Verifying the Return URL
      Value := To_Unbounded_String (Request.Get_Parameter ("openid.return_to"));
      if Value /= Realm.Return_To then
         Set_Result (Result, UNKNOWN, "openid.return_to URL does not match");
         return;
      end if;

      --  OpenID Section: 11.2.  Verifying Discovered Information
      Manager'Class (Realm).Verify_Discovered (Assoc, Request, Result);

      --  OpenID Section: 11.3.  Checking the Nonce
      Value := To_Unbounded_String (Request.Get_Parameter ("openid.response_nonce"));

      --  OpenID Section: 11.4.  Verifying Signatures
      Manager'Class (Realm).Verify_Signature (Assoc, Request, Result);

      --  Extract profile information
      Value := To_Unbounded_String (Request.Get_Parameter ("openid.ns.sreg"));
      if Value = "http://openid.net/extensions/sreg/1.1" then
         Extract_Profile ("openid.sreg", Request, Result);
      end if;
      Value := To_Unbounded_String (Request.Get_Parameter ("openid.ns.ax"));
      if Value = "http://openid.net/srv/ax/1.0" then
         Extract_Profile ("openid.ax.value", Request, Result);
      end if;
      Value := To_Unbounded_String (Request.Get_Parameter ("openid.ns.ext1"));
      if Value = "http://openid.net/srv/ax/1.0" then
         Extract_Profile ("openid.ext1.value", Request, Result);
      end if;
   end Verify;

   --  ------------------------------
   --  Verify the signature part of the result
   --  ------------------------------
   procedure Verify_Signature (Realm   : in Manager;
                               Assoc   : in Association;
                               Request : in ASF.Requests.Request'Class;
                               Result  : in out Authentication) is
      Signed : constant String := Request.Get_Parameter ("openid.signed");
      Len    : constant Natural := Signed'Length;
      Sign   : Unbounded_String;
      Param  : Unbounded_String;
      Pos    : Natural := 1;
      Last   : Natural;
   begin
      while Pos < Len loop
         Last := Index (Signed, ",", Pos);
         if Last > 0 then
            Param := To_Unbounded_String (Signed (Pos .. Last - 1));
            Pos  := Last + 1;
         else
            Param := To_Unbounded_String (Signed (Pos .. Len));
            Pos  := Len + 1;
         end if;
         declare
            Name  : constant String := "openid." & To_String (Param);
            Value : constant String := Request.Get_Parameter (Name);
         begin
            Append (Sign, Name);
            Append (Sign, ':');
            Append (Sign, Value);
            Append (Sign, ASCII.LF);
         end;
      end loop;
      Log.Info ("Signing {0}", To_String (Sign));

      declare
         S         : String := Request.Get_Parameter ("openid.sig");
         Signature : GNAT.SHA1.Context;
         R         : GNAT.SHA1.Message_Digest;
      begin
         GNAT.SHA1.Update (Signature, To_String (Assoc.Mac_Key));
         GNAT.SHA1.Update (Signature, To_String (Sign));
         R := GNAT.SHA1.Digest (Signature);
         Log.Info ("Signature: {0} - {1}", R, S);
      end;
   end Verify_Signature;

   --  ------------------------------
   --  Verify the authentication result
   --  ------------------------------
   procedure Verify_Discovered (Realm   : in out Manager;
                                Assoc   : in Association;
                                Request : in ASF.Requests.Request'Class;
                                Result  : out Authentication) is
   begin
      Result.Claimed_Id := To_Unbounded_String (Request.Get_Parameter ("openid.claimed_id"));
      Result.Identity   := To_Unbounded_String (Request.Get_Parameter ("openid.identity"));
   end Verify_Discovered;

   function To_String (OP : End_Point) return String is
   begin
      return "openid://" & To_String (OP.URL);
   end To_String;

   function To_String (Assoc : Association) return String is
   begin
      return "session_type=" & To_String (Assoc.Session_Type)
        & "&assoc_type=" & To_String (Assoc.Assoc_Type)
        & "&assoc_handle=" & To_String (Assoc.Assoc_Handle)
        & "&mac_key=" & To_String (Assoc.Mac_Key);
   end To_String;

end Security.Openid;
