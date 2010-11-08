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
with Util.Locales;
with Ada.Calendar;

--  The <b>ASF.Responses</b> package is an Ada implementation of
--  the Java servlet response (JSR 315 5. The Response).
package ASF.Responses is

   SC_CONTINUE : constant Natural := 100;
   SC_SWITCHING_PROTOCOLS : constant Natural := 101;
   SC_OK : constant Natural := 200;
   SC_CREATED : constant Natural := 201;
   SC_ACCEPTED : constant Natural := 202;
   SC_NON_AUTHORITATIVE_INFORMATION : constant Natural := 203;
   SC_NO_CONTENT : constant Natural := 204;
   SC_RESET_CONTENT : constant Natural := 205;
   SC_PARTIAL_CONTENT : constant Natural := 206;
   SC_MULTIPLE_CHOICES : constant Natural := 300;
   SC_MOVED_PERMANENTLY : constant Natural := 301;
   SC_MOVED_TEMPORARILY : constant Natural := 302;
   SC_FOUND : constant Natural := 302;
   SC_SEE_OTHER : constant Natural := 303;
   SC_NOT_MODIFIED : constant Natural := 304;
   SC_USE_PROXY : constant Natural := 305;
   SC_TEMPORARY_REDIRECT : constant Natural := 307;
   SC_BAD_REQUEST : constant Natural := 400;
   SC_UNAUTHORIZED : constant Natural := 401;
   SC_PAYMENT_REQUIRED : constant Natural := 402;
   SC_FORBIDDEN : constant Natural := 403;
   SC_NOT_FOUND : constant Natural := 404;
   SC_METHOD_NOT_ALLOWED : constant Natural := 405;
   SC_NOT_ACCEPTABLE : constant Natural := 406;
   SC_PROXY_AUTHENTICATION_REQUIRED : constant Natural := 407;
   SC_REQUEST_TIMEOUT : constant Natural := 408;
   SC_CONFLICT : constant Natural := 409;
   SC_GONE : constant Natural := 410;
   SC_LENGTH_REQUIRED : constant Natural := 411;
   SC_PRECONDITION_FAILED : constant Natural := 412;
   SC_REQUEST_ENTITY_TOO_LARGE : constant Natural := 413;
   SC_REQUEST_URI_TOO_LONG : constant Natural := 414;
   SC_UNSUPPORTED_MEDIA_TYPE : constant Natural := 415;
   SC_REQUESTED_RANGE_NOT_SATISFIABLE : constant Natural := 416;
   SC_EXPECTATION_FAILED : constant Natural := 417;
   SC_INTERNAL_SERVER_ERROR : constant Natural := 500;
   SC_NOT_IMPLEMENTED : constant Natural := 501;
   SC_BAD_GATEWAY : constant Natural := 502;
   SC_SERVICE_UNAVAILABLE : constant Natural := 503;
   SC_GATEWAY_TIMEOUT : constant Natural := 504;
   SC_HTTP_VERSION_NOT_SUPPORTED : constant Natural := 505;


   type Response is abstract tagged limited private;

   --  Returns the name of the character encoding (MIME charset) used for the body
   --  sent in this response. The character encoding may have been specified explicitly
   --  using the setCharacterEncoding(String) or setContentType(String) methods,
   --  or implicitly using the setLocale(java.util.Locale) method. Explicit
   --  specifications take precedence over implicit specifications. Calls made
   --  to these methods after getWriter has been called or after the response has
   --  been committed have no effect on the character encoding. If no character
   --  encoding has been specified, ISO-8859-1 is returned.
   function Get_Character_Encoding (Resp : in Response) return String;

   --  Returns the content type used for the MIME body sent in this response.
   --  The content type proper must have been specified using
   --  setContentType(String)  before the response is committed. If no content type
   --  has been specified, this method returns null. If a content type has been
   --  specified, and a character encoding has been explicitly or implicitly specified
   --  as described in getCharacterEncoding()  or getWriter() has been called,
   --  the charset parameter is included in the string returned. If no character
   --  encoding has been specified, the charset parameter is omitted.
   function Get_Content_Type (Resp : in Response) return String;

   --  Sets the character encoding (MIME charset) of the response being sent to the
   --  client, for example, to UTF-8. If the character encoding has already been
   --  set by setContentType(java.lang.String) or setLocale(java.util.Locale),
   --  this method overrides it. Calling setContentType(java.lang.String) with the
   --  String  of text/html and calling this method with the String of UTF-8  is
   --  equivalent with calling setContentType with the String of text/html; charset=UTF-8.
   --
   --  This method can be called repeatedly to change the character encoding.
   --  This method has no effect if it is called after getWriter has been called or
   --  after the response has been committed.
   --
   --  Containers must communicate the character encoding used for the servlet
   --  response's writer to the client if the protocol provides a way for doing so.
   --  In the case of HTTP, the character encoding is communicated as part of the
   --  Content-Type  header for text media types. Note that the character encoding
   --  cannot be communicated via HTTP headers if the servlet does not specify
   --  a content type; however, it is still used to encode text written via the servlet
   --  response's writer.
   procedure Set_Character_Encoding (Resp     : in out Response;
                                     Encoding : in String);

   --  Sets the length of the content body in the response In HTTP servlets,
   --  this method sets the HTTP Content-Length header.
   procedure Set_Content_Length (Resp   : in out Response;
                                 Length : in Integer);


   --  Sets the content type of the response being sent to the client, if the response
   --  has not been committed yet. The given content type may include a character
   --  encoding specification, for example, text/html;charset=UTF-8. The response's
   --  character encoding is only set from the given content type if this method is
   --  called before getWriter  is called.
   --
   --  This method may be called repeatedly to change content type and character
   --  encoding. This method has no effect if called after the response has been
   --  committed. It does not set the response's character encoding if it is called
   --  after getWriter  has been called or after the response has been committed.
   --
   --  Containers must communicate the content type and the character encoding used
   --  for the servlet response's writer to the client if the protocol provides a way
   --  for doing so. In the case of HTTP, the Content-Type header is used.
   procedure Set_Content_Type (Resp    : in out Response;
                               Content : in String);

   --  Returns a boolean indicating if the response has been committed.
   --  A committed response has already had its status code and headers written.
   function Is_Committed (Resp : in Response) return Boolean;


   --  Sets the locale of the response, if the response has not been committed yet.
   --  It also sets the response's character encoding appropriately for the locale,
   --  if the character encoding has not been explicitly set using
   --  setContentType(java.lang.String) or setCharacterEncoding(java.lang.String),
   --  getWriter hasn't been called yet, and the response hasn't been committed yet.
   --  If the deployment descriptor contains a locale-encoding-mapping-list element,
   --  and that element provides a mapping for the given locale, that mapping is used.
   --  Otherwise, the mapping from locale to character encoding is container dependent.
   --
   --  This method may be called repeatedly to change locale and character encoding.
   --  The method has no effect if called after the response has been committed.
   --  It does not set the response's character encoding if it is called after
   --  setContentType(java.lang.String)  has been called with a charset specification,
   --  after setCharacterEncoding(java.lang.String) has been called,
   --  after getWriter has been called, or after the response has been committed.
   --
   --  Containers must communicate the locale and the character encoding used for
   --  the servlet response's writer to the client if the protocol provides a way
   --  for doing so. In the case of HTTP, the locale is communicated via the
   --  Content-Language header, the character encoding as part of the Content-Type
   --  header for text media types. Note that the character encoding cannot be
   --  communicated via HTTP headers if the servlet does not specify a content type;
   --  however, it is still used to encode text written via the servlet response's writer.
   procedure Set_Locale (Resp : in out Response;
                         Loc  : in Util.Locales.Locale);

   --  Returns the locale specified for this response using the
   --  setLocale(java.util.Locale) method. Calls made to setLocale after the
   --  response is committed have no effect. If no locale has been specified,
   --  the container's default locale is returned.
   function Get_Locale (Resp : in Response) return Util.Locales.Locale;

   --  Adds the specified cookie to the response. This method can be called multiple
   --  times to set more than one cookie.
   procedure Add_Cookie (Resp : in out Response;
                         Cookie : in String);

   --  Returns a boolean indicating whether the named response header has already
   --  been set.
   function Contains_Header (Resp : in Response;
                             Name : in String) return Boolean;

   --  Encodes the specified URL by including the session ID in it, or, if encoding
   --  is not needed, returns the URL unchanged. The implementation of this method
   --  includes the logic to determine whether the session ID needs to be encoded
   --  in the URL. For example, if the browser supports cookies, or session tracking
   --  is turned off, URL encoding is unnecessary.
   --
   --  For robust session tracking, all URLs emitted by a servlet should be run through
   --  this method. Otherwise, URL rewriting cannot be used with browsers which do not
   --  support cookies.
   function Encode_URL (Resp : in Response;
                        URL  : in String) return String;

   --  Encodes the specified URL for use in the sendRedirect method or, if encoding
   --  is not needed, returns the URL unchanged. The implementation of this method
   --  includes the logic to determine whether the session ID needs to be encoded
   --  in the URL. Because the rules for making this determination can differ from
   --  those used to decide whether to encode a normal link, this method is separated
   --  from the encodeURL method.
   --
   --  All URLs sent to the HttpServletResponse.sendRedirect  method should be run
   --  through this method. Otherwise, URL rewriting cannot be used with browsers
   --  which do not support cookies.
   function Encode_Redirect_URL (Resp : in Response;
                                 URL  : in String) return String;

   --  Sends an error response to the client using the specified status. The server
   --  defaults to creating the response to look like an HTML-formatted server error
   --  page containing the specified message, setting the content type to "text/html",
   --  leaving cookies and other headers unmodified. If an error-page declaration
   --  has been made for the web application corresponding to the status code passed
   --  in, it will be served back in preference to the suggested msg parameter.
   --
   --  If the response has already been committed, this method throws an
   --  IllegalStateException. After using this method, the response should be
   --  considered to be committed and should not be written to.
   procedure Send_Error (Resp    : in out Response;
                         Error   : in Integer;
                         Message : in String);

   --  Sends an error response to the client using the specified status code
   --  and clearing the buffer.
   --
   --  If the response has already been committed, this method throws an
   --  IllegalStateException. After using this method, the response should be
   --  considered to be committed and should not be written to.
   procedure Send_Error (Resp    : in out Response;
                         Error   : in Integer);

   --  Sends a temporary redirect response to the client using the specified redirect
   --  location URL. This method can accept relative URLs; the servlet container must
   --  convert the relative URL to an absolute URL before sending the response to the
   --  client. If the location is relative without a leading '/' the container
   --  interprets it as relative to the current request URI. If the location is relative
   --  with a leading '/' the container interprets it as relative to the servlet
   --  container root.
   --
   --  If the response has already been committed, this method throws an
   --  IllegalStateException. After using this method, the response should be
   --  considered to be committed and should not be written to.
   procedure Send_Redirect (Resp     : in out Response;
                            Location : in String);

   --  Sets a response header with the given name and date-value.
   --  The date is specified in terms of milliseconds since the epoch.
   --  If the header had already been set, the new value overwrites the previous one.
   --  The containsHeader method can be used to test for the presence of a header
   --  before setting its value.
   procedure Set_Date_Header (Resp  : in out Response;
                              Name  : in String;
                              Date  : in Ada.Calendar.Time);

   --  Adds a response header with the given name and date-value. The date is specified
   --  in terms of milliseconds since the epoch. This method allows response headers
   --  to have multiple values.
   procedure Add_Date_Header (Resp : in out Response;
                              Name : in String;
                              Date : in Ada.Calendar.Time);

   --  Sets a response header with the given name and value. If the header had already
   --  been set, the new value overwrites the previous one. The containsHeader
   --  method can be used to test for the presence of a header before setting its value.
   procedure Set_Header (Resp  : in out Response;
                         Name  : in String;
                         Value : in String);

   --  Adds a response header with the given name and value.
   --  This method allows response headers to have multiple values.
   procedure Add_Header (Resp  : in out Response;
                         Name  : in String;
                         Value : in String);

   --  Sets a response header with the given name and integer value.
   --  If the header had already been set, the new value overwrites the previous one.
   --  The containsHeader  method can be used to test for the presence of a header
   --  before setting its value.
   procedure Set_Int_Header (Resp  : in out Response;
                             Name  : in String;
                             Value : in Integer);

   --  Adds a response header with the given name and integer value. This method
   --  allows response headers to have multiple values.
   procedure Add_Int_Header (Resp  : in out Response;
                             Name  : in String;
                             Value : in Integer);

   --  Sets the status code for this response. This method is used to set the
   --  return status code when there is no error (for example, for the status
   --  codes SC_OK or SC_MOVED_TEMPORARILY). If there is an error, and the caller
   --  wishes to invoke an error-page defined in the web application, the sendError
   --  method should be used instead.
   --
   --  The container clears the buffer and sets the Location header,
   --  preserving cookies and other headers.
   procedure Set_Status (Resp   : in out Response;
                         Status : in Natural);

private

   type Response is abstract tagged limited null record;

end ASF.Responses;
