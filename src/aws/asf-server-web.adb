-----------------------------------------------------------------------
--  asf.server -- ASF Server for AWS
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
with AWS.Templates;
with AWS.MIME;
with AWS.Messages;
with AWS.Services.Web_Block.Registry;

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Exceptions;

with EL.Beans;
with ASF.Beans;
with ASF.Applications.Views;
with ASF.Components.Core;
with ASF.Contexts.Faces;
with ASF.Contexts.Writer.String;

with ASF.Requests.Web;

with EL.Objects;
with EL.Contexts;
with EL.Contexts.Default;
with EL.Variables;
with EL.Variables.Default;

with Ada.Strings.Unbounded;
with Util.Log.Loggers;
with Ada.Containers.Vectors;
package body ASF.Server.Web is

   use Util.Log;
   use ASF.Contexts;
   use Ada.Exceptions;
   use AWS.Templates;
   use ASF.Beans;
   use Ada.Strings.Unbounded;

   Log : constant Loggers.Logger := Loggers.Create ("ASF.Server.Web");
   --  The logger

   function Reply_Page_500 (Request : in AWS.Status.Data;
                            E       : in Exception_Occurrence)
                            return AWS.Response.Data;

   function Dispatch (App  : Main.Application_Access;
                      Page : String;
                      Request : in AWS.Status.Data) return AWS.Response.Data;

   --  Binding to record the ASF applications and bind them to URI prefixes.
   type Binding is record
      Application : Main.Application_Access;
      Base_URI    : access String;
   end record;
   type Binding_Array is array (Natural range <>) of Binding;
   type Binding_Array_Access is access all Binding_Array;

   Nb_Bindings  : Natural := 0;
   Applications : Binding_Array_Access := null;

   --  Register the application to serve requests
   procedure Register_Application (URI    : in String;
                                   App    : in Main.Application_Access) is

      Apps : constant Binding_Array_Access := new Binding_Array (1 .. Nb_Bindings + 1);
   begin
      if Applications /= null then
         Apps (1 .. Nb_Bindings) := Applications (1 .. Nb_Bindings);
      end if;
      Nb_Bindings := Nb_Bindings + 1;
      Apps (Apps'Last).Application := App;
      Apps (Apps'Last).Base_URI := new String '(URI);
      Applications := Apps;


   end Register_Application;

   function Is_Static (App : Main.Application_Access;
                       Page : String) return Boolean is
      use Ada.Strings.Fixed;

      Slash_Pos   : constant Natural := Index (Page, "/", Page'First + 1);
   begin
      if Slash_Pos > Page'First then
         declare
            Dir         : constant String := Page (Page'First .. Slash_Pos);
            Static_Dirs : constant String := App.Get_Config (ASF.Applications.VIEW_STATIC_DIR_PARAM);
            Pos         : Natural := Index (Static_Dirs, ",");
         begin
            if Dir = "/scripts/" or Dir = "/themes/" then
               return True;
            end if;
--              while Pos < Static_Dirs'Last loop
--                 if Pos > 0 then
--                    if Dir = Static_Dirs (Start .. Pos) then
--                       return True;
--                    end if;
--                 end if;
--              end loop;
         end;
      end if;
      return False;
   end Is_Static;

   function Static_Dispatch (App     : Main.Application_Access;
                             Page    : String;
                             Request : AWS.Status.Data) return AWS.Response.Data is
      use Ada.Directories;

      Dir  : constant String := App.Get_Config (ASF.Applications.VIEW_DIR_PARAM);
      File : constant String := Dir & "/" & Page;
   begin
      if not Ada.Directories.Exists (File)
        or else Ada.Directories.Kind (File) /= Ada.Directories.Ordinary_File then
         return AWS.Response.Build (Content_Type => AWS.MIME.Text_HTML,
                                    Message_Body => "Invalid URI " & Page);
      end if;
      return AWS.Response.File (AWS.MIME.Content_Type (File), File);
   end Static_Dispatch;

   function Dispatch (App  : Main.Application_Access;
                      Page : String;
                      Request : in AWS.Status.Data) return AWS.Response.Data is

   begin
      if Is_Static (App, Page) then
         return Static_Dispatch (App, Page, Request);
      else
         declare
            Writer   : aliased ASF.Contexts.Writer.String.String_Writer;
            Req      : aliased ASF.Requests.Web.Request;
         begin
            Writer.Initialize ("text/html", "UTF-8", 8192);

            App.Dispatch (Page    => Page,
                          Writer  => Writer'Unchecked_Access,
                          Request => Req'Unchecked_Access);

            return AWS.Response.Build (Content_Type => Writer.Get_Content_Type,
                                       UString_Message => Writer.Get_Response);
         end;
      end if;
   end Dispatch;

   ----------------------
   --  Main server callback
   ----------------------
   function Server_Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      use Ada.Strings.Fixed;

      URI        : constant String := AWS.Status.URI (Request);
      Slash_Pos  : constant Natural := Index (URI, "/", URI'First + 1);
      Prefix_End : Natural;
   begin
      --  Find the module and action to invoke
      if Slash_Pos > 1 then
         Prefix_End := Slash_Pos - 1;
      else
         Prefix_End := URI'Last;
      end if;
      declare
         Prefix : constant String := URI (URI'First .. Prefix_End);
         Page   : constant String := URI (Prefix_End + 1 .. URI'Last);
      begin
         for I in Applications.all'Range loop
            if Applications (I).Base_URI.all = Prefix then
               return Dispatch (Applications (I).Application, Page, Request);
            end if;
         end loop;
      end;
      return AWS.Response.Build ("text/html", "<p>Unknown application</p>");
   exception
      when E : others =>
         return Reply_Page_500 (Request, E);
   end Server_Callback;


   --  Return a 500 error
   function Reply_Page_500 (Request : in AWS.Status.Data;
                            E       : in Exception_Occurrence)
                            return AWS.Response.Data is
      use type AWS.Messages.Status_Code;
      use AWS.Services;
      use AWS;

      Translations : Templates.Translate_Set;
      URI          : constant String := AWS.Status.URI (Request);
      Name         : constant String := Exception_Name (E);
      Message      : constant String := Exception_Message (E);
      ContentType  : constant String := Web_Block.Registry.Content_Type (URI);
   begin
      Templates.Insert (Translations,
                        Templates.Assoc ("URI", URI));
      Templates.Insert (Translations,
                        Templates.Assoc ("EXCEPTION_NAME", Name));
      Templates.Insert (Translations,
                        Templates.Assoc ("EXCEPTION_MESSAGE", Message));

      Log.Error ("Default_Callback exception for URI '{0}': {1}: {2}",
                 URI, Name, Message);

      if ContentType = AWS.MIME.Text_HTML then
         return AWS.Response.Build
           (Content_Type => AWS.MIME.Text_HTML,
            Message_Body => String'(Templates.Parse
              ("errors/exception.thtml",
                 Translations)));
      else
         return AWS.Response.Build
           (Content_Type => AWS.MIME.Text_XML,
            Message_Body => String'(Templates.Parse
              ("errors/exception.txml",
                 Translations)));
      end if;
   end Reply_Page_500;

end ASF.Server.Web;
