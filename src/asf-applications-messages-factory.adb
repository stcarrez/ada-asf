-----------------------------------------------------------------------
--  applications.messages-factory -- Application Message Factory
--  Copyright (C) 2011, 2012, 2015, 2018 Stephane Carrez
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

with Ada.Exceptions;

with Util.Log.Loggers;
with Util.Properties.Bundles;
with Util.Beans.Objects;
with Util.Locales;

with ASF.Locales;
with ASF.Applications.Main;
package body ASF.Applications.Messages.Factory is

   use Util.Log;

   Log : constant Loggers.Logger := Loggers.Create ("ASF.Applications.Messages.Factory");

   --  ------------------------------
   --  Get a localized message.  The message identifier is composed of a resource bundle name
   --  prefix and a bundle key.  The prefix and key are separated by the first '.'.
   --  If the message identifier does not contain any prefix, the default bundle is "messages".
   --  ------------------------------
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String) return String is
      Pos    : constant Natural := Util.Strings.Index (Message_Id, '.');
      Locale : constant Util.Locales.Locale := Context.Get_Locale;
      App    : constant access ASF.Applications.Main.Application'Class := Context.Get_Application;
      Bundle : ASF.Locales.Bundle;
   begin
      if Pos > 0 then
         App.Load_Bundle (Name   => Message_Id (Message_Id'First .. Pos - 1),
                          Locale => Util.Locales.To_String (Locale),
                          Bundle => Bundle);
         return Bundle.Get (Message_Id (Pos + 1 .. Message_Id'Last),
                            Message_Id (Pos + 1 .. Message_Id'Last));
      else
         App.Load_Bundle (Name   => "messages",
                          Locale => Util.Locales.To_String (Locale),
                          Bundle => Bundle);
         return Bundle.Get (Message_Id, Message_Id);
      end if;

   exception
      when E : Util.Properties.Bundles.NO_BUNDLE =>
         Log.Error ("Cannot localize {0}: {1}", Message_Id, Ada.Exceptions.Exception_Message (E));
         return Message_Id;
   end Get_Message;

   --  ------------------------------
   --  Build a localized message.
   --  ------------------------------
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String;
                         Severity   : in Messages.Severity := ERROR) return Message is
      Msg    : constant String := Get_Message (Context, Message_Id);
      Result : Message;
   begin
      Result.Summary := Ada.Strings.Unbounded.To_Unbounded_String (Msg);
      Result.Kind    := Severity;
      return Result;
   end Get_Message;

   --  ------------------------------
   --  Build a localized message and format the message with one argument.
   --  ------------------------------
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String;
                         Param1     : in String;
                         Severity   : in Messages.Severity := ERROR) return Message is
      Args : ASF.Utils.Object_Array (1 .. 1);
   begin
      Args (1) := Util.Beans.Objects.To_Object (Param1);
      return Get_Message (Context, Message_Id, Args, Severity);
   end Get_Message;

   --  ------------------------------
   --  Build a localized message and format the message with two argument.
   --  ------------------------------
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String;
                         Param1     : in String;
                         Param2     : in String;
                         Severity   : in Messages.Severity := ERROR) return Message is
      Args : ASF.Utils.Object_Array (1 .. 2);
   begin
      Args (1) := Util.Beans.Objects.To_Object (Param1);
      Args (2) := Util.Beans.Objects.To_Object (Param2);
      return Get_Message (Context, Message_Id, Args, Severity);
   end Get_Message;

   --  ------------------------------
   --  Build a localized message and format the message with some arguments.
   --  ------------------------------
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String;
                         Args       : in ASF.Utils.Object_Array;
                         Severity   : in Messages.Severity := ERROR) return Message is
      Msg    : constant String := Get_Message (Context, Message_Id);
      Result : Message;
   begin
      Result.Kind := Severity;
      Format_Summary (Result, Msg, Args);
      return Result;
   end Get_Message;

   --  ------------------------------
   --  Add a localized global message in the faces context.
   --  ------------------------------
   procedure Add_Message (Context    : in out ASF.Contexts.Faces.Faces_Context'Class;
                          Message_Id : in String;
                          Param1     : in String;
                          Severity   : in Messages.Severity := ERROR) is
      Msg     : constant Message := Get_Message (Context, Message_Id, Param1, Severity);
   begin
      Context.Add_Message (Client_Id => "", Message  => Msg);
   end Add_Message;

   --  ------------------------------
   --  Add a localized global message in the faces context.
   --  ------------------------------
   procedure Add_Message (Context    : in out ASF.Contexts.Faces.Faces_Context'Class;
                          Message_Id : in String;
                          Severity   : in Messages.Severity := ERROR) is
      Msg     : constant Message := Get_Message (Context, Message_Id, Severity);
   begin
      Context.Add_Message (Client_Id => "", Message  => Msg);
   end Add_Message;

   --  ------------------------------
   --  Add a localized global message in the current faces context.
   --  ------------------------------
   procedure Add_Message (Message_Id : in String;
                          Severity   : in Messages.Severity := ERROR) is
      Context : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
   begin
      Add_Message (Context.all, Message_Id, Severity);
   end Add_Message;

   --  ------------------------------
   --  Add a localized field message in the current faces context.  The message is associated
   --  with the component identified by <tt>Client_Id</tt>.
   --  ------------------------------
   procedure Add_Field_Message (Client_Id  : in String;
                                Message_Id : in String;
                                Severity   : in Messages.Severity := ERROR) is
      Context : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Msg     : constant Message := Get_Message (Context.all, Message_Id, Severity);
   begin
      Context.Add_Message (Client_Id => Client_Id, Message  => Msg);
   end Add_Field_Message;

end ASF.Applications.Messages.Factory;
