-----------------------------------------------------------------------
--  applications.messages-factory -- Application Message Factory
--  Copyright (C) 2011 Stephane Carrez
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
      App    : constant access ASF.Applications.Main.Application'Class := Context.Get_Application;
      Bundle : ASF.Locales.Bundle;
   begin
      if Pos > 0 then
         App.Load_Bundle (Name   => Message_Id (Message_Id'First .. Pos - 1),
                          Locale => "en",
                          Bundle => Bundle);
         return Bundle.Get (Message_Id (Pos + 1 .. Message_Id'Last),
                            Message_Id (Pos + 1 .. Message_Id'Last));
      else
         App.Load_Bundle (Name   => "messages",
                          Locale => "en",
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
                         Message_Id : in String) return Message is
      Msg    : constant String := Get_Message (Context, Message_Id);
      Result : Message;
   begin
      Result.Summary := Ada.Strings.Unbounded.To_Unbounded_String (Msg);
      return Result;
   end Get_Message;

   --
   --
   --     function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
   --                           Message_Id : in String) return Message;
   --
   --     function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
   --                           Message_Id : in String;
   --                           Param1     : in String) return Message;

   --  ------------------------------
   --  Build a localized message and format the message with some arguments.
   --  ------------------------------
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String;
                         Args       : in ASF.Utils.Object_Array) return Message is
      Msg    : constant String := Get_Message (Context, Message_Id);
      Result : Message;
   begin
      ASF.Utils.Formats.Format (Msg, Args, Result.Summary);
      return Result;
   end Get_Message;


   --  ------------------------------
   --  Add a localized global message in the current faces context.
   --  ------------------------------
   procedure Add_Message (Message_Id : in String) is
      Context : constant ASF.Contexts.Faces.Faces_Context_Access := ASF.Contexts.Faces.Current;
      Msg     : constant Message := Get_Message (Context.all, Message_Id);
   begin
      Context.Add_Message (Client_Id => "", Message   => Msg);
   end Add_Message;

end ASF.Applications.Messages.Factory;
