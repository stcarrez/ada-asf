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

with ASF.Contexts.Faces;
package ASF.Applications.Messages.Factory is

   --  Get a localized message.  The message identifier is composed of a resource bundle name
   --  prefix and a bundle key.  The prefix and key are separated by the first '.'.
   --  If the message identifier does not contain any prefix, the default bundle is "messages".
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String) return String;

   --  Build a localized message.
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String;
                         Severity   : in Messages.Severity := ERROR) return Message;

   --  Build a localized message and format the message with one argument.
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String;
                         Param1     : in String;
                         Severity   : in Messages.Severity := ERROR) return Message;

   --  Build a localized message and format the message with two argument.
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String;
                         Param1     : in String;
                         Param2     : in String;
                         Severity   : in Messages.Severity := ERROR) return Message;

   --  Build a localized message and format the message with some arguments.
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String;
                         Args       : in ASF.Utils.Object_Array;
                         Severity   : in Messages.Severity := ERROR) return Message;

   --  Add a localized global message in the current faces context.
   procedure Add_Message (Message_Id : in String;
                          Severity   : in Messages.Severity := ERROR);

   --  Add a localized field message in the current faces context.  The message is associated
   --  with the component identified by <tt>Client_Id</tt>.
   procedure Add_Field_Message (Client_Id  : in String;
                                Message_Id : in String;
                                Severity   : in Messages.Severity := ERROR);

   --  Add a localized global message in the faces context.
   procedure Add_Message (Context    : in out ASF.Contexts.Faces.Faces_Context'Class;
                          Message_Id : in String;
                          Param1     : in String;
                          Severity   : in Messages.Severity := ERROR);

   --  Add a localized global message in the faces context.
   procedure Add_Message (Context    : in out ASF.Contexts.Faces.Faces_Context'Class;
                          Message_Id : in String;
                          Severity   : in Messages.Severity := ERROR);

end ASF.Applications.Messages.Factory;
