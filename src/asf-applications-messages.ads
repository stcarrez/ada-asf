-----------------------------------------------------------------------
--  applications.messages -- Application Messages
--  Copyright (C) 2010, 2018, 2022 Stephane Carrez
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

with Ada.Strings.Unbounded;
with ASF.Utils;
package ASF.Applications.Messages is

   type Severity is (NONE, INFO, WARN, ERROR, FATAL);

   --  ------------------------------
   --  Application message
   --  ------------------------------
   type Message is private;

   --  Return the message severity level.
   function Get_Severity (Msg : in Message) return Severity;

   --  Sets the message severity level.
   procedure Set_Severity (Msg  : in out Message;
                           Kind : in Severity);

   --  Return the localized message summary.
   function Get_Summary (Msg : in Message) return String;

   --  Sets the localized message summary.
   procedure Set_Summary (Msg     : in out Message;
                          Summary : in String);

   --  Format the localized message with the arguments and set the message summary.
   procedure Format_Summary (Msg     : in out Message;
                             Summary : in String;
                             Args    : in ASF.Utils.Object_Array);

   --  Return the localized message detail.  If the message detail was
   --  not provided, returns the message summary.
   function Get_Detail (Msg : in Message) return String;

   --  Sets the localized message detail.
   procedure Set_Detail (Msg    : in out Message;
                         Detail : in String);

   --  Returns true if both messages are identical (same severity, same messages)
   overriding
   function "=" (Left, Right : in Message) return Boolean;

private

   type Message is record
      Kind      : Severity := ERROR;
      Summary   : Ada.Strings.Unbounded.Unbounded_String;
      Detail    : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end ASF.Applications.Messages;
