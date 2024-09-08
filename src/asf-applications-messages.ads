-----------------------------------------------------------------------
--  applications.messages -- Application Messages
--  Copyright (C) 2010, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
