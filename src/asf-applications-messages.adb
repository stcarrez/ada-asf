-----------------------------------------------------------------------
--  applications.messages -- Application Messages
--  Copyright (C) 2010, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body ASF.Applications.Messages is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Return the message severity level.
   --  ------------------------------
   function Get_Severity (Msg : in Message) return Severity is
   begin
      return Msg.Kind;
   end Get_Severity;

   --  ------------------------------
   --  Sets the message severity level.
   --  ------------------------------
   procedure Set_Severity (Msg  : in out Message;
                           Kind : in Severity) is
   begin
      Msg.Kind := Kind;
   end Set_Severity;

   --  ------------------------------
   --  Return the localized message summary.
   --  ------------------------------
   function Get_Summary (Msg : in Message) return String is
   begin
      return To_String (Msg.Summary);
   end Get_Summary;

   --  ------------------------------
   --  Sets the localized message summary.
   --  ------------------------------
   procedure Set_Summary (Msg     : in out Message;
                          Summary : in String) is
   begin
      Msg.Summary := To_Unbounded_String (Summary);
   end Set_Summary;

   --  ------------------------------
   --  Format the localized message with the arguments and set the message summary.
   --  ------------------------------
   procedure Format_Summary (Msg     : in out Message;
                             Summary : in String;
                             Args    : in ASF.Utils.Object_Array) is
   begin
      ASF.Utils.Formats.Format (Summary, Args, Msg.Summary);
   end Format_Summary;

   --  ------------------------------
   --  Return the localized message detail.  If the message detail was
   --  not provided, returns the message summary.
   --  ------------------------------
   function Get_Detail (Msg : in Message) return String is
   begin
      if Length (Msg.Detail) = 0 then
         return To_String (Msg.Summary);
      else
         return To_String (Msg.Detail);
      end if;
   end Get_Detail;

   --  ------------------------------
   --  Sets the localized message detail.
   --  ------------------------------
   procedure Set_Detail (Msg    : in out Message;
                         Detail : in String) is
   begin
      Msg.Detail := To_Unbounded_String (Detail);
   end Set_Detail;

   --  ------------------------------
   --  Returns true if both messages are identical (same severity, same messages)
   --  ------------------------------
   overriding
   function "=" (Left, Right : in Message) return Boolean is
   begin
      return Left.Kind = Right.Kind and then Left.Summary = Right.Summary
        and then Left.Detail = Right.Detail;
   end "=";

end ASF.Applications.Messages;
