-----------------------------------------------------------------------
--  Faces Context Tests - Unit tests for ASF.Contexts.Faces
--  Copyright (C) 2010, 2011 Stephane Carrez
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

with AUnit.Test_Caller;

with Util.Tests;

package body ASF.Contexts.Faces.Tests is

   use Util.Tests;

   package Caller is new AUnit.Test_Caller (Test);

   procedure Add_Tests (Suite : AUnit.Test_Suites.Access_Test_Suite) is

   begin
      --  To document what is tested, register the test methods for each
      --  operation that is tested.
      Suite.Add_Test (Caller.Create ("Test ASF.Contexts.Faces.Add_Message",
                                     Test_Add_Message'Access));
      Suite.Add_Test (Caller.Create ("Test ASF.Contexts.Faces.Max_Severity",
                                     Test_Max_Severity'Access));
      Suite.Add_Test (Caller.Create ("Test ASF.Contexts.Faces.Get_Message",
                                     Test_Get_Messages'Access));
   end Add_Tests;

   --  Test the faces message queue.
   procedure Test_Add_Message (T : in out Test) is
      Ctx : Faces_Context;
   begin
      Ctx.Add_Message (Client_Id => "", Message => "msg1");
      Ctx.Add_Message (Client_Id => "", Message => "msg1");
      Ctx.Add_Message (Client_Id => "", Message => "msg2");
      Ctx.Add_Message (Client_Id => "", Message => "msg3");
      Ctx.Add_Message (Client_Id => "info", Message => "msg3", Severity => INFO);
      Ctx.Add_Message (Client_Id => "warn", Message => "msg3", Severity => WARN);
      Ctx.Add_Message (Client_Id => "error", Message => "msg3", Severity => ERROR);
      Ctx.Add_Message (Client_Id => "fatal", Message => "msg3", Severity => FATAL);
      T.Assert (Ctx.Get_Maximum_Severity = FATAL, "Add message failed");
   end Test_Add_Message;

   procedure Test_Max_Severity (T : in out Test) is
      Ctx : Faces_Context;
   begin
      T.Assert (Ctx.Get_Maximum_Severity = NONE, "Invalid max severity with no message");

      Ctx.Add_Message (Client_Id => "info", Message => "msg3", Severity => INFO);
      T.Assert (Ctx.Get_Maximum_Severity = INFO, "Invalid max severity with info message");

      Ctx.Add_Message (Client_Id => "info", Message => "msg3", Severity => WARN);
      T.Assert (Ctx.Get_Maximum_Severity = WARN, "Invalid max severity with warn message");

      Ctx.Add_Message (Client_Id => "info", Message => "msg3", Severity => FATAL);
      T.Assert (Ctx.Get_Maximum_Severity = FATAL, "Invalid max severity with warn message");
   end Test_Max_Severity;

   procedure Test_Get_Messages (T : in out Test) is
      Ctx : Faces_Context;
   begin
      --  Iterator on an empty message list.
      declare
         Iter : constant Vectors.Cursor := Ctx.Get_Messages (Client_Id => "");
      begin
         T.Assert (not Vectors.Has_Element (Iter), "Iterator should indicate no message");
      end;

      Ctx.Add_Message (Client_Id => "info", Message => "msg1", Severity => INFO);
      declare
         Iter : constant Vectors.Cursor := Ctx.Get_Messages (Client_Id => "info");
         M    : Message;
      begin
         T.Assert (Vectors.Has_Element (Iter), "Iterator should indicate a message");

         M := Vectors.Element (Iter);
         Assert_Equals (T, "msg1", Get_Summary (M), "Invalid message");
         Assert_Equals (T, "msg1", Get_Detail (M), "Invalid details");
         T.Assert (INFO = Get_Severity (M), "Invalid severity");
      end;
   end Test_Get_Messages;

end ASF.Contexts.Faces.Tests;
