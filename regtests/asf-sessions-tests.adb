-----------------------------------------------------------------------
--  Sessions Tests - Unit tests for ASF.Sessions
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

with Util.Test_Caller;
with ASF.Sessions.Factory;
with Util.Measures;
with EL.Objects;
package body ASF.Sessions.Tests is

   use Util.Tests;

   --  ------------------------------
   --  Test session creation.
   --  ------------------------------
   procedure Test_Create_Session (T : in out Test) is
      F : ASF.Sessions.Factory.Session_Factory;

      S : ASF.Sessions.Session;
      St : Util.Measures.Stamp;
   begin
      for I in 1 .. 10 loop
         F.Create_Session (S);
      end loop;
      Util.Measures.Report (St, "10 Session create");

      for I in 1 .. 100 loop
         F.Create_Session (S);

         T.Assert (S.Is_Valid, "Session should be valid");
         T.Assert (S.Get_Id'Length = 32, "Session id has an invalid length");
         --  Ada.Text_IO.Put_Line ("ID=" & S.Get_Id);

         declare
            S2 : ASF.Sessions.Session;
         begin
            F.Find_Session (Id => S.Get_Id, Result => S2);
            T.Assert (S2.Is_Valid, "Session was not found");

            Assert_Equals (T, S.Get_Id, S2.Get_Id, "Invalid session id");
         end;
      end loop;
   end Test_Create_Session;

   --  ------------------------------
   --  Tests on an empty session object.
   --  ------------------------------
   procedure Test_Empty_Session (T : in out Test) is
      S : ASF.Sessions.Session;
   begin
      T.Assert (not S.Is_Valid, "Session should be invalid");

      S.Invalidate;
      T.Assert (not S.Is_Valid, "Session should be invalid");
   end Test_Empty_Session;

   procedure Test_Session_Attributes (T : in out Test) is
      F : ASF.Sessions.Factory.Session_Factory;

      S : ASF.Sessions.Session;
   begin
      F.Create_Session (S);

      S.Set_Attribute ("a1", EL.Objects.To_Object (Integer (234)));
      S.Set_Attribute ("a2", EL.Objects.To_Object (String '("name")));

      declare
         Value : constant EL.Objects.Object := S.Get_Attribute ("a1");
      begin
         Assert_Equals (T, "234", EL.Objects.To_String (Value), "Invalid attribute a1");
      end;
   end Test_Session_Attributes;

   package Caller is new Util.Test_Caller (Test, "Sessions");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is

   begin
      --  To document what is tested, register the test methods for each
      --  operation that is tested.
      Caller.Add_Test (Suite, "Test ASF.Sessions.Factory.Create_Session",
                       Test_Create_Session'Access);
      Caller.Add_Test (Suite, "Test ASF.Sessions.Factory.Find_Session",
                       Test_Create_Session'Access);
      Caller.Add_Test (Suite, "Test ASF.Sessions.Get_Id",
                       Test_Create_Session'Access);
      Caller.Add_Test (Suite, "Test ASF.Sessions.Is_Valid",
                       Test_Empty_Session'Access);
      Caller.Add_Test (Suite, "Test ASF.Sessions.Set_Attribute",
                       Test_Session_Attributes'Access);
      Caller.Add_Test (Suite, "Test ASF.Sessions.Get_Attribute",
                       Test_Session_Attributes'Access);
   end Add_Tests;

end ASF.Sessions.Tests;
