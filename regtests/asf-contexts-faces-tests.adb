-----------------------------------------------------------------------
--  Faces Context Tests - Unit tests for ASF.Contexts.Faces
--  Copyright (C) 2010, 2011, 2012, 2013 Stephane Carrez
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

with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

with Util.Test_Caller;
with EL.Variables.Default;

with ASF.Contexts.Flash;
package body ASF.Contexts.Faces.Tests is

   use Util.Tests;

   package Caller is new Util.Test_Caller (Test, "Contexts.Faces");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is

   begin
      --  To document what is tested, register the test methods for each
      --  operation that is tested.
      Caller.Add_Test (Suite, "Test ASF.Contexts.Faces.Add_Message",
                       Test_Add_Message'Access);
      Caller.Add_Test (Suite, "Test ASF.Contexts.Faces.Max_Severity",
                       Test_Max_Severity'Access);
      Caller.Add_Test (Suite, "Test ASF.Contexts.Faces.Get_Message",
                       Test_Get_Messages'Access);
      Caller.Add_Test (Suite, "Test ASF.Contexts.Faces.Queue_Exception",
                       Test_Queue_Exception'Access);
      Caller.Add_Test (Suite, "Test ASF.Contexts.Faces.Get_Flash",
                       Test_Flash_Context'Access);
      Caller.Add_Test (Suite, "Test ASF.Contexts.Faces.Get_Attribute",
                       Test_Get_Attribute'Access);
      Caller.Add_Test (Suite, "Test ASF.Contexts.Faces.Get_Bean",
                       Test_Get_Bean'Access);
      Caller.Add_Test (Suite, "Test ASF.Helpers.Beans.Get_Bean",
                       Test_Get_Bean_Helper'Access);
   end Add_Tests;

   --  ------------------------------
   --  Setup the faces context for the unit test.
   --  ------------------------------
   procedure Setup (T       : in out Test;
                    Context : in out Faces_Context) is
   begin
      T.Form          := new ASF.Applications.Tests.Form_Bean;
      T.ELContext     := new EL.Contexts.Default.Default_Context;
      T.Root_Resolver := new EL.Contexts.Default.Default_ELResolver;
      T.Variables     := new EL.Variables.Default.Default_Variable_Mapper;
      T.ELContext.Set_Resolver (T.Root_Resolver.all'Access);
      T.ELContext.Set_Variable_Mapper (T.Variables.all'Access);
      Context.Set_ELContext (T.ELContext.all'Access);

      T.Root_Resolver.Register (Ada.Strings.Unbounded.To_Unbounded_String ("dumbledore"),
                                EL.Objects.To_Object (String '("albus")));

      T.Root_Resolver.Register (Ada.Strings.Unbounded.To_Unbounded_String ("potter"),
                                EL.Objects.To_Object (String '("harry")));
      T.Root_Resolver.Register (Ada.Strings.Unbounded.To_Unbounded_String ("hogwarts"),
                                EL.Objects.To_Object (T.Form.all'Access,
                                                      EL.Objects.STATIC));
   end Setup;

   --  ------------------------------
   --  Cleanup the test instance.
   --  ------------------------------
   overriding
   procedure Tear_Down (T : in out Test) is
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Contexts.Default.Default_Context'Class,
                                        EL.Contexts.Default.Default_Context_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Variables.Variable_Mapper'Class,
                                        EL.Variables.Variable_Mapper_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (EL.Contexts.Default.Default_ELResolver'Class,
                                        EL.Contexts.Default.Default_ELResolver_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (ASF.Applications.Tests.Form_Bean'Class,
                                        ASF.Applications.Tests.Form_Bean_Access);

   begin
      Free (T.ELContext);
      Free (T.Variables);
      Free (T.Root_Resolver);
      Free (T.Form);
   end Tear_Down;

   --  ------------------------------
   --  Test getting an attribute from the faces context.
   --  ------------------------------
   procedure Test_Get_Attribute (T : in out Test) is
      Ctx   : Faces_Context;
      Name  : EL.Objects.Object;
   begin
      T.Setup (Ctx);

      Name := Ctx.Get_Attribute ("dumbledore");
      T.Assert (not EL.Objects.Is_Null (Name), "Null attribute returned");
      Util.Tests.Assert_Equals (T, "albus", EL.Objects.To_String (Name), "Invalid attribute");

      Name := Ctx.Get_Attribute ("potter");
      T.Assert (not EL.Objects.Is_Null (Name), "Null attribute returned");
      Util.Tests.Assert_Equals (T, "harry", EL.Objects.To_String (Name), "Invalid attribute");

      Name := Ctx.Get_Attribute ("voldemort");
      T.Assert (EL.Objects.Is_Null (Name), "Oops... is there any horcrux left?");

   end Test_Get_Attribute;

   --  ------------------------------
   --  Test getting a bean object from the faces context.
   --  ------------------------------
   procedure Test_Get_Bean (T : in out Test) is
      use type Util.Beans.Basic.Readonly_Bean_Access;

      Ctx   : Faces_Context;
      Bean  : Util.Beans.Basic.Readonly_Bean_Access;
   begin
      T.Setup (Ctx);

      Bean := Ctx.Get_Bean ("dumbledore");
      T.Assert (Bean = null, "Dumbledore should not be a bean");

      Bean := Ctx.Get_Bean ("hogwarts");
      T.Assert (Bean /= null, "hogwarts should be a bean");
   end Test_Get_Bean;

   --  ------------------------------
   --  Test getting a bean object from the faces context and doing a conversion.
   --  ------------------------------
   procedure Test_Get_Bean_Helper (T : in out Test) is
      use type ASF.Applications.Tests.Form_Bean_Access;

      Ctx   : aliased Faces_Context;
      Bean  : ASF.Applications.Tests.Form_Bean_Access;
   begin
      T.Setup (Ctx);

      Bean := Get_Form_Bean ("hogwarts");
      T.Assert (Bean = null, "A bean was found while the faces context does not exist!");

      ASF.Contexts.Faces.Set_Current (Ctx'Unchecked_Access, null);
      Bean := Get_Form_Bean ("hogwarts");
      T.Assert (Bean /= null, "hogwarts should be a bean");

      Bean := Get_Form_Bean ("dumbledore");
      T.Assert (Bean = null, "Dumbledore should not be a bean");

      ASF.Contexts.Faces.Restore (null);
   end Test_Get_Bean_Helper;

   --  ------------------------------
   --  Test the faces message queue.
   --  ------------------------------
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

   --  ------------------------------
   --  Test adding some exception in the faces context.
   --  ------------------------------
   procedure Test_Queue_Exception (T : in out Test) is
      procedure Raise_Exception (Depth : in Natural;
                                 Excep : in Natural);
      procedure Check_Exception (Event   : in Events.Exceptions.Exception_Event'Class;
                                 Remove  : out Boolean;
                                 Context : in out Faces_Context'Class);

      Ctx : Faces_Context;
      Cnt : Natural := 0;

      procedure Raise_Exception (Depth : in Natural;
                                 Excep : in Natural) is
      begin
         if Depth > 0 then
            Raise_Exception (Depth - 1, Excep);
         end if;
         case Excep is
            when 1 =>
               raise Constraint_Error with "except code 1";

            when 2 =>
               raise Ada.IO_Exceptions.Name_Error;

            when others =>
               raise Program_Error with "Testing program error";
         end case;
      end Raise_Exception;

      procedure Check_Exception (Event   : in Events.Exceptions.Exception_Event'Class;
                                 Remove  : out Boolean;
                                 Context : in out Faces_Context'Class) is
         pragma Unreferenced (Context, Event);
      begin
         Cnt    := Cnt + 1;
         Remove := False;
      end Check_Exception;

   begin
      --  Create some exceptions and queue them.
      for I in 1 .. 3 loop
         begin
            Raise_Exception (3, I);
         exception
            when E : others =>
               Ctx.Queue_Exception (E);
         end;
      end loop;

      Ctx.Iterate_Exception (Check_Exception'Access);
      Util.Tests.Assert_Equals (T, 3, Cnt, "3 exception should have been queued");
   end Test_Queue_Exception;

   --  ------------------------------
   --  Test the flash instance.
   --  ------------------------------
   procedure Test_Flash_Context (T : in out Test) is
      use type ASF.Contexts.Faces.Flash_Context_Access;

      Ctx   : Faces_Context;
      Flash : aliased ASF.Contexts.Flash.Flash_Context;
   begin
      Ctx.Set_Flash (Flash'Unchecked_Access);

      T.Assert (Ctx.Get_Flash /= null, "Null flash context returned");
   end Test_Flash_Context;

end ASF.Contexts.Faces.Tests;
