-----------------------------------------------------------------------
--  Faces Context Tests - Unit tests for ASF.Contexts.Faces
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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

with Util.Tests;

with EL.Contexts.Default;
with EL.Variables.Default;

package ASF.Contexts.Faces.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with record
      ELContext      : aliased EL.Contexts.Default.Default_Context;
      Variables      : aliased EL.Variables.Default.Default_Variable_Mapper;
      Root_Resolver  : aliased EL.Contexts.Default.Default_ELResolver;
   end record;

   --  Setup the faces context for the unit test.
   procedure Setup (T       : in out Test;
                    Context : in out Faces_Context);

   --  Test getting an attribute from the faces context.
   procedure Test_Get_Attribute (T : in out Test);

   --  Test getting a bean object from the faces context.
   procedure Test_Get_Bean (T : in out Test);

   --  Test the faces message queue.
   procedure Test_Add_Message (T : in out Test);
   procedure Test_Max_Severity (T : in out Test);
   procedure Test_Get_Messages (T : in out Test);

   --  Test adding some exception in the faces context.
   procedure Test_Queue_Exception (T : in out Test);

   --  Test the flash instance.
   procedure Test_Flash_Context (T : in out Test);

end ASF.Contexts.Faces.Tests;
