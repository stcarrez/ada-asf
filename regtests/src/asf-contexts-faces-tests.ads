-----------------------------------------------------------------------
--  Faces Context Tests - Unit tests for ASF.Contexts.Faces
--  Copyright (C) 2010, 2011, 2012, 2013, 2015 Stephane Carrez
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

with ASF.Tests;
with ASF.Applications.Tests;
with ASF.Helpers.Beans;

package ASF.Contexts.Faces.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   function Get_Form_Bean is
     new ASF.Helpers.Beans.Get_Bean (ASF.Applications.Tests.Form_Bean,
                                     ASF.Applications.Tests.Form_Bean_Access);

   type Test is new ASF.Tests.EL_Test with record
      --  The ELContext, Variables, Resolver, Form area controlled object.
      --  Due to AUnit implementation, we cannot store a controlled object in the Test object.
      --  This is due to the 'for Obj'Address use Ret;' clause used by AUnit to allocate
      --  a test object.
      --  The application object is allocated dyanmically by Set_Up.
      Form           : ASF.Applications.Tests.Form_Bean_Access;
   end record;

   --  Cleanup the test instance.
   overriding
   procedure Tear_Down (T : in out Test);

   --  Setup the faces context for the unit test.
   procedure Setup (T       : in out Test;
                    Context : in out Faces_Context);

   --  Test getting an attribute from the faces context.
   procedure Test_Get_Attribute (T : in out Test);

   --  Test getting a bean object from the faces context.
   procedure Test_Get_Bean (T : in out Test);

   --  Test getting a bean object from the faces context and doing a conversion.
   procedure Test_Get_Bean_Helper (T : in out Test);

   --  Test the faces message queue.
   procedure Test_Add_Message (T : in out Test);
   procedure Test_Max_Severity (T : in out Test);
   procedure Test_Get_Messages (T : in out Test);

   --  Test the application message factory for the creation of localized messages.
   procedure Test_Add_Localized_Message (T : in out Test);

   --  Test adding some exception in the faces context.
   procedure Test_Queue_Exception (T : in out Test);

   --  Test the flash instance.
   procedure Test_Flash_Context (T : in out Test);

   --  Test the mockup faces context.
   procedure Test_Mockup_Faces_Context (T : in out Test);

end ASF.Contexts.Faces.Tests;
