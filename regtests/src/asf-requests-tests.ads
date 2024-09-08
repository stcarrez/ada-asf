-----------------------------------------------------------------------
--  asf-requests-tests - Unit tests for requests
--  Copyright (C) 2012, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package ASF.Requests.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the Split_Header procedure.
   procedure Test_Split_Header (T : in out Test);

   --  Test the Accept_Locales procedure.
   procedure Test_Accept_Locales (T : in out Test);

   --  Test the Set_Attribute procedure.
   procedure Test_Set_Attribute (T : in out Test);

end ASF.Requests.Tests;
