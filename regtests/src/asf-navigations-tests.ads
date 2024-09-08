-----------------------------------------------------------------------
--  asf-navigations-tests -  Tests for ASF navigation
--  Copyright (C) 2013, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package ASF.Navigations.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Initialize the test application
   overriding
   procedure Set_Up (T : in out Test);

   --  Test a form navigation with an exact match (view, outcome, action).
   procedure Test_Exact_Navigation (T : in out Test);

   --  Test a form navigation with a partial match (view, outcome).
   procedure Test_Partial_Navigation (T : in out Test);

   --  Test a form navigation with a exception match (view, outcome).
   procedure Test_Exception_Navigation (T : in out Test);

   --  Test a form navigation with a wildcard match on the URI (view, outcome).
   procedure Test_Wildcard_Navigation (T : in out Test);

   --  Test a form navigation with a condition (view, outcome, condition).
   procedure Test_Conditional_Navigation (T : in out Test);

   --  Test a navigation rule with a status.
   procedure Test_Status_Navigation (T : in out Test);

   --  Check the navigation for an URI and expect the result to match the regular expression.
   procedure Check_Navigation (T          : in out Test;
                               Name       : in String;
                               Match      : in String;
                               Raise_Flag : in Boolean := False;
                               Status     : in Natural := 200);

end ASF.Navigations.Tests;
