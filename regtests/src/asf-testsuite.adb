-----------------------------------------------------------------------
--  ASF testsuite - Ada Server Faces Test suite
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with ASF.Contexts.Writer.Tests;
with ASF.Views.Facelets.Tests;
with ASF.Applications.Views.Tests;
with ASF.Applications.Main.Tests;
with ASF.Contexts.Faces.Tests;
with ASF.Applications.Tests;
with ASF.Lifecycles.Tests;
with ASF.Navigations.Tests;
with ASF.Models.Selects.Tests;
with ASF.Converters.Tests;

package body ASF.Testsuite is

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      ASF.Contexts.Writer.Tests.Add_Tests (Ret);
      ASF.Contexts.Faces.Tests.Add_Tests (Ret);
      ASF.Converters.Tests.Add_Tests (Ret);
      ASF.Models.Selects.Tests.Add_Tests (Ret);
      ASF.Views.Facelets.Tests.Add_Tests (Ret);
      ASF.Lifecycles.Tests.Add_Tests (Ret);
      ASF.Navigations.Tests.Add_Tests (Ret);
      ASF.Applications.Main.Tests.Add_Tests (Ret);
      ASF.Applications.Views.Tests.Add_Tests (Ret);

      --  Run the application tests at the end.
      ASF.Applications.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end ASF.Testsuite;
