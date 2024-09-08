-----------------------------------------------------------------------
--  ASF -- Ada Server Faces
--  Copyright (C) 2009, 2010, 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;
with ASF.Tests;
with ASF.Testsuite;
procedure ASF_Harness is

   procedure Harness is new Util.Tests.Harness (Suite  => ASF.Testsuite.Suite,
                                                Finish => ASF.Tests.Finish);

begin
   Harness ("asf-tests.xml");
end ASF_Harness;
