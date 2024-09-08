-----------------------------------------------------------------------
--  Filters Tests - Unit tests for ASF.Filters
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body ASF.Filters.Tests is

   --  ------------------------------
   --  Increment the counter each time Do_Filter is called.
   --  ------------------------------
   procedure Do_Filter (F        : in Test_Filter;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class;
                        Chain    : in out ASF.Servlets.Filter_Chain) is
   begin
      F.Count.all := F.Count.all + 1;
      ASF.Servlets.Do_Filter (Chain => Chain, Request => Request, Response => Response);
   end Do_Filter;

   --  ------------------------------
   --  Initialize the test filter.
   --  ------------------------------
   overriding
   procedure Initialize (Server  : in out Test_Filter;
                         Config  : in ASF.Servlets.Filter_Config) is
      pragma Unreferenced (Config);
   begin
      Server.Count := Server.Counter'Unchecked_Access;
   end Initialize;

end ASF.Filters.Tests;
