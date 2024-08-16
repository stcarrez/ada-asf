-----------------------------------------------------------------------
--  Filters Tests - Unit tests for ASF.Filters
--  Copyright (C) 2015 Stephane Carrez
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
