-----------------------------------------------------------------------
--  asf-rest-tests - Unit tests for ASF.Rest and ASF.Servlets.Rest
--  Copyright (C) 2016 Stephane Carrez
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

with Ada.Strings.Unbounded;

with Util.Tests;
with Util.Beans.Objects;
with ASF.Tests;

package ASF.Rest.Tests is

   --  Test API with simple operations.
   procedure Simple_Get (Req    : in out ASF.Rest.Request'Class;
                         Reply  : in out ASF.Rest.Response'Class;
                         Stream : in out ASF.Rest.Output_Stream'Class);
   procedure Simple_Put (Req    : in out ASF.Rest.Request'Class;
                         Reply  : in out ASF.Rest.Response'Class;
                         Stream : in out ASF.Rest.Output_Stream'Class);
   procedure Simple_Post (Req    : in out ASF.Rest.Request'Class;
                          Reply  : in out ASF.Rest.Response'Class;
                          Stream : in out ASF.Rest.Output_Stream'Class);
   procedure Simple_Delete (Req    : in out ASF.Rest.Request'Class;
                            Reply  : in out ASF.Rest.Response'Class;
                            Stream : in out ASF.Rest.Output_Stream'Class);

   --  Test API with an object created for each request.
   type Test_API is record
      N : Natural := 0;
   end record;

   procedure Create (Data   : in out Test_API;
                     Req    : in out ASF.Rest.Request'Class;
                     Reply  : in out ASF.Rest.Response'Class;
                     Stream : in out ASF.Rest.Output_Stream'Class);

   procedure Update (Data   : in out Test_API;
                     Req    : in out ASF.Rest.Request'Class;
                     Reply  : in out ASF.Rest.Response'Class;
                     Stream : in out ASF.Rest.Output_Stream'Class);

   procedure Delete (Data   : in out Test_API;
                     Req    : in out ASF.Rest.Request'Class;
                     Reply  : in out ASF.Rest.Response'Class;
                     Stream : in out ASF.Rest.Output_Stream'Class);

   procedure List (Data   : in out Test_API;
                   Req    : in out ASF.Rest.Request'Class;
                   Reply  : in out ASF.Rest.Response'Class;
                   Stream : in out ASF.Rest.Output_Stream'Class);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test REST POST create operation
   procedure Test_Create (T : in out Test);

   --  Test REST GET operation
   procedure Test_Get (T : in out Test);

   --  Test REST PUT update operation
   procedure Test_Update (T : in out Test);

   --  Test REST DELETE delete operation
   procedure Test_Delete (T : in out Test);

   --  Test REST operation on invalid operation.
   procedure Test_Invalid (T : in out Test);

   procedure Test_Operation (T      : in out Test;
                             Method : in String;
                             URI    : in String;
                             Status : in Natural);

end ASF.Rest.Tests;
