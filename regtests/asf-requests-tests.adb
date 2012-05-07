-----------------------------------------------------------------------
--  asf-requests-tests - Unit tests for requests
--  Copyright (C) 2012 Stephane Carrez
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

with Util.Test_Caller;
with Util.Log.Loggers;
with Util.Measures;
with ASF.Requests.Mockup;
package body ASF.Requests.Tests is

   use Util.Tests;
   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Requests.Tests");

   package Caller is new Util.Test_Caller (Test, "Requests");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test ASF.Requests.Split_Header",
                       Test_Split_Header'Access);
      Caller.Add_Test (Suite, "Test ASF.Requests.Accept_Locales",
                       Test_Accept_Locales'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the Split_Header procedure.
   --  ------------------------------
   procedure Test_Split_Header (T : in out Test) is
      procedure Process_Text (Item    : in String;
                              Quality : in Quality_Type);

      Count : Natural := 0;

      procedure Process_Text (Item    : in String;
                              Quality : in Quality_Type) is
      begin
         T.Assert (Item = "text/plain" or Item = "text/html" or Item = "text/x-dvi"
                   or Item = "text/x-c", "Invalid item: " & Item);
         T.Assert (Quality = 0.5 or Quality = 0.8 or Quality = 1.0,
                   "Invalid quality");
         if Item = "text/plain" then
            T.Assert (Quality = 0.5, "Invalid quality for " & Item);

         elsif Item = "text/x-dvi" or Item = "text/html" then
            T.Assert (Quality = 0.8, "Invalid quality for " & Item);

         else
            T.Assert (Quality = 1.0, "Invalid quality for " & Item);
         end if;
         Count := Count + 1;
      end Process_Text;

   begin
      Split_Header ("text/plain; q=0.5, text/html,text/x-dvi; q=0.8, text/x-c",
                    Process_Text'Access);
      Util.Tests.Assert_Equals (T, 4, Count, "Invalid number of items");
   end Test_Split_Header;

   --  ------------------------------
   --  Test the Accept_Locales procedure.
   --  ------------------------------
   procedure Test_Accept_Locales (T : in out Test) is
      procedure Process_Locale (Locale : in Util.Locales.Locale;
                                Quality : in Quality_Type);

      use Util.Locales;

      Req : ASF.Requests.Mockup.Request;

      Count : Natural := 0;

      procedure Process_Locale (Locale : in Util.Locales.Locale;
                                Quality : in Quality_Type) is
         Lang : constant String := Util.Locales.Get_Language (Locale);
      begin
         Log.Info ("Found locale: {0}", Util.Locales.To_String (Locale));

         T.Assert (Lang = "da" or Lang = "en_GB" or Lang = "en",
                   "Invalid lang: " & Lang);

         Count := Count + 1;
      end Process_Locale;

   begin
      Req.Set_Header ("Accept-Language", "da, en-gb;q=0.8, en;q=0.7");
      Req.Accept_Locales (Process_Locale'Access);

      Util.Tests.Assert_Equals (T, 3, Count, "Invalid number of calls");
   end Test_Accept_Locales;

end ASF.Requests.Tests;
