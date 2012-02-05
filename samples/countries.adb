-----------------------------------------------------------------------
--  countries - A simple bean example
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

with ASF.Models.Selects;
package body Countries is

   --  ------------------------------
   --  Get a select item list which contains a list of countries.
   --  ------------------------------
   function Create_Country_List return Util.Beans.Basic.Readonly_Bean_Access is
      use ASF.Models.Selects;

      Result : constant Select_Item_List_Access := new Select_Item_List;
   begin
      Result.Append (Label => "Andorra", Value => "AD");
      Result.Append (Label => "Belgium", Value => "BE");
      Result.Append (Label => "Canada", Value => "CA");
      Result.Append (Label => "Denmark", Value => "DK");
      Result.Append (Label => "Estonia", Value => "EE");
      Result.Append (Label => "France", Value => "fr");
      Result.Append (Label => "Germany", Value => "DE");
      Result.Append (Label => "Hungary", Value => "HU");
      Result.Append (Label => "Italy", Value => "IT");
      Result.Append (Label => "Japan", Value => "JP");
      Result.Append (Label => "Kosovo", Value => "XK");
      Result.Append (Label => "Luxembourg", Value => "LT");
      Result.Append (Label => "Martinique", Value => "MQ");
      Result.Append (Label => "Netherlands", Value => "NL");
      Result.Append (Label => "Oman", Value => "OM");
      Result.Append (Label => "Portugal", Value => "PT");
      Result.Append (Label => "Qatar", Value => "QA");
      Result.Append (Label => "Russia", Value => "RU");
      Result.Append (Label => "Spain", Value => "ES");
      Result.Append (Label => "Taiwan", Value => "TW");
      Result.Append (Label => "United States", Value => "US");
      Result.Append (Label => "Vietnam", Value => "VN");
      Result.Append (Label => "Western Sahara", Value => "EH");
      Result.Append (Label => "Yemen", Value => "YE");
      Result.Append (Label => "Zambia", Value => "ZM");
      return Result.all'Access;
   end Create_Country_List;

end Countries;
