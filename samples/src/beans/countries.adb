-----------------------------------------------------------------------
--  countries - A simple bean example
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
