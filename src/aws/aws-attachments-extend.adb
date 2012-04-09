-----------------------------------------------------------------------
--  aws-attachments-extend -- ASF extensions for AWS attachments
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

with Ada.Strings.Unbounded;
package body AWS.Attachments.Extend is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Get the length of the data content.
   --  ------------------------------
   function Get_Length (E : in Element) return Natural is
   begin
      case E.Kind is
         when Data =>
            return E.Data.Length;

         when others =>
            return 0;
      end case;
   end Get_Length;

   --  ------------------------------
   --  Get the name of the attachement.
   --  ------------------------------
   function Get_Name (E : in Element) return String is
   begin
      case E.Kind is
         when Data =>
            return To_String (E.Data.Content_Id);

         when others =>
            return "";
      end case;
   end Get_Name;

end AWS.Attachments.Extend;