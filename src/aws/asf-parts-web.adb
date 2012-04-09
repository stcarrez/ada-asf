-----------------------------------------------------------------------
--  asf-parts-web -- ASF Parts on top of AWS attachments
--  Copyright (C) 2011, 2012 Stephane Carrez
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

with AWS.Attachments.Extend;

package body ASF.Parts.Web is

   --  ------------------------------
   --  Get the size of the mime part.
   --  ------------------------------
   function Get_Size (Data : in AWS_Part) return Natural is
   begin
      return AWS.Attachments.Extend.Get_Length (Data.Element);
   end Get_Size;

   --  ------------------------------
   --  Get the content name submitted in the mime part.
   --  ------------------------------
   function Get_Name (Data : in AWS_Part) return String is
   begin
      return AWS.Attachments.Filename (Data.Element);
   end Get_Name;

   --  ------------------------------
   --  Get the path of the local file which contains the part.
   --  ------------------------------
   function Get_Local_Filename (Data : in AWS_Part) return String is
   begin
      return AWS.Attachments.Local_Filename (Data.Element);
   end Get_Local_Filename;

   --  ------------------------------
   --  Get the content type of the part.
   --  ------------------------------
   function Get_Content_Type (Data : in AWS_Part) return String is
   begin
      return AWS.Attachments.Content_Type (Data.Element);
   end Get_Content_Type;

   --  ------------------------------
   --  Build a part instance from the AWS attachment and execute the <b>Process</b> operation.
   --  ------------------------------
   procedure Process_Part (Part    : in AWS.Attachments.Element;
                           Process : not null access procedure (Part : in ASF.Parts.Part'Class)) is
      P : AWS_Part;
   begin
      P.Element := Part;
      Process (P);
   end Process_Part;

end ASF.Parts.Web;
