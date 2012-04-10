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

with AWS.Attachments;

--  The <b>ASF.Parts.Web</b> package implements ASF parts on top of AWS attachments.
package ASF.Parts.Web is

   --  ------------------------------
   --  Multi part content
   --  ------------------------------
   --  The <b>Part</b> type describes a mime part received in a request.
   --  The content is stored in a file and several operations are provided
   --  to manage the content.
   type AWS_Part is new Part with private;

   --  Get the size of the mime part.
   overriding
   function Get_Size (Data : in AWS_Part) return Natural;

   --  Get the content name submitted in the mime part.
   overriding
   function Get_Name (Data : in AWS_Part) return String;

   --  Get the path of the local file which contains the part.
   overriding
   function Get_Local_Filename (Data : in AWS_Part) return String;

   --  Get the content type of the part.
   overriding
   function Get_Content_Type (Data : in AWS_Part) return String;

   --  Build a part instance from the AWS attachment and execute the <b>Process</b> operation.
   procedure Process_Part (Part    : in AWS.Attachments.Element;
                           Process : not null access procedure (Part : in ASF.Parts.Part'Class));

private

   type AWS_Part is new Part with record
      Element      : AWS.Attachments.Element;
   end record;

end ASF.Parts.Web;
