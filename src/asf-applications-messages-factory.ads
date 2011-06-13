-----------------------------------------------------------------------
--  applications.messages-factory -- Application Message Factory
--  Copyright (C) 2011 Stephane Carrez
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

with ASF.Contexts.Faces;
with ASF.Utils;
package ASF.Applications.Messages.Factory is
--
--     function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
--                           Message_Id : in String) return Message;
--
--     function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
--                           Message_Id : in String;
--                           Param1     : in String) return Message;

   --  Build a message
   function Get_Message (Context    : in ASF.Contexts.Faces.Faces_Context'Class;
                         Message_Id : in String;
                         Args       : in ASF.Utils.Object_Array) return Message;

end ASF.Applications.Messages.Factory;
