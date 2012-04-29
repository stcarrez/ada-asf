-----------------------------------------------------------------------
--  applications-messages-utils -- Utilities for messages
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

with ASF.Applications.Messages.Vectors;
package ASF.Applications.Messages.Utils is

   --  Copy the messages refered to by the iterator <b>From</b> into the message
   --  list represented by <b>Into</b>.
   procedure Copy (Into : in out ASF.Applications.Messages.Vectors.Vector;
                   From : in ASF.Applications.Messages.Vectors.Cursor);

end ASF.Applications.Messages.Utils;
