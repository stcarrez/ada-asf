-----------------------------------------------------------------------
--  applications-messages-utils -- Utilities for messages
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Applications.Messages.Vectors;
package ASF.Applications.Messages.Utils is

   --  Copy the messages refered to by the iterator <b>From</b> into the message
   --  list represented by <b>Into</b>.
   procedure Copy (Into : in out ASF.Applications.Messages.Vectors.Vector;
                   From : in ASF.Applications.Messages.Vectors.Cursor);

end ASF.Applications.Messages.Utils;
