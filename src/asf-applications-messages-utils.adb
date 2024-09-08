-----------------------------------------------------------------------
--  applications-messages-utils -- Utilities for messages
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body ASF.Applications.Messages.Utils is

   --  ------------------------------
   --  Copy the messages refered to by the iterator <b>From</b> into the message
   --  list represented by <b>Into</b>.
   --  ------------------------------
   procedure Copy (Into : in out ASF.Applications.Messages.Vectors.Vector;
                   From : in ASF.Applications.Messages.Vectors.Cursor) is
      procedure Copy (Item : in Message);

      procedure Copy (Item : in Message) is
      begin
         Into.Append (Item);
      end Copy;

      Iter : Vectors.Cursor := From;
   begin
      while Vectors.Has_Element (Iter) loop
         Vectors.Query_Element (Iter, Copy'Access);
         Vectors.Next (Iter);
      end loop;
   end Copy;

end ASF.Applications.Messages.Utils;
