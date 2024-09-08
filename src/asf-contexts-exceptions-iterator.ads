-----------------------------------------------------------------------
--  asf-contexts-exceptions-iterator -- Exception handlers in faces context
--  Copyright (C) 2011, 2021 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Contexts.Faces;

--  Iterate over the exception events which are in the queue and execute the given procedure.
--  The procedure should return True in <b>Remove</b> to indicate that the exception has been
--  processed.
--
--  Notes: this procedure should not be called directly (use ASF.Contexts.Faces.Iterate).
--  This procedure is separate to avoid circular dependency.
package ASF.Contexts.Exceptions.Iterator is

   procedure Iterate
     (Queue   : in out Exception_Queue;
      Context : in out ASF.Contexts.Faces.Faces_Context'Class;
      Process : not null access
        procedure (Event   : in ASF.Events.Exceptions.Exception_Event'Class;
                   Remove  : out Boolean;
                   Context : in out ASF.Contexts.Faces.Faces_Context'Class));

end ASF.Contexts.Exceptions.Iterator;
