-----------------------------------------------------------------------
--  applications.messages.vectors -- List of messages
--  Copyright (C) 2010 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Containers.Vectors;
package ASF.Applications.Messages.Vectors
  is new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Message);
