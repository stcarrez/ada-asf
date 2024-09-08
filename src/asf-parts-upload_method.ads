-----------------------------------------------------------------------
--  asf-parts -- ASF Parts
--  Copyright (C) 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with EL.Methods.Proc_In;

package ASF.Parts.Upload_Method is
  new EL.Methods.Proc_In (Param1_Type => ASF.Parts.Part'Class);
