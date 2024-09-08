-----------------------------------------------------------------------
--  components-utils-flush -- Flush javascript queue and response
--  Copyright (C) 2011, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with ASF.Contexts.Writer;
package body ASF.Components.Utils.Flush is

   --  ------------------------------
   --  Flush the javascript queue
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UIFlush;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
      Writer : constant ASF.Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
   begin
      if UI.Get_Attribute (Name    => "response",
                           Context => Context,
                           Default => False)
      then
         Writer.Write_Scripts;
      else
         Writer.Flush;
      end if;
   end Encode_Begin;

end ASF.Components.Utils.Flush;
