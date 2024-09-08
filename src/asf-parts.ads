-----------------------------------------------------------------------
--  asf-parts -- ASF Parts
--  Copyright (C) 2011, 2012, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Servlet.Parts;

--  The <b>ASF.Parts</b> package is an Ada implementation of the Java servlet part
--  (JSR 315 3. The Request) provided by the <tt>javax.servlet.http.Part</tt> class.
package ASF.Parts is

   --  ------------------------------
   --  Multi part content
   --  ------------------------------
   --  The <b>Part</b> type describes a mime part received in a request.
   --  The content is stored in a file and several operations are provided
   --  to manage the content.
   subtype Part is Servlet.Parts.Part;

end ASF.Parts;
