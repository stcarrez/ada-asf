-----------------------------------------------------------------------
--  asf-lifecycles-response -- Response phase
--  Copyright (C) 2010 Stephane Carrez
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

--  The <b>ASF.Lifecycles.Response</b> package defines the behavior
--  of the response phase.
package ASF.Lifecycles.Response is

   --  ------------------------------
   --  Response controller
   --  ------------------------------
   type Response_Controller is new Phase_Controller with null record;

   --  Execute the restore view phase.
   overriding
   procedure Execute (Controller : in Response_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Lifecycles.Response;
