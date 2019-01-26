-----------------------------------------------------------------------
--  asf-lifecycles-validation -- Validation phase
--  Copyright (C) 2010, 2019 Stephane Carrez
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
with Ada.Exceptions;
with ASF.Components.Root;
with ASF.Components.Base;
with Util.Log.Loggers;

--  The <b>ASF.Lifecycles.Validation</b> package defines the behavior
--  of the validation phase.
package body ASF.Lifecycles.Validation is

   use Ada.Exceptions;
   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Lifecycles.Validation");

   --  ------------------------------
   --  Execute the validation phase.
   --  ------------------------------
   overriding
   procedure Execute (Controller : in Validation_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (Controller);

      View : constant Components.Root.UIViewRoot := Context.Get_View_Root;
      Root : constant access Components.Base.UIComponent'Class := Components.Root.Get_Root (View);
   begin
      Root.Process_Validators (Context);

   exception
      when E : others =>
         Log.Error ("Error when running the validation phase {0}: {1}: {2}", "?",
                    Exception_Name (E), Exception_Message (E));
         raise;
   end Execute;

end ASF.Lifecycles.Validation;
