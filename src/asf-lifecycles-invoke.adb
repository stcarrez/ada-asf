-----------------------------------------------------------------------
--  asf-lifecycles-invoke -- Invoke application phase
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
with Ada.Exceptions;
with ASF.Components.Root;
with ASF.Components.Base;
with ASF.Components.Core;
with Util.Log.Loggers;

--  The <b>ASF.Lifecycles.Invoke</b> package defines the behavior
--  of the invoke application phase.
package body ASF.Lifecycles.Invoke is

   use Ada.Exceptions;
   use Util.Log;
   use ASF.Components;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Lifecycles.Invoke");

   --  ------------------------------
   --  Execute the invoke application phase.
   --  ------------------------------
   overriding
   procedure Execute (Controller : in Invoke_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (Controller);

      View : constant Components.Root.UIViewRoot := Context.Get_View_Root;
      Root : constant access Components.Base.UIComponent'Class := Components.Root.Get_Root (View);
   begin
      if Root.all in Components.Core.UIView'Class then
         Components.Core.UIView'Class (Root.all).Process_Application (Context);
      end if;

   exception
      when E: others =>
         Log.Error ("Error when invoking application on view {0}: {1}: {2}", "?",
                    Exception_Name (E), Exception_Message (E));
         raise;
   end Execute;

end ASF.Lifecycles.Invoke;
