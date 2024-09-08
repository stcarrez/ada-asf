-----------------------------------------------------------------------
--  asf-lifecycles-invoke -- Invoke application phase
--  Copyright (C) 2010, 2011, 2012, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Exceptions;
with ASF.Components.Root;
with ASF.Components.Base;
with ASF.Components.Core.Views;
with Util.Log.Loggers;

--  The <b>ASF.Lifecycles.Invoke</b> package defines the behavior
--  of the invoke application phase.
package body ASF.Lifecycles.Invoke is

   use Ada.Exceptions;
   use Util.Log;

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

      procedure Process (Child : in Components.Base.UIComponent_Access);

      procedure Process (Child : in Components.Base.UIComponent_Access) is
      begin
         if Child.all in Components.Core.Views.UIView'Class then
            Components.Core.Views.UIView'Class (Child.all).Process_Application (Context);
         end if;
      end Process;

      procedure Process_Application_Children is new Components.Base.Iterate (Process);

   begin
      if Root.all in Components.Core.Views.UIView'Class then
         Process (Root.all'Unchecked_Access);
      else
         Process_Application_Children (Root.all);
      end if;

   exception
      when E : others =>
         Log.Error ("Error when invoking application on view {0}: {1}: {2}", "?",
                    Exception_Name (E), Exception_Message (E));
         raise;
   end Execute;

end ASF.Lifecycles.Invoke;
