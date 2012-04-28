-----------------------------------------------------------------------
--  asf-lifecycles -- Lifecycle
--  Copyright (C) 2010, 2011, 2012 Stephane Carrez
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

with Ada.Finalization;

with ASF.Events.Phases;
with ASF.Contexts.Faces;
with Util.Concurrent.Arrays;
limited with ASF.Applications.Main;
package ASF.Lifecycles is

   subtype Phase_Type is
     Events.Phases.Phase_Type range Events.Phases.RESTORE_VIEW .. Events.Phases.RENDER_RESPONSE;

   type Phase_Controller is abstract tagged limited private;
   type Phase_Controller_Access is access all Phase_Controller'Class;
   type Phase_Controller_Array is array (Phase_Type) of Phase_Controller_Access;

   --  Execute the phase.
   procedure Execute (Controller : in Phase_Controller;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is abstract;

   --  Initialize the phase controller.
   procedure Initialize (Controller : in out Phase_Controller;
                         App        : access ASF.Applications.Main.Application'Class) is null;

   type Lifecycle is abstract new Ada.Finalization.Limited_Controlled with private;
   type Lifecycle_Access is access all Lifecycle'Class;

   --  Creates the phase controllers by invoking the <b>Set_Controller</b>
   --  procedure for each phase.  This is called by <b>Initialize</b> to build
   --  the lifecycle handler.
   procedure Create_Phase_Controllers (Controller : in out Lifecycle) is abstract;

   --  Initialize the the lifecycle handler.
   procedure Initialize (Controller : in out Lifecycle;
                         App        : access ASF.Applications.Main.Application'Class);

   --  Finalize the lifecycle handler, freeing the allocated storage.
   overriding
   procedure Finalize (Controller : in out Lifecycle);

   --  Set the controller to be used for the given phase.
   procedure Set_Controller (Controller : in out Lifecycle;
                             Phase      : in Phase_Type;
                             Instance   : in Phase_Controller_Access);

   --  Register a bundle and bind it to a facelet variable.
   procedure Execute (Controller : in Lifecycle;
                      Context    : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Render the response by executing the response phase.
   procedure Render (Controller : in Lifecycle;
                     Context    : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Add a phase listener in the lifecycle controller.
   procedure Add_Phase_Listener (Controller : in out Lifecycle;
                                 Listener   : in ASF.Events.Phases.Phase_Listener_Access);

private
   type Phase_Controller is abstract tagged limited null record;

   use type ASF.Events.Phases.Phase_Listener_Access;

   --  Use the concurrent arrays package so that we can insert phase listeners while
   --  we also have the lifecycle manager which invokes listeners for the existing requests.
   package Listener_Vectors is
      new Util.Concurrent.Arrays (Element_Type => ASF.Events.Phases.Phase_Listener_Access);

   type Lifecycle is abstract new Ada.Finalization.Limited_Controlled with record
      Controllers : Phase_Controller_Array;
      Listeners   : Listener_Vectors.Vector;
   end record;

end ASF.Lifecycles;
