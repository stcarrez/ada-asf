-----------------------------------------------------------------------
--  asf-modules -- Application Module and module registry
--  Copyright (C) 2009, 2010 Stephane Carrez
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

with Util.Strings;
with Util.Events.Channels;

with ASF.Beans;
with ASF.Applications;
with ASF.Events.Modules;

private with Ada.Containers.Indefinite_Hashed_Maps;

--  The <b>ASF.Modules</b> package defines simple pluggable modules in
--  the web application.  A module is a software component that can be
--  integrated in the application during the startup or initialization
--  phase.
--
--  A module can be attached to a given URI under the application's URI.
--  The module will handle all requests below that URI.
--
--  Each module is associated with an event channel that allows other
--  modules to post easily events.
package ASF.Modules is

   type Name_Access is new Util.Strings.Name_Access;

   --  ------------------------------
   --  Module
   --  ------------------------------
   --  Binding definition.
   type Module is tagged limited private;
   type Module_Access is access all Module'Class;

   --  Get the module name
   function Get_Name (Plugin : Module) return String;

   --  Get the base URI for this module
   function Get_URI (Plugin : Module) return String;

   --  Find the module with the given name
   function Find_Module (Plugin : Module;
                         Name   : String) return Module_Access;

   --  Get the module configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   function Get_Config (Plugin  : Module;
                        Name    : String;
                        Default : String := "") return String;

   --  Send the event to the module.  The module identified by <b>To</b> is
   --  found and the event is posted on its event channel.
   procedure Send_Event (Plugin  : in Module;
                         To      : in String;
                         Content : in ASF.Events.Modules.Module_Event'Class);

   --  Receive an event sent by another module with <b>Send_Event</b> method.
   procedure Receive_Event (Plugin  : in out Module;
                            Content : in ASF.Events.Modules.Module_Event'Class);

   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
   procedure Register (Plugin  : in out Module;
                       Name    : in String;
                       Handler : in ASF.Beans.Create_Bean_Access;
                       Free    : in ASF.Beans.Free_Bean_Access := null;
                       Scope   : in ASF.Beans.Scope_Type := ASF.Beans.REQUEST_SCOPE);

   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
   procedure Register (Plugin : in out Module;
                       Name    : in String;
                       Bind    : in ASF.Beans.Binding_Access);

   --  Register all the definitions from the module into a main factory.
   --  This operation is called when the module is registered in the application.
   procedure Register_Factory (Plugin : in Module;
                               Into   : in out ASF.Beans.Bean_Factory);

   --  ------------------------------
   --  Module Registry
   --  ------------------------------
   --  The module registry maintains the list of available modules with
   --  operations to retrieve them either from a name or from the base URI.
   type Module_Registry is limited private;
   type Module_Registry_Access is access all Module_Registry;

   --  Initialize the registry
   procedure Initialize (Registry : in out Module_Registry;
                         Config   : in ASF.Applications.Config);

   --  Register the module in the registry.
   procedure Register (Registry : in Module_Registry_Access;
                       Plugin   : in Module_Access;
                       Name     : in String;
                       URI      : in String);

   --  Find the module with the given name
   function Find_By_Name (Registry : Module_Registry;
                          Name     : String) return Module_Access;

   --  Find the module mapped to a given URI
   function Find_By_URI (Registry : Module_Registry;
                         URI      : String) return Module_Access;

private

   use Util.Strings;

   --  Map to find a module from its name or its URI
   package Module_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => Name_Access,
                                            Element_Type    => Module_Access,
                                            Hash            => Hash,
                                            Equivalent_Keys => Equivalent_Keys);

   --  Event channel subscriber
   type Module_Subscriber is new Util.Events.Channels.Subscriber with record
      Module : Module_Access;
   end record;

   --  Receive an event from the event channel
   procedure Receive_Event (Sub  : in out Module_Subscriber;
                            Item : in Util.Events.Event'Class);

   type Module is tagged limited record
      Registry   : Module_Registry_Access;
      Subscriber : aliased Module_Subscriber;
      Channel    : Util.Events.Channels.Channel_Access;
      Name       : Name_Access;
      URI        : Name_Access;
      Config     : ASF.Applications.Config;
      Factory    : ASF.Beans.Bean_Factory;
   end record;

   type Module_Registry is limited record
      Config   : ASF.Applications.Config;
      Name_Map : Module_Maps.Map;
      URI_Map  : Module_Maps.Map;
   end record;

end ASF.Modules;
