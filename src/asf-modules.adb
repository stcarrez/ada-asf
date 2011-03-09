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
with Ada.IO_Exceptions;

with Util.Files;
with Util.Log.Loggers;
package body ASF.Modules is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Modules");

   --  ------------------------------
   --  Receive an event from the event channel
   --  ------------------------------
   procedure Receive_Event (Sub  : in out Module_Subscriber;
                            Item : in Util.Events.Event'Class) is
   begin
      Sub.Module.Receive_Event (ASF.Events.Modules.Module_Event'Class (Item));
   end Receive_Event;

   --  ------------------------------
   --  Get the module name
   --  ------------------------------
   function Get_Name (Plugin : Module) return String is
   begin
      return Plugin.Name.all;
   end Get_Name;

   --  ------------------------------
   --  Get the base URI for this module
   --  ------------------------------
   function Get_URI (Plugin : Module) return String is
   begin
      return Plugin.URI.all;
   end Get_URI;

   --  ------------------------------
   --  Get the module configuration property identified by the name.
   --  If the configuration property does not exist, returns the default value.
   --  ------------------------------
   function Get_Config (Plugin  : Module;
                        Name    : String;
                        Default : String := "") return String is
   begin
      return Plugin.Config.Get (Name, Default);
   end Get_Config;

   --  ------------------------------
   --  Get the application.
   --  ------------------------------
   function Get_Application (Plugin : in Module)
                             return access ASF.Applications.Main.Application'Class is
   begin
      return Plugin.App;
   end Get_Application;

   --  ------------------------------
   --  Initialize the the module.  This procedure is called by the application when
   --  the module is registered in the application.
   --  ------------------------------
   procedure Initialize (Plugin : in out Module;
                         App    : access ASF.Applications.Main.Application'Class) is
   begin
      Plugin.App := App;
   end Initialize;

   --  ------------------------------
   --  Send the event to the module
   --  ------------------------------
   procedure Send_Event (Plugin  : in Module;
                         To      : in String;
                         Content : in ASF.Events.Modules.Module_Event'Class) is
      Target : constant Module_Access := Plugin.Find_Module (To);
   begin
      if Target = null then
         Log.Info ("Event sent to '{0}' has no recipient", To);
         return;
      end if;
      Target.Channel.Post (Content);
   end Send_Event;

   --  ------------------------------
   --  Receive an event sent by another module with <b>Send_Event</b> method.
   --  ------------------------------
   procedure Receive_Event (Plugin  : in out Module;
                            Content : in ASF.Events.Modules.Module_Event'Class) is
   begin
      null;
   end Receive_Event;

   --  ------------------------------
   --  Find the module with the given name
   --  ------------------------------
   function Find_Module (Plugin : Module;
                         Name   : String) return Module_Access is
   begin
      if Plugin.Registry = null then
         return null;
      end if;
      return Find_By_Name (Plugin.Registry.all, Name);
   end Find_Module;

   --  ------------------------------
   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
   --  ------------------------------
   procedure Register (Plugin  : in out Module;
                       Name    : in String;
                       Handler : in ASF.Beans.Create_Bean_Access;
                       Free    : in ASF.Beans.Free_Bean_Access := null;
                       Scope   : in ASF.Beans.Scope_Type := ASF.Beans.REQUEST_SCOPE) is
   begin
      ASF.Beans.Register (Plugin.Factory, Name, Handler, Free, Scope);
   end Register;

   --  ------------------------------
   --  Register under the given name a function to create the bean instance when
   --  it is accessed for a first time.  The scope defines the scope of the bean.
   --  bean
   --  ------------------------------
   procedure Register (Plugin : in out Module;
                       Name    : in String;
                       Bind    : in ASF.Beans.Binding_Access) is
   begin
      ASF.Beans.Register (Plugin.Factory, Name, Bind);
   end Register;

   --  ------------------------------
   --  Register all the definitions from the module into a main factory.
   --  This operation is called when the module is registered in the application.
   --  ------------------------------
   procedure Register_Factory (Plugin : in Module;
                               Into   : in out ASF.Beans.Bean_Factory) is
   begin
      ASF.Beans.Register (Into, Plugin.Factory);
   end Register_Factory;

   --  ------------------------------
   --  Initialize the registry
   --  ------------------------------
   procedure Initialize (Registry : in out Module_Registry;
                         Config   : in ASF.Applications.Config) is
   begin
      Registry.Config := Config;
   end Initialize;

   --  ------------------------------
   --  Register the module in the registry.
   --  ------------------------------
   procedure Register (Registry : in Module_Registry_Access;
                       Plugin   : in Module_Access;
                       Name     : in String;
                       URI      : in String) is
   begin
      Log.Info ("Register module '{0}' under URI '{1}'", Name, URI);

      if Plugin.Registry /= null then
         Log.Error ("Module '{0}' is already attached to a registry", Name);

         raise Program_Error with "Module '" & Name & "' already registered";
      end if;
      Plugin.Registry := Registry;
      Plugin.Name     := new String '(Name);
      Plugin.URI      := new String '(URI);
      Plugin.Registry.Name_Map.Insert (Plugin.Name, Plugin);
      if URI /= "" then
         Plugin.Registry.URI_Map.Insert (Plugin.URI, Plugin);
      end if;

      --  Load the module configuration file
      declare
         Base  : constant String := Name & ".properties";
         Paths : constant String := Registry.Config.Get ("app.modules.dir", "./config");
         Path  : constant String := Util.Files.Find_File_Path (Base, Paths);
      begin
         Plugin.Config.Load_Properties (Path);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log.Warn ("Module configuration file '{0}' does not exist", Path);
      end;

      --  Override the module configuration with the application configuration
      Plugin.Config.Copy (Registry.Config, Name & ".");

      --  Configure the event channels for this module
      declare
         Kind : constant String := Plugin.Config.Get (Name & ".channel.type", "direct");
      begin
         Plugin.Channel := Util.Events.Channels.Create (Name, Kind);
         Plugin.Channel.Subscribe (Plugin.Subscriber'Access);
      end;
   exception
      when Constraint_Error =>
         Log.Error ("Another module is already registered "
                    & "under name '{0}' or URI '{1}'", Name, URI);
         raise;
   end Register;

   --  ------------------------------
   --  Find the module with the given name
   --  ------------------------------
   function Find_By_Name (Registry : Module_Registry;
                          Name     : String) return Module_Access is
      Key : constant Name_Access := Name'Unrestricted_Access;
      Pos : constant Module_Maps.Cursor := Module_Maps.Find (Registry.Name_Map, Key);
   begin
      if Module_Maps.Has_Element (Pos) then
         return Module_Maps.Element (Pos);
      end if;
      return null;
   end Find_By_Name;

   --  ------------------------------
   --  Find the module mapped to a given URI
   --  ------------------------------
   function Find_By_URI (Registry : Module_Registry;
                         URI      : String) return Module_Access is
      Key : constant Name_Access := URI'Unrestricted_Access;
      Pos : constant Module_Maps.Cursor := Module_Maps.Find (Registry.URI_Map, Key);
   begin
      if Module_Maps.Has_Element (Pos) then
         return Module_Maps.Element (Pos);
      end if;
      return null;
   end Find_By_URI;

end ASF.Modules;
