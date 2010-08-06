-----------------------------------------------------------------------
--  applications -- Ada Web Application
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
with Ada.Strings.Fixed;
with ASF.Components.Core.Factory;
with ASF.Components.Html.Factory;
with ASF.Components.Util.Factory;
with ASF.Views.Nodes.Core;
with ASF.Views.Nodes.Facelets;
with ASF.Contexts.Facelets;
package body ASF.Applications.Views is

   use ASF.Components;

   type Facelet_Context is new ASF.Contexts.Facelets.Facelet_Context with record
      Facelets : access ASF.Views.Facelets.Facelet_Factory;
   end record;

   --  Include the definition having the given name.
--     overriding
--     procedure Include_Definition (Context : in out Facelet_Context;
--                                   Name    : in Ada.Strings.Unbounded.Unbounded_String;
--                                   Parent  : in UIComponent_Access);

   --  Include the definition having the given name.
   overriding
   procedure Include_Facelet (Context : in out Facelet_Context;
                              Source  : in String;
                              Parent  : in UIComponent_Access);

   --  ------------------------------
   --  Include the definition having the given name.
   --  ------------------------------
--     overriding
--     procedure Include_Definition (Context : in out Facelet_Context;
--                                   Name    : in Ada.Strings.Unbounded.Unbounded_String;
--                                   Parent  : in UIComponent_Access) is
--
--        use ASF.Views;
--
--        Tree : Facelets.Facelet;
--     begin
--        Facelets.Find_Facelet (Factory => Context.Facelets.all,
--                               Name    => Ada.Strings.Unbounded.To_String (Name),
--                               Result  => Tree);
--
--        Facelets.Build_View (View    => Tree,
--                             Context => Context,
--                             Root    => Parent);
--     end Include_Definition;

   --  ------------------------------
   --  Include the definition having the given name.
   --  ------------------------------
   overriding
   procedure Include_Facelet (Context : in out Facelet_Context;
                              Source    : in String;
                              Parent  : in UIComponent_Access) is

      use ASF.Views;

      Path : constant String := Context.Resolve_Path (Source);
      Tree : Facelets.Facelet;
   begin
      Facelets.Find_Facelet (Factory => Context.Facelets.all,
                             Name    => Path,
                             Context => Context,
                             Result  => Tree);

      Facelets.Build_View (View    => Tree,
                           Context => Context,
                           Root    => Parent);
   end Include_Facelet;

   --  ------------------------------
   --  Get the facelet name from the view name.
   --  ------------------------------
   function Get_Facelet_Name (Handler : in View_Handler;
                              Name    : in String) return String is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;

      Pos : constant Natural := Index (Name, ".", Ada.Strings.Backward);
   begin
      if Pos > 0 and then To_String (Handler.View_Ext) = Name (Pos .. Name'Last) then
         return Name (Name'First .. Pos - 1) & To_String (Handler.File_Ext);
      end if;
      return Name & To_String (Handler.File_Ext);
   end Get_Facelet_Name;

   --  ------------------------------
   --  Set the current faces context before processing a view.
   --  ------------------------------
   procedure Set_Context (Handler : in out View_Handler;
                          Context : in ASF.Contexts.Faces.Faces_Context_Access) is
   begin
      Context.Get_ELContext.Set_Function_Mapper (Handler.Functions'Unchecked_Access);
      ASF.Contexts.Faces.Set_Current (Context);
   end Set_Context;

   --  ------------------------------
   --  Restore the view identified by the given name in the faces context
   --  and create the component tree representing that view.
   --  ------------------------------
   procedure Restore_View (Handler : in out View_Handler;
                           Name    : in String;
                           Context : in out ASF.Contexts.Faces.Faces_Context;
                           View    : out ASF.Components.Core.UIViewRoot) is

      use ASF.Views;

      Ctx       : Facelet_Context;
      Tree      : Facelets.Facelet;
      Root      : constant UIComponent_Access := new Core.UIComponentBase;
      View_Name : constant String := Handler.Get_Facelet_Name (Name);
   begin
      Ctx.Facelets := Handler.Facelets'Unchecked_Access;
      Ctx.Set_ELContext (Context.Get_ELContext);
      Facelets.Find_Facelet (Factory => Handler.Facelets,
                             Name    => View_Name,
                             Context => Ctx,
                             Result  => Tree);

      Facelets.Build_View (View => Tree, Context => Ctx, Root => Root);
      View.Set_Root (Root);
   end Restore_View;

   --  ------------------------------
   --  Render the view represented by the component tree.  The view is
   --  rendered using the context.
   --  ------------------------------
   procedure Render_View (Handler : in out View_Handler;
                          Context : in out ASF.Contexts.Faces.Faces_Context;
                          View    : in ASF.Components.Core.UIViewRoot) is
      pragma Unreferenced (Handler);
   begin
      View.Get_Root.Encode_All (Context);
   end Render_View;

   --  ------------------------------
   --  Initialize the view handler.
   --  ------------------------------
   procedure Initialize (Handler : out View_Handler;
                         Conf    : in Config) is
      use ASF.Views;
      use Ada.Strings.Unbounded;
   begin
      Facelets.Register (Factory  => Handler.Facelets,
                         Bindings => Core.Factory.Definition);
      Facelets.Register (Factory  => Handler.Facelets,
                         Bindings => Html.Factory.Definition);
      Facelets.Register (Factory  => Handler.Facelets,
                         Bindings => Nodes.Core.Definition);
      Facelets.Register (Factory  => Handler.Facelets,
                         Bindings => Nodes.Facelets.Definition);

      ASF.Components.Util.Factory.Set_Functions (Handler.Functions);

      Handler.Paths    := To_Unbounded_String (Conf.Get (VIEW_DIR_PARAM));
      Handler.View_Ext := To_Unbounded_String (Conf.Get (VIEW_EXT_PARAM));
      Handler.File_Ext := To_Unbounded_String (Conf.Get (VIEW_FILE_EXT_PARAM));

      Facelets.Set_Search_Directory (Factory => Handler.Facelets,
                                     Paths   => To_String (Handler.Paths));
   end Initialize;

   --  ------------------------------
   --  Closes the view handler
   --  ------------------------------
   procedure Close (Handler : in out View_Handler) is
      use ASF.Views;
   begin
      Facelets.Clear_Cache (Handler.Facelets);
   end Close;

   --  ------------------------------
   --  Set the extension mapping rule to find the facelet file from
   --  the name.
   --  ------------------------------
   procedure Set_Extension_Mapping (Handler : in out View_Handler;
                                    From    : in String;
                                    Into    : in String) is
      use Ada.Strings.Unbounded;
   begin
      Handler.View_Ext := To_Unbounded_String (From);
      Handler.File_Ext := To_Unbounded_String (Into);
   end Set_Extension_Mapping;

   --  ------------------------------
   --  Register a module
   --  ------------------------------
   procedure Register_Module (Handler : in out View_Handler;
                              Module  : in ASF.Modules.Module_Access) is
      use Ada.Strings.Unbounded;

      Name : constant String := Module.Get_Name;
      URI  : constant String := Module.Get_URI;
      Def  : constant String := To_String (Handler.Paths) & "/" & URI;
      Dir  : constant String := Module.Get_Config (Name & ".web.dir", Def);
   begin
      ASF.Views.Facelets.Register_Module (Handler.Facelets, URI, Dir);
   end Register_Module;

end ASF.Applications.Views;
