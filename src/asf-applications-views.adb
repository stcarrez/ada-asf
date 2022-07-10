-----------------------------------------------------------------------
--  applications -- Ada Web Application
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2018, 2020, 2021, 2022 Stephane Carrez
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

with ASF.Contexts.Facelets;
with ASF.Applications.Main;
with ASF.Components.Base;
with ASF.Components.Core;
with ASF.Components.Core.Views;
with ASF.Converters;
with ASF.Validators;
with EL.Objects;
package body ASF.Applications.Views is

   use ASF.Components;

   type Facelet_Context is new ASF.Contexts.Facelets.Facelet_Context with record
      Facelets    : access ASF.Views.Facelets.Facelet_Factory;
      Application : access ASF.Applications.Main.Application'Class;
   end record;

   --  Include the definition having the given name.
   overriding
   procedure Include_Facelet (Context : in out Facelet_Context;
                              Source  : in String;
                              Parent  : in Base.UIComponent_Access);

   overriding
   function Get_Converter (Context : in Facelet_Context;
                           Name    : in EL.Objects.Object)
                            return ASF.Converters.Converter_Access;

   overriding
   function Get_Validator (Context : in Facelet_Context;
                           Name    : in EL.Objects.Object)
                           return ASF.Validators.Validator_Access;

   --  Compose a URI path with two components.  Unlike the Ada.Directories.Compose,
   --  and Util.Files.Compose the path separator must be a URL path separator (ie, '/').
   --  ------------------------------
   function Compose (Directory : in String;
                     Name      : in String) return String;

   --  ------------------------------
   --  Include the definition having the given name.
   --  ------------------------------
   overriding
   procedure Include_Facelet (Context : in out Facelet_Context;
                              Source  : in String;
                              Parent  : in Base.UIComponent_Access) is

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
   --  Get a converter from a name.
   --  Returns the converter object or null if there is no converter.
   --  ------------------------------
   overriding
   function Get_Converter (Context : in Facelet_Context;
                           Name    : in EL.Objects.Object)
                            return ASF.Converters.Converter_Access is
   begin
      return Context.Application.Find (Name);
   end Get_Converter;

   --  ------------------------------
   --  Get a validator from a name.
   --  Returns the validator object or null if there is no validator.
   --  ------------------------------
   overriding
   function Get_Validator (Context : in Facelet_Context;
                           Name    : in EL.Objects.Object)
                           return ASF.Validators.Validator_Access is
   begin
      return Context.Application.Find_Validator (Name);
   end Get_Validator;

   --  ------------------------------
   --  Get the facelet name from the view name.
   --  ------------------------------
   function Get_Facelet_Name (Handler : in View_Handler;
                              Name    : in String) return String is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;

      Pos : constant Natural := Index (Name, ".", Ada.Strings.Backward);
   begin
      if Pos > 0 and then Handler.View_Ext = Name (Pos .. Name'Last) then
         return Name (Name'First .. Pos - 1) & To_String (Handler.File_Ext);

      elsif Pos > 0 and then Handler.File_Ext = Name (Pos .. Name'Last) then
         return Name;

      end if;
      return Name & To_String (Handler.File_Ext);
   end Get_Facelet_Name;

   --  ------------------------------
   --  Restore the view identified by the given name in the faces context
   --  and create the component tree representing that view.
   --  ------------------------------
   procedure Restore_View (Handler : in out View_Handler;
                           Name    : in String;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                           View    : out ASF.Components.Root.UIViewRoot;
                           Ignore  : in Boolean := False) is

      use ASF.Views;
      use Util.Locales;
      use type ASF.Components.Base.UIComponent_Access;

      Ctx       : Facelet_Context;
      Tree      : Facelets.Facelet;
      View_Name : constant String := Handler.Get_Facelet_Name (Name);
   begin
      Ctx.Facelets    := Handler.Facelets'Unchecked_Access;
      Ctx.Application := Context.Get_Application;
      Ctx.Set_ELContext (Context.Get_ELContext);
      Facelets.Find_Facelet (Factory => Handler.Facelets,
                             Name    => View_Name,
                             Context => Ctx,
                             Result  => Tree,
                             Ignore  => Ignore);

      --  If the view could not be found, do not report any error yet.
      --  The SC_NOT_FOUND response will be returned when rendering the response.
      if Facelets.Is_Null (Tree) then
         return;
      end if;

      --  Build the component tree for this request.
      declare
         Root : aliased Core.UIComponentBase;
         Node : Base.UIComponent_Access;
      begin
         Facelets.Build_View (View    => Tree,
                              Context => Ctx,
                              Root    => Root'Unchecked_Access);
         ASF.Components.Base.Steal_Root_Component (Root, Node);

         --  If there was some error while building the view, return now.
         --  The SC_NOT_FOUND response will also be returned when rendering the response.
         if Node = null then
            return;
         end if;
         ASF.Components.Root.Set_Root (View, Node, View_Name);
         if Context.Get_Locale = NULL_LOCALE then
            if Node.all in Core.Views.UIView'Class then
               Context.Set_Locale (Core.Views.UIView'Class (Node.all).Get_Locale (Context));
            else
               Context.Set_Locale (Handler.Calculate_Locale (Context));
            end if;
         end if;
      end;
   end Restore_View;

   --  ------------------------------
   --  Create a new UIViewRoot instance initialized from the context and with
   --  the view identifier.  If the view is a valid view, create the component tree
   --  representing that view.
   --  ------------------------------
   procedure Create_View (Handler : in out View_Handler;
                          Name    : in String;
                          Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                          View    : out ASF.Components.Root.UIViewRoot;
                          Ignore  : in Boolean := False) is
      Pos : constant Natural := Util.Strings.Rindex (Name, '.');
   begin
      if Pos > 0 then
         Handler.Restore_View (Name (Name'First .. Pos - 1), Context, View, Ignore);
      else
         Handler.Restore_View (Name, Context, View, Ignore);
      end if;
   end Create_View;

   --  ------------------------------
   --  Render the view represented by the component tree.  The view is
   --  rendered using the context.
   --  ------------------------------
   procedure Render_View (Handler : in out View_Handler;
                          Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                          View    : in ASF.Components.Root.UIViewRoot) is
      pragma Unreferenced (Handler);

      Root : constant access ASF.Components.Base.UIComponent'Class
        := ASF.Components.Root.Get_Root (View);
   begin
      if Root /= null then
         Root.Encode_All (Context);
      end if;
   end Render_View;

   --  ------------------------------
   --  Compute the locale that must be used according to the <b>Accept-Language</b> request
   --  header and the application supported locales.
   --  ------------------------------
   function Calculate_Locale (Handler : in View_Handler;
                              Context : in ASF.Contexts.Faces.Faces_Context'Class)
                              return Util.Locales.Locale is
      pragma Unreferenced (Handler);

      App : constant ASF.Contexts.Faces.Application_Access := Context.Get_Application;
   begin
      return App.Calculate_Locale (Context);
   end Calculate_Locale;

   --  ------------------------------
   --  Compose a URI path with two components.  Unlike the Ada.Directories.Compose,
   --  and Util.Files.Compose the path separator must be a URL path separator (ie, '/').
   --  ------------------------------
   function Compose (Directory : in String;
                     Name      : in String) return String is
   begin
      if Directory'Length = 0 then
         return Name;
      elsif Directory (Directory'Last) = '/' and then Name (Name'First) = '/' then
         return Directory & Name (Name'First + 1 .. Name'Last);
      elsif Directory (Directory'Last) = '/' or else Name (Name'First) = '/' then
         return Directory & Name;
      else
         return Directory & "/" & Name;
      end if;
   end Compose;

   --  ------------------------------
   --  Get the URL suitable for encoding and rendering the view specified by the <b>View</b>
   --  identifier.
   --  ------------------------------
   function Get_Action_URL (Handler : in View_Handler;
                            Context : in ASF.Contexts.Faces.Faces_Context'Class;
                            View    : in String) return String is
      use Ada.Strings.Unbounded;

      Pos          : constant Natural := Util.Strings.Rindex (View, '.');
      Context_Path : constant String := Context.Get_Request.Get_Context_Path;
   begin
      if Pos > 0 and then View (Pos .. View'Last) = Handler.File_Ext then
         return Compose (Context_Path,
                         View (View'First .. Pos - 1) & To_String (Handler.View_Ext));
      end if;
      if Pos > 0 and then View (Pos .. View'Last) = Handler.View_Ext then
         return Compose (Context_Path, View);
      end if;
      return Compose (Context_Path, View);
   end Get_Action_URL;

   --  ------------------------------
   --  Get the URL for redirecting the user to the specified view.
   --  ------------------------------
   function Get_Redirect_URL (Handler : in View_Handler;
                              Context : in ASF.Contexts.Faces.Faces_Context'Class;
                              View    : in String) return String is
      Pos : constant Natural := Util.Strings.Rindex (View, '?');
   begin
      if Pos > 0 then
         return Handler.Get_Action_URL (Context, View (View'First .. Pos - 1))
           & View (Pos .. View'Last);
      else
         return Handler.Get_Action_URL (Context, View);
      end if;
   end Get_Redirect_URL;

   --  ------------------------------
   --  Initialize the view handler.
   --  ------------------------------
   procedure Initialize (Handler    : out View_Handler;
                         Components : access ASF.Factory.Component_Factory;
                         Conf       : in Config) is
      use ASF.Views;
      use Ada.Strings.Unbounded;
   begin
      Handler.Paths    := Conf.Get (VIEW_DIR_PARAM);
      Handler.View_Ext := Conf.Get (VIEW_EXT_PARAM);
      Handler.File_Ext := Conf.Get (VIEW_FILE_EXT_PARAM);

      Facelets.Initialize (Factory => Handler.Facelets,
                           Components => Components,
                           Paths   => To_String (Handler.Paths),
                           Ignore_White_Spaces => Conf.Get (VIEW_IGNORE_WHITE_SPACES_PARAM),
                           Ignore_Empty_Lines  => Conf.Get (VIEW_IGNORE_EMPTY_LINES_PARAM),
                           Escape_Unknown_Tags => Conf.Get (VIEW_ESCAPE_UNKNOWN_TAGS_PARAM));

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

end ASF.Applications.Views;
