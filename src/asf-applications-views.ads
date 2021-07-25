-----------------------------------------------------------------------
--  applications.views -- Ada Web Application
--  Copyright (C) 2009, 2010, 2011, 2012, 2021 Stephane Carrez
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

with Util.Locales;

with ASF.Components.Root;
with ASF.Contexts.Faces;
with ASF.Views.Facelets;
with ASF.Factory;
with Ada.Strings.Unbounded;
package ASF.Applications.Views is

   No_View : exception;

   type Local_Array_Access is access Util.Locales.Locale_Array;

   --  ------------------------------
   --  View Handler
   --  ------------------------------
   --  The view handler manages the component tree, the request processing
   --  life cycle and rendering the result view.
   type View_Handler is tagged limited private;
   type View_Handler_Access is access all View_Handler'Class;

   --  Initialize the view handler.
   procedure Initialize (Handler    : out View_Handler;
                         Components : access ASF.Factory.Component_Factory;
                         Conf       : in Config);

   --  Restore the view identified by the given name in the faces context
   --  and create the component tree representing that view.
   procedure Restore_View (Handler : in out View_Handler;
                           Name    : in String;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                           View    : out ASF.Components.Root.UIViewRoot;
                           Ignore  : in Boolean := False);

   --  Create a new UIViewRoot instance initialized from the context and with
   --  the view identifier.  If the view is a valid view, create the component tree
   --  representing that view.
   procedure Create_View (Handler : in out View_Handler;
                          Name    : in String;
                          Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                          View    : out ASF.Components.Root.UIViewRoot;
                          Ignore  : in Boolean := False);

   --  Render the view represented by the component tree.  The view is
   --  rendered using the context.
   procedure Render_View (Handler : in out View_Handler;
                          Context : in out ASF.Contexts.Faces.Faces_Context'Class;
                          View    : in ASF.Components.Root.UIViewRoot);

   --  Compute the locale that must be used according to the <b>Accept-Language</b> request
   --  header and the application supported locales.
   function Calculate_Locale (Handler : in View_Handler;
                              Context : in ASF.Contexts.Faces.Faces_Context'Class)
                              return Util.Locales.Locale;

   --  Get the URL suitable for encoding and rendering the view specified by the <b>View</b>
   --  identifier.
   function Get_Action_URL (Handler : in View_Handler;
                            Context : in ASF.Contexts.Faces.Faces_Context'Class;
                            View    : in String) return String;

   --  Get the URL for redirecting the user to the specified view.
   function Get_Redirect_URL (Handler : in View_Handler;
                              Context : in ASF.Contexts.Faces.Faces_Context'Class;
                              View    : in String) return String;

   --  Closes the view handler
   procedure Close (Handler : in out View_Handler);

   --  Set the extension mapping rule to find the facelet file from
   --  the name.
   procedure Set_Extension_Mapping (Handler : in out View_Handler;
                                    From    : in String;
                                    Into    : in String);

   --  Get the facelet name from the view name.
   function Get_Facelet_Name (Handler : in View_Handler;
                              Name    : in String) return String;

   --  Register a module
--     procedure Register_Module (Handler : in out View_Handler;
--                                Module  : in ASF.Modules.Module_Access);

private

   type View_Handler is tagged limited record
      Facelets  : aliased ASF.Views.Facelets.Facelet_Factory;
      Paths     : Ada.Strings.Unbounded.Unbounded_String;
      View_Ext  : Ada.Strings.Unbounded.Unbounded_String;
      File_Ext  : Ada.Strings.Unbounded.Unbounded_String;
      Locales   : Local_Array_Access := null;
   end record;

end ASF.Applications.Views;
