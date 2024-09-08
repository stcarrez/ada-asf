-----------------------------------------------------------------------
--  asf-navigations-redirect -- Navigator to redirect to another page
--  Copyright (C) 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with EL.Expressions;
with EL.Contexts;
package ASF.Navigations.Redirect is

   --  ------------------------------
   --  Redirect page navigator
   --  ------------------------------
   --  The <b>Redirect_Navigator</b> handles the redirection rule for the navigation.
   --  The redirection view can contain EL expressions that will be evaluated when the
   --  redirect rule is executed.
   type Redirect_Navigator is new Navigation_Case with private;
   type Redirect_Navigator_Access is access all Redirect_Navigator'Class;

   --  Get the redirection view.  Evaluate the EL expressions used in the view name.
   function Get_Redirection (Controller : in Redirect_Navigator;
                             Context    : in ASF.Contexts.Faces.Faces_Context'Class)
                             return String;

   --  Navigate to the next page or action according to the controller's navigator.
   --  The navigator controller redirects the user to another page.
   overriding
   procedure Navigate (Controller : in Redirect_Navigator;
                       Context    : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Create a navigation case to redirect to another page.
   function Create_Redirect_Navigator (To_View : in String;
                                       Context : in EL.Contexts.ELContext'Class)
                                       return Navigation_Access;

private

   type Redirect_Navigator is new Navigation_Case with record
      View_Expr  : EL.Expressions.Expression;
   end record;

end ASF.Navigations.Redirect;
