-----------------------------------------------------------------------
--  asf-navigations-redirect -- Navigator to redirect to another page
--  Copyright (C) 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Log.Loggers;
with Util.Beans.Objects;
package body ASF.Navigations.Redirect is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Navigations.Redirect");

   --  ------------------------------
   --  Get the redirection view.  Evaluate the EL expressions used in the view name.
   --  ------------------------------
   function Get_Redirection (Controller : in Redirect_Navigator;
                             Context    : in ASF.Contexts.Faces.Faces_Context'Class)
                             return String is
      Value : constant Util.Beans.Objects.Object
        := Controller.View_Expr.Get_Value (Context.Get_ELContext.all);
   begin
      if Util.Beans.Objects.Is_Null (Value) then
         Log.Error ("The redirection URI is null");
         return "";
      end if;
      return Util.Beans.Objects.To_String (Value);
   end Get_Redirection;

   --  ------------------------------
   --  Navigate to the next page or action according to the controller's navigator.
   --  The navigator controller redirects the user to another page.
   --  ------------------------------
   overriding
   procedure Navigate (Controller : in Redirect_Navigator;
                       Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      View : constant String := Controller.Get_Redirection (Context);
      URI  : constant String := Controller.View_Handler.Get_Redirect_URL (Context, View);
   begin
      Log.Debug ("Navigate by redirecting to {0}", URI);
      Context.Get_Response.Send_Redirect (Location => URI);
      Context.Response_Completed;
   end Navigate;

   --  ------------------------------
   --  Create a navigation case to redirect to another page.
   --  ------------------------------
   function Create_Redirect_Navigator (To_View : in String;
                                       Context : in EL.Contexts.ELContext'Class)
                                       return Navigation_Access is
      use EL.Expressions;

      Expr   : constant Expression := EL.Expressions.Create_Expression (To_View, Context);
      Result : constant Redirect_Navigator_Access := new Redirect_Navigator;
   begin
      Result.View_Expr := Expr;
      return Result.all'Access;
   end Create_Redirect_Navigator;

end ASF.Navigations.Redirect;
