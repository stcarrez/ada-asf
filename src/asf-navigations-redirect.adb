-----------------------------------------------------------------------
--  asf-navigations-redirect -- Navigator to redirect to another page
--  Copyright (C) 2010, 2011 Stephane Carrez
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
with Util.Log.Loggers;
package body ASF.Navigations.Redirect is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Navigations.Redirect");

   --  ------------------------------
   --  Navigate to the next page or action according to the controller's navigator.
   --  The navigator controller redirects the user to another page.
   --  ------------------------------
   overriding
   procedure Navigate (Controller : in Redirect_Navigator;
                       Context    : in out ASF.Contexts.Faces.Faces_Context'Class) is
      URI : constant String := To_String (Controller.View_Name);
   begin
      Log.Debug ("Navigate by redirecting to {0}", URI);
      Context.Get_Response.Send_Redirect (Location => URI);
      Context.Response_Completed;
   end Navigate;

   --  ------------------------------
   --  Create a navigation case to redirect to another page.
   --  ------------------------------
   function Create_Redirect_Navigator (To_View : in String) return Navigation_Access is
      Result : constant Redirect_Navigator_Access := new Redirect_Navigator;
   begin
      Result.View_Name := To_Unbounded_String (To_View);
      return Result.all'Access;
   end Create_Redirect_Navigator;

end ASF.Navigations.Redirect;
