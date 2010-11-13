-----------------------------------------------------------------------
--  asf_volume_server -- The volume_server application with Ada Server Faces
--  Copyright (C) 2010 Stephane Carrez
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

with ASF.Server.Web;
with ASF.Servlets;
with ASF.Servlets.Faces;
with ASF.Filters.Dump;
with ASF.Applications;
with ASF.Applications.Main;
with Volume_Servlet;
with EL.Objects;
with EL.Beans;
procedure Asf_Volume_Server is

   type Compute_Bean is new EL.Beans.Bean with record
      Height : Float := -1.0;
      Radius : Float := -1.0;
   end record;

   --  Get the value identified by the name.
   function Get_Value (From : Compute_Bean;
                       Name : String) return EL.Objects.Object;

   --  Set the value identified by the name.
   procedure Set_Value (From  : in out Compute_Bean;
                        Name  : in String;
                        Value : in EL.Objects.Object);

   --  Get the value identified by the name.
   function Get_Value (From : Compute_Bean;
                       Name : String) return EL.Objects.Object is
   begin
      if Name = "radius" and From.Radius >= 0.0 then
         return EL.Objects.To_Object (From.Radius);

      elsif Name = "height" and From.Height >= 0.0 then
         return EL.Objects.To_Object (From.Height);

      else
         return EL.Objects.Null_Object;
      end if;
   end Get_Value;

   --  Set the value identified by the name.
   procedure Set_Value (From  : in out Compute_Bean;
                        Name  : in String;
                        Value : in EL.Objects.Object) is
   begin
      if Name = "radius" then
         From.Radius := EL.Objects.To_Float (Value);
      elsif Name = "height" then
         From.Height := EL.Objects.To_Float (Value);
      end if;
   end Set_Value;

   CONTEXT_PATH : constant String := "/volume";

   Compute : aliased Volume_Servlet.Servlet;
   App     : aliased ASF.Applications.Main.Application;
   Faces   : aliased ASF.Servlets.Faces.Faces_Servlet;
   Dump    : aliased ASF.Filters.Dump.Dump_Filter;
   Bean    : aliased Compute_Bean;
   WS      : ASF.Server.Web.AWS_Container;
   C       : ASF.Applications.Config;
begin
   Bean.Radius := 1.2;
   Bean.Height := 2.0;
   C.Set (ASF.Applications.VIEW_EXT, ".html");
   C.Set (ASF.Applications.VIEW_DIR, "samples/web");

   App.Initialize (C);
   App.Set_Global ("contextPath", CONTEXT_PATH);
   App.Set_Global ("compute", EL.Objects.To_Object (Bean'Unchecked_Access));

   --  Register the servlets and filters
   App.Add_Servlet (Name => "compute", Server => Faces'Unchecked_Access);
   App.Add_Filter (Name => "dump", Filter => Dump'Unchecked_Access);

   --  Define servlet mappings
   App.Add_Mapping (Name => "compute", Pattern => "*.html");
--     App.Add_Filter_Mapping (Name => "dump", Pattern => "*.html");

   WS.Register_Application (CONTEXT_PATH, App'Unchecked_Access);

   WS.Start;

   delay 60.0;

end Asf_Volume_Server;
