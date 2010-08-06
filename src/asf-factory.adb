-----------------------------------------------------------------------
--  asf-factory -- Component and tag factory
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
with Util.Log.Loggers;
package body ASF.Factory is

   use Util.Log;

   --  The logger
   Log : constant Loggers.Logger := Loggers.Create ("ASF.Factory");

   --  ------------------------------
   --  Find the create function associated with the name.
   --  Returns null if there is no binding associated with the name.
   --  ------------------------------
   function Find (Factory : Factory_Bindings;
                  Name    : String) return Binding is
      Left  : Natural := Factory.Bindings'First;
      Right : Natural := Factory.Bindings'Last;
   begin
      while Left <= Right loop
         declare
            Pos  : constant Natural := (Left + Right + 1) / 2;
            Item : constant Name_Access := Factory.Bindings (Pos).Name;
         begin
            if Name = Item.all then
               return Factory.Bindings (Pos);
            elsif Name < Item.all then
               Right := Pos - 1;
            else
               Left := Pos + 1;
            end if;
         end;
      end loop;
      raise Unknown_Name;
   end Find;

   --  ------------------------------
   --  Check the definition of the component factory.
   --  ------------------------------
   procedure Check (Factory : in Factory_Bindings) is
      P : Name_Access := null;
   begin
      Log.Debug ("Check binding for {0}", Factory.URI.all);


      for I in Factory.Bindings'Range loop
         declare
            B : constant Binding := Factory.Bindings (I);
         begin
            if P /= null then
               if B.Name.all = P.all then
                  Log.Error ("In factory {0}, binding {1} registered twice",
                             Factory.URI.all, P.all);
                  raise Program_Error
                    with Factory.URI.all & ": binding '" & P.all & "' registered twice";
               end if;
               if B.Name.all < P.all then
                  Log.Error ("In factory {0}, binding {1} at position {2} "
                             & "is not at the good place", Factory.URI.all, P.all,
                             Natural'Image (I));
                  raise Program_Error
                    with Factory.URI.all & ": binding '" & B.Name.all
                      & "' at position " & Natural'Image (I) & " not at the good place";
               end if;
            end if;
            P := B.Name;
         end;
      end loop;
   end Check;

   --  ------------------------------
   --  Find the create function in bound to the name in the given URI namespace.
   --  Returns null if no such binding exist.
   --  ------------------------------
   function Find (Factory : Component_Factory;
                  URI     : String;
                  Name    : String) return Binding is
      NS  : aliased constant String := URI;
      Pos : constant Factory_Maps.Cursor := Factory.Map.Find (NS'Unchecked_Access);
   begin
      if not Factory_Maps.Has_Element (Pos) then
         raise Unknown_Name;
      end if;
      return Find (Factory => Factory_Maps.Element (Pos).all, Name => Name);
   end Find;

   --  ------------------------------
   --  Register a binding library in the factory
   --  ------------------------------
   procedure Register (Factory  : in out Component_Factory;
                       Bindings : in Factory_Bindings_Access) is
   begin
      Log.Info ("Register bindings: {0}", Bindings.URI.all);

      Check (Bindings.all);
      Factory.Map.Include (Bindings.URI, Bindings);
   end Register;

end ASF.Factory;
