-----------------------------------------------------------------------
--  asf.beans -- Bean Registration and Factory
--  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2020, 2022 Stephane Carrez
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
with Util.Beans.Objects.Maps;
package body ASF.Beans is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Beans");

   --  ------------------------------
   --  Register under the name identified by <b>Name</b> the class instance <b>Class</b>.
   --  ------------------------------
   procedure Register_Class (Factory : in out Bean_Factory;
                             Name    : in String;
                             Class   : in Class_Binding_Access) is
   begin
      Log.Info ("Register bean class {0}", Name);

      Factory.Registry.Include (Name, Class_Binding_Ref.Create (Class));
   end Register_Class;

   --  ------------------------------
   --  Register under the name identified by <b>Name</b> a function to create a bean.
   --  This is a simplified class registration.
   --  ------------------------------
   procedure Register_Class (Factory : in out Bean_Factory;
                             Name    : in String;
                             Handler : in Create_Bean_Access) is
      Class : constant Default_Class_Binding_Access := new Default_Class_Binding;
   begin
      Class.Create := Handler;
      Register_Class (Factory, Name, Class.all'Access);
   end Register_Class;

   --  ------------------------------
   --  Register the bean identified by <b>Name</b> and associated with the class <b>Class</b>.
   --  The class must have been registered by using the <b>Register</b> class operation.
   --  The scope defines the scope of the bean.
   --  ------------------------------
   procedure Register (Factory : in out Bean_Factory;
                       Name    : in String;
                       Class   : in String;
                       Params  : in Parameter_Bean_Ref.Ref;
                       Scope   : in Scope_Type := REQUEST_SCOPE) is
   begin
      Log.Info ("Register bean '{0}' created by '{1}' in scope {2}",
                Name, Class, Scope_Type'Image (Scope));

      declare
         Pos     : constant Registry_Maps.Cursor := Factory.Registry.Find (Class);
         Binding : Bean_Binding;
      begin
         if not Registry_Maps.Has_Element (Pos) then
            Log.Error ("Class '{0}' does not exist.  Cannot register bean '{1}'",
                       Class, Name);
            return;
         end if;
         Binding.Create := Registry_Maps.Element (Pos);
         Binding.Scope  := Scope;
         Binding.Params := Params;
         Factory.Map.Include (Ada.Strings.Unbounded.To_Unbounded_String (Name), Binding);
      end;
   end Register;

   --  ------------------------------
   --  Register the bean identified by <b>Name</b> and associated with the class <b>Class</b>.
   --  The class must have been registered by using the <b>Register</b> class operation.
   --  The scope defines the scope of the bean.
   --  ------------------------------
   procedure Register (Factory : in out Bean_Factory;
                       Name    : in String;
                       Class   : in Class_Binding_Access;
                       Params  : in Parameter_Bean_Ref.Ref;
                       Scope   : in Scope_Type := REQUEST_SCOPE) is
      Binding : Bean_Binding;
   begin
      Log.Info ("Register bean '{0}' in scope {1}",
                Name, Scope_Type'Image (Scope));

      Binding.Create := Class_Binding_Ref.Create (Class);
      Binding.Scope  := Scope;
      Binding.Params := Params;
      Factory.Map.Include (Ada.Strings.Unbounded.To_Unbounded_String (Name), Binding);
   end Register;

   --  ------------------------------
   --  Register all the definitions from a factory to a main factory.
   --  ------------------------------
   procedure Register (Factory : in out Bean_Factory;
                       From    : in Bean_Factory) is
   begin
      declare
         Pos : Registry_Maps.Cursor := From.Registry.First;
      begin
         while Registry_Maps.Has_Element (Pos) loop
            Factory.Registry.Include (Key      => Registry_Maps.Key (Pos),
                                      New_Item => Registry_Maps.Element (Pos));
            Registry_Maps.Next (Pos);
         end loop;
      end;
      declare
         Pos : Bean_Maps.Cursor := Bean_Maps.First (From.Map);
      begin
         while Bean_Maps.Has_Element (Pos) loop
            Factory.Map.Include (Key      => Bean_Maps.Key (Pos),
                                 New_Item => Bean_Maps.Element (Pos));
            Bean_Maps.Next (Pos);
         end loop;
      end;
   end Register;

   --  ------------------------------
   --  Create a bean by using the create operation registered for the name
   --  ------------------------------
   procedure Create (Factory : in Bean_Factory;
                     Name    : in Unbounded_String;
                     Context : in EL.Contexts.ELContext'Class;
                     Result  : out Util.Beans.Basic.Readonly_Bean_Access;
                     Scope   : out Scope_Type) is
      use type Util.Beans.Basic.Readonly_Bean_Access;
      Pos : constant Bean_Maps.Cursor := Factory.Map.Find (Name);
   begin
      if Bean_Maps.Has_Element (Pos) then
         declare
            Binding : constant Bean_Binding := Bean_Maps.Element (Pos);
         begin
            Binding.Create.Value.Create (Name, Result);
            if Result /= null and then not Binding.Params.Is_Null then
               if Result.all in Util.Beans.Basic.Bean'Class then
                  EL.Beans.Initialize (Util.Beans.Basic.Bean'Class (Result.all),
                                       Binding.Params.Value.Params,
                                       Context);
               else
                  Log.Warn ("Bean {0} cannot be set with pre-defined properties as it does "
                            & "not implement the Bean interface", To_String (Name));
               end if;
            end if;
            Scope := Binding.Scope;
         end;
      else
         Result := null;
         Scope := ANY_SCOPE;
      end if;
   end Create;

   --  ------------------------------
   --  Create a bean by using the registered create function.
   --  ------------------------------
   overriding
   procedure Create (Factory : in Default_Class_Binding;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Result  : out Util.Beans.Basic.Readonly_Bean_Access) is
      pragma Unreferenced (Name);
   begin
      Result := Factory.Create.all;
   end Create;

   --  ------------------------------
   --  Create a map bean object that allows to associate name/value pairs in a bean.
   --  ------------------------------
   function Create_Map_Bean return Util.Beans.Basic.Readonly_Bean_Access is
   begin
      return new Util.Beans.Objects.Maps.Map_Bean;
   end Create_Map_Bean;

end ASF.Beans;
