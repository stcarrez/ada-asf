-----------------------------------------------------------------------
--  asf.beans -- Bean Registration and Factory
--  Copyright (C) 2009, 2010, 2011, 2015, 2022 Stephane Carrez
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

with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with Util.Refs;

with EL.Beans;
with EL.Contexts;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;

--  The <b>ASF.Beans</b> package is a registry for creating request, session
--  and application beans.
--
--  First, an application or a module registers in a class factory the class
--  of objects that can be created.  Each class is represented by a <b>Class_Binding</b>
--  interface that allows to create instances of the given class.  Each class
--  is associated with a unique name in the class factory (ie, the class name).
--  This step is done when the application or module is initialized.
--
--  Second, a set of application configuration files define the runtime bean objects
--  that can be created automatically when a request is processed.  Each runtime bean
--  object is associated with a bean name, a bean type identifying the class of object,
--  and a scope that identifies the lifespan of the object.
--
--  When a request is processed and a bean must be created, the bean factory is
--  searched to find a the object class and object scope (a <b>Bean_Binding</b>).
--  The <b>Class_Binding</b> associated with the <b>Bean_Binding</b> is then used
--  to create the object.
package ASF.Beans is

   --  Defines the scope of the bean instance.
   type Scope_Type is
     (
      --  Application scope means the bean is shared by all sessions and requests
      APPLICATION_SCOPE,

      --  Session scope means the bean is created one for each session.
      SESSION_SCOPE,

      --  Request scope means the bean is created for each request
      REQUEST_SCOPE,

      ANY_SCOPE);

   --  ------------------------------
   --  Class Binding
   --  ------------------------------
   --  The <b>Class_Binding</b> provides an operation to create objects of a given class.
   type Class_Binding is abstract new Util.Refs.Ref_Entity with null record;
   type Class_Binding_Access is access all Class_Binding'Class;

   procedure Create (Factory : in Class_Binding;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Result  : out Util.Beans.Basic.Readonly_Bean_Access) is abstract;

   --  Simplified bean creation.  A <b>Create_Bean_Access</b> function can be registered
   --  as a simplified class binding to create bean instances.
   type Create_Bean_Access is access function return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Bean initialization
   --  ------------------------------
   --  After a bean object is created, it can be initialized with a set of values defined
   --  by the <b>EL.Beans.Param_Value</b> type which holds the bean property name as well
   --  as an EL expression that will be evaluated to get the property value.
   type Parameter_Bean is new Util.Refs.Ref_Entity with record
      Params : EL.Beans.Param_Vectors.Vector;
   end record;
   type Parameter_Bean_Access is access all Parameter_Bean;

   package Parameter_Bean_Ref is
     new Util.Refs.Indefinite_References (Element_Type   => Parameter_Bean,
                                          Element_Access => Parameter_Bean_Access);

   --  ------------------------------
   --  Bean Factory
   --  ------------------------------
   --  The registry maintains a list of creation bindings which allow to create
   --  a bean object of a particular type.
   type Bean_Factory is limited private;

   --  Register under the name identified by <b>Name</b> the class instance <b>Class</b>.
   procedure Register_Class (Factory : in out Bean_Factory;
                             Name    : in String;
                             Class   : in Class_Binding_Access);

   --  Register under the name identified by <b>Name</b> a function to create a bean.
   --  This is a simplified class registration.
   procedure Register_Class (Factory : in out Bean_Factory;
                             Name    : in String;
                             Handler : in Create_Bean_Access);

   --  Register all the definitions from a factory to a main factory.
   procedure Register (Factory : in out Bean_Factory;
                       From    : in Bean_Factory);

   --  Register the bean identified by <b>Name</b> and associated with the class <b>Class</b>.
   --  The class must have been registered by using the <b>Register</b> class operation.
   --  The scope defines the scope of the bean.
   procedure Register (Factory : in out Bean_Factory;
                       Name    : in String;
                       Class   : in String;
                       Params  : in Parameter_Bean_Ref.Ref;
                       Scope   : in Scope_Type := REQUEST_SCOPE);

   --  Register the bean identified by <b>Name</b> and associated with the class <b>Class</b>.
   --  The class must have been registered by using the <b>Register</b> class operation.
   --  The scope defines the scope of the bean.
   procedure Register (Factory : in out Bean_Factory;
                       Name    : in String;
                       Class   : in Class_Binding_Access;
                       Params  : in Parameter_Bean_Ref.Ref;
                       Scope   : in Scope_Type := REQUEST_SCOPE);

   --  Create a bean by using the create operation registered for the name
   procedure Create (Factory : in Bean_Factory;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Context : in EL.Contexts.ELContext'Class;
                     Result  : out Util.Beans.Basic.Readonly_Bean_Access;
                     Scope   : out Scope_Type);

   --  Create a map bean object that allows to associate name/value pairs in a bean.
   function Create_Map_Bean return Util.Beans.Basic.Readonly_Bean_Access;

private

   use Ada.Strings.Unbounded;

   package Class_Binding_Ref is
     new Util.Refs.Indefinite_References (Element_Type   => Class_Binding'Class,
                                          Element_Access => Class_Binding_Access);

   package Registry_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type     => String,
                                                Element_Type => Class_Binding_Ref.Ref,
                                                Hash         => Ada.Strings.Hash,
                                                Equivalent_Keys => "=",
                                                "=" => Class_Binding_Ref."=");
   --  ------------------------------
   --  Default class binding record
   --  ------------------------------
   type Default_Class_Binding is new Class_Binding with record
      Create : Create_Bean_Access;
   end record;
   type Default_Class_Binding_Access is access all Default_Class_Binding'Class;

   --  Create a bean by using the registered create function.
   overriding
   procedure Create (Factory : in Default_Class_Binding;
                     Name    : in Ada.Strings.Unbounded.Unbounded_String;
                     Result  : out Util.Beans.Basic.Readonly_Bean_Access);

   type Bean_Binding is record
      Scope  : Scope_Type;
      Create : Class_Binding_Ref.Ref;
      Params : Parameter_Bean_Ref.Ref;
   end record;

   package Bean_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type     => Unbounded_String,
                                 Element_Type => Bean_Binding,
                                 Hash         => Ada.Strings.Unbounded.Hash,
                                 Equivalent_Keys => "=");

   type Bean_Factory is limited record
      Registry : Registry_Maps.Map;
      Map      : Bean_Maps.Map;
   end record;

end ASF.Beans;
