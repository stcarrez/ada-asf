-----------------------------------------------------------------------
--  asf-beans-injections -- Injection of parameters, headers, cookies in beans
--  Copyright (C) 2015 Stephane Carrez
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
with Util.Strings;
with ASF.Requests;
package ASF.Beans.Injections is

   type Inject_Type;

   --  The injection handler is a procedure that extracts the information from a request
   --  object and injects the value in the Ada bean instance.
   type Injection_Handler is not null access
     procedure (Bean       : in out Util.Beans.Basic.Bean'Class;
                Descriptor : in Inject_Type;
                Request    : in ASF.Requests.Request'Class);

   --  Define and control the injection of a value in some Ada bean.
   type Inject_Type is record
      --  The handler that retrieves the value from the request object and injects it in the bean.
      Handler : Injection_Handler;

      --  The bean property name in the Ada bean.
      Name    : Util.Strings.Name_Access;

      --  The HTTP header name, the query parameter name or the cookie name to inject.
      Param   : Util.Strings.Name_Access;

      --  The path component to inject.
      Pos     : Natural := 0;
   end record;

   type Inject_Array_Type is array (Positive) of Inject_Type;

   --  Inject the request header whose name is defined by Descriptor.Param.
   procedure Header (Bean       : in out Util.Beans.Basic.Bean'Class;
                     Descriptor : in Inject_Type;
                     Request    : in ASF.Requests.Request'Class);

   --  Inject the request query string parameter whose name is defined by Descriptor.Param.
   procedure Query_Param (Bean       : in out Util.Beans.Basic.Bean'Class;
                          Descriptor : in Inject_Type;
                          Request    : in ASF.Requests.Request'Class);

   --  Inject the request cookie whose name is defined by Descriptor.Param.
   procedure Cookie (Bean       : in out Util.Beans.Basic.Bean'Class;
                     Descriptor : in Inject_Type;
                     Request    : in ASF.Requests.Request'Class);

   --  Inject the request URI path component whose position is defined by Descriptor.Pos.
   procedure Path_Param (Bean       : in out Util.Beans.Basic.Bean'Class;
                         Descriptor : in Inject_Type;
                         Request    : in ASF.Requests.Request'Class);

   --  Inject into the Ada bean a set of information extracted from the request object.
   --  The value is obtained from a request header, a cookie, a query string parameter or
   --  from a URI path component.  The value is injected by using the bean operation
   --  <tt>Set_Value</tt>.
   procedure Inject (Into    : in out Util.Beans.Basic.Bean'Class;
                     List    : in Inject_Array_Type;
                     Request : in ASF.Requests.Request'Class);

end ASF.Beans.Injections;
