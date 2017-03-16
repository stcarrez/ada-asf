-----------------------------------------------------------------------
--  asf-routes -- Request routing
--  Copyright (C) 2015, 2016, 2017 Stephane Carrez
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

with Ada.Finalization;

with Util.Beans.Basic;
with Util.Refs;
with EL.Expressions;
with EL.Contexts;

package ASF.Routes is

   type Route_Type is abstract new Util.Refs.Ref_Entity with null record;
   type Route_Type_Access is access all Route_Type'Class;

   --  function Duplicate (Route : in Route_Type) return Route_Type_Access is abstract;

   package Route_Type_Refs is
     new Util.Refs.Indefinite_References (Element_Type => Route_Type'Class,
                                          Element_Access => Route_Type_Access);

   subtype Route_Type_Ref is Route_Type_Refs.Ref;
   --  subtype Route_Type_Access is Route_Type_Refs.Element_Access;

   No_Parameter : exception;

   type Path_Mode is (FULL, PREFIX, SUFFIX);

   --  The <tt>Route_Context_Type</tt> defines the context information after a path
   --  has been routed.
   type Route_Context_Type is tagged limited private;

   --  Get path information after the routing.
   function Get_Path (Context : in Route_Context_Type;
                      Mode    : in Path_Mode := FULL) return String;

   --  Get the path parameter value for the given parameter index.
   --  The <tt>No_Parameter</tt> exception is raised if the parameter does not exist.
   function Get_Parameter (Context : in Route_Context_Type;
                           Index   : in Positive) return String;

   --  Get the number of path parameters that were extracted for the route.
   function Get_Parameter_Count (Context : in Route_Context_Type) return Natural;

   --  Return the position of the variable part of the path.
   --  If the URI matches a wildcard pattern, the position of the last '/' in the wildcard pattern
   --  is returned.
   function Get_Path_Pos (Context : in Route_Context_Type) return Natural;

   --  Return the route associated with the resolved route context.
   function Get_Route (Context : in Route_Context_Type) return Route_Type_Access;

   --  Change the context to use a new route.
   procedure Change_Route (Context : in out Route_Context_Type;
                           To      : in Route_Type_Access);

   --  Inject the parameters that have been extracted from the path according
   --  to the selected route.
   procedure Inject_Parameters (Context   : in Route_Context_Type;
                                Into      : in out Util.Beans.Basic.Bean'Class;
                                ELContext : in EL.Contexts.ELContext'Class);

   --  The <tt>Router_Type</tt> registers the different routes with their rules and
   --  resolves a path into a route context information.
   type Router_Type is new Ada.Finalization.Limited_Controlled with private;
   type Router_Type_Access is access all Router_Type'Class;

   --  Add a route associated with the given path pattern.  The pattern is split into components.
   --  Some path components can be a fixed string (/home) and others can be variable.
   --  When a path component is variable, the value can be retrieved from the route context.
   --  Once the route path is created, the <tt>Process</tt> procedure is called with the route
   --  reference.
   procedure Add_Route (Router    : in out Router_Type;
                        Pattern   : in String;
                        ELContext : in EL.Contexts.ELContext'Class;
                        Process   : not null access procedure (Route : in out Route_Type_Ref));

   --  Build the route context from the given path by looking at the different routes registered
   --  in the router with <tt>Add_Route</tt>.
   procedure Find_Route (Router  : in Router_Type;
                         Path    : in String;
                         Context : in out Route_Context_Type'Class);

   --  Walk the routes that have been added by <tt>Add_Route</tt> and call the <tt>Process</tt>
   --  procedure with each path pattern and route object.
   procedure Iterate (Router  : in Router_Type;
                      Process : not null access procedure (Pattern : in String;
                                                           Route   : in Route_Type_Access));

private

   type String_Access is access all String;

   type Route_Node_Type;
   type Route_Node_Access is access all Route_Node_Type'Class;

   --  Describes a variable path component whose value must be injected in an Ada bean.
   type Route_Param_Type is limited record
      Route : Route_Node_Access;
      First : Natural := 0;
      Last  : Natural := 0;
   end record;

   type Route_Param_Array is array (Positive range <>) of Route_Param_Type;

   type Route_Match_Type is (NO_MATCH, MAYBE_MATCH, WILDCARD_MATCH, EXT_MATCH, YES_MATCH);

   type Route_Node_Type is abstract tagged limited record
      Next_Route : Route_Node_Access;
      Children   : Route_Node_Access;
      Route      : Route_Type_Ref;
   end record;

   --  Inject the parameter that was extracted from the path.
   procedure Inject_Parameter (Node      : in Route_Node_Type;
                               Param     : in String;
                               Into      : in out Util.Beans.Basic.Bean'Class;
                               ELContext : in EL.Contexts.ELContext'Class) is null;

   --  Check if the route node accepts the given path component.
   function Matches (Node    : in Route_Node_Type;
                     Name    : in String;
                     Is_Last : in Boolean) return Route_Match_Type is abstract;

   --  Return the component path pattern that this route node represents.
   --  Example: 'index.html', '#{user.id}', ':id'
   function Get_Pattern (Node : in Route_Node_Type) return String is abstract;

   --  Return the position of the variable part of the path.
   --  If the URI matches a wildcard pattern, the position of the last '/' in the wildcard pattern
   --  is returned.
   function Get_Path_Pos (Node  : in Route_Node_Type;
                          Param : in Route_Param_Type) return Natural;

   --  Find recursively a match on the given route sub-tree.  The match must start at the position
   --  <tt>First</tt> in the path up to the last path position.  While the path components are
   --  checked, the route context is populated with variable components.  When the full path
   --  matches, <tt>YES_MATCH</tt> is returned in the context gets the route instance.
   procedure Find_Match (Node    : in Route_Node_Type;
                         Path    : in String;
                         First   : in Natural;
                         Match   : out Route_Match_Type;
                         Context : in out Route_Context_Type'Class);

   --  Walk the routes that have been added by <tt>Add_Route</tt> and call the <tt>Process</tt>
   --  procedure with each path pattern and route object.
   procedure Iterate (Node    : in Route_Node_Type;
                      Path    : in String;
                      Process : not null access procedure (Pattern : in String;
                                                           Route   : in Route_Type_Access));

   --  A fixed path component identification.
   type Path_Node_Type (Len : Natural) is new Route_Node_Type with record
      Name : aliased String (1 .. Len);
   end record;
   type Path_Node_Access is access all Path_Node_Type'Class;

   --  Check if the route node accepts the given path component.
   --  Returns YES_MATCH if the name corresponds exactly to the node's name.
   overriding
   function Matches (Node    : in Path_Node_Type;
                     Name    : in String;
                     Is_Last : in Boolean) return Route_Match_Type;

   --  Return the component path pattern that this route node represents (ie, 'Name').
   overriding
   function Get_Pattern (Node : in Path_Node_Type) return String;

   --  A variable path component whose value is injected in an Ada bean using the EL expression.
   --  The route node is created each time an EL expression is found in the route pattern.
   --  Example: /home/#{user.id}/index.html
   --  In this example, the EL expression refers to <tt>user.id</tt>.
   type EL_Node_Type is new Route_Node_Type with record
      Value : EL.Expressions.Value_Expression;
   end record;
   type EL_Node_Access is access all EL_Node_Type'Class;

   --  Check if the route node accepts the given path component.
   --  Returns MAYBE_MATCH.
   overriding
   function Matches (Node    : in EL_Node_Type;
                     Name    : in String;
                     Is_Last : in Boolean) return Route_Match_Type;

   --  Return the component path pattern that this route node represents (ie, the EL expr).
   overriding
   function Get_Pattern (Node : in EL_Node_Type) return String;

   --  Inject the parameter that was extracted from the path.
   overriding
   procedure Inject_Parameter (Node      : in EL_Node_Type;
                               Param     : in String;
                               Into      : in out Util.Beans.Basic.Bean'Class;
                               ELContext : in EL.Contexts.ELContext'Class);

   --  A variable path component which can be injected in an Ada bean.
   --  Example: /home/:id/view.html
   --  The path component represented by <tt>:id</tt> is injected in the Ada bean object
   --  passed to the <tt>Inject_Parameters</tt> procedure.
   type Param_Node_Type (Len : Natural) is new Route_Node_Type with record
      Name : String (1 .. Len);
   end record;
   type Param_Node_Access is access all Param_Node_Type'Class;

   --  Check if the route node accepts the given path component.
   --  Returns MAYBE_MATCH.
   overriding
   function Matches (Node    : in Param_Node_Type;
                     Name    : in String;
                     Is_Last : in Boolean) return Route_Match_Type;

   --  Return the component path pattern that this route node represents (ie, Name).
   overriding
   function Get_Pattern (Node : in Param_Node_Type) return String;

   --  Inject the parameter that was extracted from the path.
   overriding
   procedure Inject_Parameter (Node      : in Param_Node_Type;
                               Param     : in String;
                               Into      : in out Util.Beans.Basic.Bean'Class;
                               ELContext : in EL.Contexts.ELContext'Class);

   type Extension_Node_Type (Len : Natural) is new Route_Node_Type with record
      Ext : String (1 .. Len);
   end record;
   type Extension_Node_Access is access all Extension_Node_Type'Class;

   --  Check if the route node accepts the given extension.
   --  Returns MAYBE_MATCH.
   overriding
   function Matches (Node    : in Extension_Node_Type;
                     Name    : in String;
                     Is_Last : in Boolean) return Route_Match_Type;

   --  Return the component path pattern that this route node represents (ie, *.Ext).
   overriding
   function Get_Pattern (Node : in Extension_Node_Type) return String;

   type Wildcard_Node_Type is new Route_Node_Type with null record;
   type Wildcard_Node_Access is access all Wildcard_Node_Type'Class;

   --  Check if the route node accepts the given extension.
   --  Returns WILDCARD_MATCH.
   overriding
   function Matches (Node    : in Wildcard_Node_Type;
                     Name    : in String;
                     Is_Last : in Boolean) return Route_Match_Type;

   --  Return the component path pattern that this route node represents (ie, *).
   overriding
   function Get_Pattern (Node : in Wildcard_Node_Type) return String;

   --  Return the position of the variable part of the path.
   --  If the URI matches a wildcard pattern, the position of the last '/' in the wildcard pattern
   --  is returned.
   overriding
   function Get_Path_Pos (Node  : in Wildcard_Node_Type;
                          Param : in Route_Param_Type) return Natural;

   MAX_ROUTE_PARAMS : constant Positive := 10;

   type Route_Context_Type is limited new Ada.Finalization.Limited_Controlled with record
      Route  : Route_Type_Access;
      Path   : String_Access;
      Params : Route_Param_Array (1 .. MAX_ROUTE_PARAMS);
      Count  : Natural := 0;
   end record;

   --  Release the storage held by the route context.
   overriding
   procedure Finalize (Context : in out Route_Context_Type);

   type Router_Type is new Ada.Finalization.Limited_Controlled with record
      Route : aliased Path_Node_Type (Len => 0);
   end record;

   --  Release the storage held by the router.
   overriding
   procedure Finalize (Router : in out Router_Type);

   --  Insert the route node at the correct place in the children list
   --  according to the rule kind.
   procedure Insert (Parent : in Route_Node_Access;
                     Node   : in Route_Node_Access;
                     Kind   : in Route_Match_Type);

end ASF.Routes;
