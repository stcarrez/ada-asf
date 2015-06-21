-----------------------------------------------------------------------
--  asf-routes -- Request routing
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
with System.Address_Image;
with Ada.Unchecked_Deallocation;
with Util.Strings;
with Util.Beans.Objects;
with Util.Log.Loggers;

package body ASF.Routes is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("ASF.Routes");

   --  ------------------------------
   --  Get path information after the routing.
   --  ------------------------------
   function Get_Path_Info (Context : in Route_Context_Type) return String is
   begin
      if Context.Path = null then
         return "";
      else
         return Context.Path.all;
      end if;
   end Get_Path_Info;

   --  ------------------------------
   --  Return the route associated with the resolved route context.
   --  ------------------------------
   function Get_Route (Context : in Route_Context_Type) return Route_Type_Access is
   begin
      return Context.Route;
   end Get_Route;

   --  ------------------------------
   --  Inject the parameters that have been extracted from the path according
   --  to the selected route.
   --  ------------------------------
   procedure Inject_Parameters (Context   : in Route_Context_Type;
                                Into      : in out Util.Beans.Basic.Bean'Class;
                                ELContext : in EL.Contexts.ELContext'Class) is
   begin
      if Context.Count > 0 then
         Log.Debug ("Inject route parameters from {0}", Context.Path.all);
         for I in 1 .. Context.Count loop
            declare
               Param : Route_Param_Type renames Context.Params (I);
            begin
               Param.Route.Inject_Parameter (Context.Path (Param.First .. Param.Last),
                                             Into, ELContext);
            end;
         end loop;
      end if;
   end Inject_Parameters;

   --  ------------------------------
   --  Release the storage held by the route context.
   --  ------------------------------
   overriding
   procedure Finalize (Context : in out Route_Context_Type) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => String, Name => String_Access);
   begin
      Free (Context.Path);
   end Finalize;

   --  ------------------------------
   --  Add a route associated with the given path pattern.  The pattern is split into components.
   --  Some path components can be a fixed string (/home) and others can be variable.
   --  When a path component is variable, the value can be retrieved from the route context.
   --  ------------------------------
   procedure Add_Route (Router    : in out Router_Type;
                        Pattern   : in String;
                        To        : in Route_Type_Access;
                        ELContext : in EL.Contexts.ELContext'Class) is
      First    : Natural := Pattern'First;
      Pos      : Natural;
      Node     : Route_Node_Access := Router.Route.Children;
      Prev     : Route_Node_Access;
      Match    : Route_Match_Type := NO_MATCH;
      New_Path : Path_Node_Access;
      Parent   : Route_Node_Access := Router.Route'Unchecked_Access;
      Found    : Boolean;
   begin
      Log.Info ("Adding route {0} to {1}", Pattern, System.Address_Image (To.all'Address));
      loop
         --  Ignore consecutive '/'.
         while First <= Pattern'Last and then Pattern (First) = '/' loop
            First := First + 1;
         end loop;

         --  Find the path component's end.
         Pos := Util.Strings.Index (Pattern, '/', First);
         if Pos = 0 then
            Pos := Pattern'Last;
         else
            Pos := Pos - 1;
         end if;
         if Pattern (First) = '#' then
            declare
               E : EL_Node_Access;
            begin
               --  First := First + 1;
               Prev := null;
               Found := False;

               --  Find the EL_Node that have the same EL expression.
               while Node /= null loop
                  if Node.all in EL_Node_Type'Class then
                     E := EL_Node_Type'Class (Node.all)'Access;
                     if E.Value.Get_Expression = Pattern (First .. Pos) then
                        Parent := Node;
                        Node := Node.Children;
                        Found := True;
                        exit;
                     end if;
                  end if;
                  Prev := Node;
                  Node := Node.Next_Route;
               end loop;

               if not Found then
                  E := new EL_Node_Type;
                  E.Value := EL.Expressions.Create_Expression (Pattern (First .. Pos), ELContext);
                  if Prev /= null then
                     Prev.Next_Route := E.all'Access;
                  else
                     Parent.Children := E.all'Access;
                  end if;
                  Parent := E.all'Access;
               end if;
            end;

         elsif Pattern (First) = ':' then
            declare
               Param    : Param_Node_Access;
            begin
               First := First + 1;
               Prev := null;
               Found := False;

               --  Find the Param_Node that have the same name.
               while Node /= null loop
                  if Node.all in Param_Node_Type'Class then
                     Param := Param_Node_Type'Class (Node.all)'Access;
                     if Param.Name = Pattern (First .. Pos) then
                        Parent := Node;
                        Node := Node.Children;
                        Found := True;
                        exit;
                     end if;
                  end if;
                  Prev := Node;
                  Node := Node.Next_Route;
               end loop;

               --  Append the param node for the component.
               if not Found then
                  Param := new Param_Node_Type (Len => Pos - First + 1);
                  Param.Name := Pattern (First .. Pos);
                  if Prev /= null then
                     Prev.Next_Route := Param.all'Access;
                  else
                     Parent.Children := Param.all'Access;
                  end if;
                  Parent := Param.all'Access;
               end if;
            end;

         elsif Pattern (First) = '*' and First = Pattern'Last then
            Prev := null;
            Found := False;

            --  Find the Wildcard_Node.
            while Node /= null loop
               Found := Node.all in Wildcard_Node_Type'Class;
               exit when Found;
               Prev := Node;
               Node := Node.Next_Route;
            end loop;

            if not Found then
               declare
                  Wildcard  : Wildcard_Node_Access;
               begin
                  Wildcard := new Wildcard_Node_Type;
                  Node := Wildcard.all'Access;
                  if Prev /= null then
                     Prev.Next_Route := Node;
                  else
                     Parent.Children := Node;
                  end if;
               end;
            end if;
            Parent := Node;

         elsif Pattern (First) = '*' and Pos = Pattern'Last then
--              if First + 1 >= Pattern'Last or else Pattern (First + 1) /= '.' then
--                 return;
--              end if;
            declare
               Ext      : Extension_Node_Access;
            begin
               First := First + 1;
               Prev := null;
               Found := False;

               --  Find the Extension_Node that have the same name.
               while Node /= null loop
                  if Node.all in Extension_Node_Type'Class then
                     Ext := Extension_Node_Type'Class (Node.all)'Access;
                     if Ext.Ext = Pattern (First .. Pos) then
                        Parent := Node;
                        Node := Node.Children;
                        Found := True;
                        exit;
                     end if;
                  end if;
                  Prev := Node;
                  Node := Node.Next_Route;
               end loop;

               if not Found then
                  Ext := new Extension_Node_Type (Len => Pos - First + 1);
                  Ext.Ext := Pattern (First .. Pos);
                  if Prev /= null then
                     Prev.Next_Route := Ext.all'Access;
                  else
                     Parent.Children := Ext.all'Access;
                  end if;
                  Parent := Ext.all'Access;
               end if;
            end;

         else
            Found := False;

            --  Find an exact match for this component.
            while Node /= null loop
               if Node.all in Path_Node_Type'Class then
                  Match := Node.Matches (Pattern (First .. Pos), Pos = Pattern'Last);
                  if Match = YES_MATCH then
                     Parent := Node;
                     Node := Node.Children;
                     Found := True;
                     exit;
                  end if;
               end if;
               Node := Node.Next_Route;
            end loop;

            --  Add a path node matching the component at begining of the children list.
            --  (before the Param_Node and EL_Node instances if any).
            if not Found then
               New_Path := new Path_Node_Type (Len => Pos - First + 1);
               New_Path.Name := Pattern (First .. Pos);
               New_Path.Next_Route := Parent.Children;
               Parent.Children := New_Path.all'Access;
               Parent := Parent.Children;
            end if;
         end if;
         First := Pos + 2;
         exit when First > Pattern'Last;
      end loop;
      if Parent.Route = null then
         Parent.Route := To;
      end if;
   end Add_Route;

   --  ------------------------------
   --  Walk the routes that have been added by <tt>Add_Route</tt> and call the <tt>Process</tt>
   --  procedure with each path pattern and route object.
   --  ------------------------------
   procedure Iterate (Router  : in Router_Type;
                      Process : not null access procedure (Pattern : in String;
                                                           Route   : in Route_Type_Access)) is
   begin
      Router.Route.Iterate ("", Process);
   end Iterate;

   --  ------------------------------
   --  Build the route context from the given path by looking at the different routes registered
   --  in the router with <tt>Add_Route</tt>.
   --  ------------------------------
   procedure Find_Route (Router  : in Router_Type;
                         Path    : in String;
                         Context : in out Route_Context_Type'Class) is
      Match : Route_Match_Type;
   begin
      Log.Debug ("Finding route for {0}", Path);

      Context.Path := new String '(Path);
      Router.Route.Find_Match (Path, Path'First, Match, Context);
   end Find_Route;

   --  Find recursively a match on the given route sub-tree.  The match must start at the position
   --  <tt>First</tt> in the path up to the last path position.  While the path components are
   --  checked, the route context is populated with variable components.  When the full path
   --  matches, <tt>YES_MATCH</tt> is returned in the context gets the route instance.
   procedure Find_Match (Node    : in Route_Node_Type;
                         Path    : in String;
                         First   : in Natural;
                         Match   : out Route_Match_Type;
                         Context : in out Route_Context_Type'Class) is
      N     : Route_Node_Access := Node.Children;
      Pos   : Natural := First;
      Last  : Natural;
   begin
      while Pos <= Path'Last and then Path (Pos) = '/' loop
         Pos := Pos + 1;
      end loop;
      Last := Util.Strings.Index (Path, '/', Pos);
      if Last = 0 then
         Last := Path'Last;
      else
         Last := Last - 1;
      end if;
      while N /= null loop
         Match := N.Matches (Path (Pos .. Last), Last = Path'Last);
         if Match = YES_MATCH then
            if Last = Path'Last then
               Context.Route := N.Route;
               return;
            end if;
            N.Find_Match (Path, Last + 2, Match, Context);
            return;
         elsif Match = MAYBE_MATCH then
            declare
               Count : constant Natural := Context.Count + 1;
            begin
               Context.Count := Count;
               Context.Params (Count).Route := N;
               Context.Params (Count).First := Pos;
               Context.Params (Count).Last := Last;

               --  We reached the end of the path and we have a route, this is a match.
               if Last = Path'Last and N.Route /= null then
                  Match := YES_MATCH;
                  Context.Route := N.Route;
                  return;
               end if;
               N.Find_Match (Path, Last + 2, Match, Context);
               if Match = YES_MATCH then
                  return;
               end if;
               Context.Count := Count - 1;
            end;
         elsif Match = WILDCARD_MATCH then
            declare
               Ext   : constant Natural := Util.Strings.Rindex (Path, '/');
               Count : Natural;
            begin
               Match := N.Matches (Path (Ext + 1 .. Path'Last), True);
               if Match = YES_MATCH or Match = WILDCARD_MATCH then
                  Count := Context.Count + 1;
                  Context.Count := Count;
                  Context.Params (Count).Route := N;
                  Context.Params (Count).First := Pos;
                  Context.Params (Count).Last := Path'Last;
                  Context.Route := N.Route;
                  return;
               end if;
            end;
         end if;
         N := N.Next_Route;
      end loop;
      Match := NO_MATCH;
   end Find_Match;

   --  ------------------------------
   --  Walk the routes that have been added by <tt>Add_Route</tt> and call the <tt>Process</tt>
   --  procedure with each path pattern and route object.
   --  ------------------------------
   procedure Iterate (Node    : in Route_Node_Type;
                      Path    : in String;
                      Process : not null access procedure (Pattern : in String;
                                                           Route   : in Route_Type_Access)) is
      Child : Route_Node_Access := Node.Children;
   begin
      if Node.Route /= null then
         Process (Path, Node.Route);
      end if;
      while Child /= null loop
         Child.Iterate (Path & "/" & Child.Get_Pattern, Process);
         Child := Child.Next_Route;
      end loop;
   end Iterate;

   --  ------------------------------
   --  Check if the route node accepts the given path component.
   --  Returns YES_MATCH if the name corresponds exactly to the node's name.
   --  ------------------------------
   overriding
   function Matches (Node    : in Path_Node_Type;
                     Name    : in String;
                     Is_Last : in Boolean) return Route_Match_Type is
      pragma Unreferenced (Is_Last);
   begin
      if Node.Name = Name then
         return YES_MATCH;
      else
         return NO_MATCH;
      end if;
   end Matches;

   --  ------------------------------
   --  Return the component path pattern that this route node represents (ie, 'Name').
   --  ------------------------------
   overriding
   function Get_Pattern (Node : in Path_Node_Type) return String is
   begin
      return Node.Name;
   end Get_Pattern;

   --  ------------------------------
   --  Check if the route node accepts the given path component.
   --  Returns MAYBE_MATCH.
   --  ------------------------------
   overriding
   function Matches (Node    : in EL_Node_Type;
                     Name    : in String;
                     Is_Last : in Boolean) return Route_Match_Type is
      pragma Unreferenced (Node, Name, Is_Last);
   begin
      return MAYBE_MATCH;
   end Matches;

   --  ------------------------------
   --  Return the component path pattern that this route node represents (ie, the EL expr).
   --  ------------------------------
   overriding
   function Get_Pattern (Node : in EL_Node_Type) return String is
   begin
      return Node.Value.Get_Expression;
   end Get_Pattern;

   --  ------------------------------
   --  Inject the parameter that was extracted from the path.
   --  ------------------------------
   overriding
   procedure Inject_Parameter (Node      : in EL_Node_Type;
                               Param     : in String;
                               Into      : in out Util.Beans.Basic.Bean'Class;
                               ELContext : in EL.Contexts.ELContext'Class) is
      pragma Unreferenced (Into);
   begin
      Node.Value.Set_Value (Value   => Util.Beans.Objects.To_Object (Param),
                            Context => ELContext);
   end Inject_Parameter;

   --  ------------------------------
   --  Check if the route node accepts the given path component.
   --  Returns MAYBE_MATCH.
   --  ------------------------------
   overriding
   function Matches (Node    : in Param_Node_Type;
                     Name    : in String;
                     Is_Last : in Boolean) return Route_Match_Type is
      pragma Unreferenced (Node, Name, Is_Last);
   begin
      return MAYBE_MATCH;
   end Matches;

   --  ------------------------------
   --  Return the component path pattern that this route node represents (ie, Name).
   --  ------------------------------
   overriding
   function Get_Pattern (Node : in Param_Node_Type) return String is
   begin
      return ":" & Node.Name;
   end Get_Pattern;

   --  ------------------------------
   --  Inject the parameter that was extracted from the path.
   --  ------------------------------
   overriding
   procedure Inject_Parameter (Node      : in Param_Node_Type;
                               Param     : in String;
                               Into      : in out Util.Beans.Basic.Bean'Class;
                               ELContext : in EL.Contexts.ELContext'Class) is
      pragma Unreferenced (ELContext);
   begin
      Into.Set_Value (Name => Node.Name, Value => Util.Beans.Objects.To_Object (Param));
   end Inject_Parameter;

   --  ------------------------------
   --  Check if the route node accepts the given path component.
   --  Returns MAYBE_MATCH.
   --  ------------------------------
   overriding
   function Matches (Node    : in Extension_Node_Type;
                     Name    : in String;
                     Is_Last : in Boolean) return Route_Match_Type is
      Pos : Natural;
   begin
      if not Is_Last then
         return WILDCARD_MATCH;
      else
         Pos := Util.Strings.Index (Name, '.');
         if Pos = 0 then
            return NO_MATCH;
         elsif Name (Pos .. Name'Last) = Node.Ext then
            return YES_MATCH;
         else
            return NO_MATCH;
         end if;
      end if;
   end Matches;

   --  ------------------------------
   --  Return the component path pattern that this route node represents (ie, *.Ext).
   --  ------------------------------
   overriding
   function Get_Pattern (Node : in Extension_Node_Type) return String is
   begin
      return "*" & Node.Ext;
   end Get_Pattern;

   --  ------------------------------
   --  Check if the route node accepts the given extension.
   --  Returns WILDCARD_MATCH.
   --  ------------------------------
   overriding
   function Matches (Node    : in Wildcard_Node_Type;
                     Name    : in String;
                     Is_Last : in Boolean) return Route_Match_Type is
   begin
      return WILDCARD_MATCH;
   end Matches;

   --  ------------------------------
   --  Return the component path pattern that this route node represents (ie, *).
   --  ------------------------------
   overriding
   function Get_Pattern (Node : in Wildcard_Node_Type) return String is
   begin
      return "*";
   end Get_Pattern;

   --  ------------------------------
   --  Release the storage held by the router.
   --  ------------------------------
   overriding
   procedure Finalize (Router : in out Router_Type) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Route_Node_Type'Class,
                                        Name   => Route_Node_Access);

      procedure Destroy (Node : in out Route_Node_Access);

      --  ------------------------------
      --  Destroy a node recursively.
      --  ------------------------------
      procedure Destroy (Node : in out Route_Node_Access) is
         Child : Route_Node_Access;
      begin
         loop
            Child := Node.Children;
            exit when Child = null;
            Node.Children := Child.Next_Route;
            Destroy (Child);
         end loop;
         Free (Node);
         Node := null;
      end Destroy;

      Child : Route_Node_Access;
   begin
      loop
         Child := Router.Route.Children;
         exit when Child = null;
         Router.Route.Children := Child.Next_Route;
         Destroy (Child);
      end loop;
   end Finalize;

end ASF.Routes;
