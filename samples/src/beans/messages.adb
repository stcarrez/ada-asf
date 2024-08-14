-----------------------------------------------------------------------
--  messages - A simple memory-based forum
--  Copyright (C) 2012, 2014, 2022 Stephane Carrez
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
with Ada.Calendar;
with Util.Beans.Objects.Time;
with ASF.Events.Faces.Actions;
package body Messages is

   use Ada.Strings.Unbounded;

   Database : aliased Forum;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Message_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "text" then
         return Util.Beans.Objects.To_Object (From.Text);

      elsif Name = "email" then
         return Util.Beans.Objects.To_Object (From.Email);

      elsif Name = "id" then
         return Util.Beans.Objects.To_Object (Integer (From.Id));
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Message_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "text" then
         From.Text := Util.Beans.Objects.To_Unbounded_String (Value);

      elsif Name = "email" then
         From.Email := Util.Beans.Objects.To_Unbounded_String (Value);

      elsif Name = "id" then
         From.Id := Message_Id (Util.Beans.Objects.To_Integer (Value));

      end if;
   end Set_Value;

   --  ------------------------------
   --  Post the message.
   --  ------------------------------
   procedure Post (From    : in out Message_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Length (From.Text) > 200 then
         Ada.Strings.Unbounded.Replace_Slice (From.Text, 200, Length (From.Text), "...");
      end if;
      Database.Post (From);
      Outcome := To_Unbounded_String ("posted");
   end Post;

   package Post_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Message_Bean,
                                                      Method => Post,
                                                      Name   => "post");

   Binding_Array : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Post_Binding.Proxy'Unchecked_Access, Post_Binding.Proxy'Unchecked_Access);

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Message_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Binding_Array'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Create a message bean instance.
   --  ------------------------------
   function Create_Message_Bean return Util.Beans.Basic.Readonly_Bean_Access is
      Result : constant Message_Bean_Access := new Message_Bean;
   begin
      return Result.all'Access;
   end Create_Message_Bean;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Forum_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "count" then
         return Util.Beans.Objects.To_Object (Integer (From.Get_Count));
      elsif Name = "today" then
         return Util.Beans.Objects.Time.To_Object (Ada.Calendar.Clock);
      else
         return Util.Beans.Objects.Null_Object;
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   overriding
   function Get_Count (From : in Forum_Bean) return Natural is
      pragma Unreferenced (From);
   begin
      return Database.Get_Message_Count;
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   overriding
   procedure Set_Row_Index (From  : in out Forum_Bean;
                            Index : in Natural) is
   begin
      From.Pos := Message_Id (Index);
      From.Msg := Database.Get_Message (From.Pos);
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   overriding
   function Get_Row (From  : in Forum_Bean) return Util.Beans.Objects.Object is
   begin
      return From.Current;
   end Get_Row;

   --  ------------------------------
   --  Create the list of messages.
   --  ------------------------------
   function Create_Message_List return Util.Beans.Basic.Readonly_Bean_Access is
      F : constant Forum_Bean_Access := new Forum_Bean;
   begin
      F.Current := Util.Beans.Objects.To_Object (F.Msg'Access, Util.Beans.Objects.STATIC);
      return F.all'Access;
   end Create_Message_List;

   protected body Forum is

      --  ------------------------------
      --  Post the message in the forum.
      --  ------------------------------
      procedure Post (Message : in Message_Bean) is
         use type Ada.Containers.Count_Type;
      begin
         if Messages.Length > 100 then
            Messages.Delete (Message_Id'First);
         end if;
         Messages.Append (Message);
      end Post;

      --  ------------------------------
      --  Delete the message identified by <b>Id</b>.
      --  ------------------------------
      procedure Delete (Id : in Message_Id) is
      begin
         Messages.Delete (Id);
      end Delete;

      --  ------------------------------
      --  Get the message identified by <b>Id</b>.
      --  ------------------------------
      function Get_Message (Id : in Message_Id) return Message_Bean is
      begin
         return Messages.Element (Id);
      end Get_Message;

      --  ------------------------------
      --  Get the number of messages in the forum.
      --  ------------------------------
      function Get_Message_Count return Natural is
      begin
         return Natural (Messages.Length);
      end Get_Message_Count;

   end Forum;

end Messages;
