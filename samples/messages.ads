-----------------------------------------------------------------------
--  messages - A simple memory-based forum
--  Copyright (C) 2012 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Util.Beans.Objects;
with Util.Beans.Basic;
with Util.Beans.Methods;

package Messages is

   --  Identifies a message in the forum.
   type Message_Id is new Positive;

   type Message_Bean is new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with private;
   type Message_Bean_Access is access all Message_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : Message_Bean;
                       Name : String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Message_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Post the message.
   procedure Post (From    : in out Message_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Message_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Create a message bean instance.
   function Create_Message_Bean return Util.Beans.Basic.Readonly_Bean_Access;

   --  The <tt>Forum_Bean</tt> contains a list of messages.
   --  It is intended to be shared by every user, hence there will be only one instance
   --  in the server.
   type Forum_Bean is new Util.Beans.Basic.List_Bean with private;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : Forum_Bean;
                       Name : String) return Util.Beans.Objects.Object;

   --  Get the number of elements in the list.
   overriding
   function Get_Count (From : in Forum_Bean) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   overriding
   procedure Set_Row_Index (From  : in out Forum_Bean;
                            Index : in Natural);

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : in Forum_Bean) return Util.Beans.Objects.Object;

   --  Create the list of messages.
   function Create_Message_List return Util.Beans.Basic.Readonly_Bean_Access;

private

   type Message_Bean is new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with record
      Id     : Message_Id;
      Text   : Ada.Strings.Unbounded.Unbounded_String;
      Email  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Message_Vector is
      new Ada.Containers.Vectors (Index_Type   => Message_Id,
                                  Element_Type => Message_Bean,
                                  "="          => "=");

   protected type Forum is

      --  Post the message in the forum.
      procedure Post (Message : in Message_Bean);

      --  Delete the message identified by <b>Id</b>.
      procedure Delete (Id : in Message_Id);

      --  Get the message identified by <b>Id</b>.
      function Get_Message (Id : in Message_Id) return Message_Bean;

      --  Get the number of messages in the forum.
      function Get_Message_Count return Natural;

   private
      Messages : Message_Vector.Vector;
   end Forum;

   type Forum_Bean is new Util.Beans.Basic.List_Bean with record
      Msg     : aliased Message_Bean;
      Pos     : Message_Id;
      Current : Util.Beans.Objects.Object;
   end record;
   type Forum_Bean_Access is access all Forum_Bean'Class;

end Messages;
