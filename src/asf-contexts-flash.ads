-----------------------------------------------------------------------
--  contexts-facelets-flash -- Flash context
--  Copyright (C) 2012, 2022 Stephane Carrez
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

with Util.Beans.Basic;
with Util.Beans.Objects.Maps;

with ASF.Events.Phases;
with ASF.Contexts.Faces;
with ASF.Applications.Messages.Vectors;

--  The <b>Flash</b> package implements the flash context scope which allows to pass
--  temporary information between requests.  The <b>Flash</b> concept is taken from
--  Ruby on Rails.  A typical usage is the following:
--
--   1/ an application receives a GET or a POST request.  While processing that request
--      if stores some information in the flash context.  The information is not available
--      immediately but prepared for the next request.  In many cases, messages are stored
--      in the flash context.
--
--   2/ after the GET or the POST, the navigation rules redirect the user to another page.
--
--   3/ at the next GET request, in the render response phase, the flash context is
--      retrieve and the information that was stored in [1] gets available.  Messages that
--      could have been stored, are then displayed during this phase.
--
--   4/ the flash context is cleared and a new flash context could be setup with new information.
--
--  The flash context is accessed from the XHTML facetlet files through the <b>flash</b> variable.
package ASF.Contexts.Flash is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Flash context
   --  ------------------------------
   --  The <b>Flash_Context</b> gives access to the flash instance and its management.
   --  There are two flash instances: the 'previous' and the 'next' one.
   --   o attributes are read from the 'previous' instance,
   --   o attributes are written by default to the 'next' instance,
   --
   type Flash_Context is tagged limited private;
   type Flash_Context_Access is access all Flash_Context'Class;

   --  Set the attribute having given name with the value.
   procedure Set_Attribute (Flash   : in out Flash_Context;
                            Name    : in String;
                            Value   : in Util.Beans.Objects.Object);

   --  Set the attribute having given name with the value.
   procedure Set_Attribute (Flash   : in out Flash_Context;
                            Name    : in Unbounded_String;
                            Value   : in Util.Beans.Objects.Object);

   --  Get the attribute with the given name from the 'previous' flash context.
   function Get_Attribute (Flash : in Flash_Context;
                           Name  : in String) return Util.Beans.Objects.Object;

   --  Keep in the flash context the request attribute identified by the name <b>Name</b>.
   procedure Keep (Flash   : in out Flash_Context;
                   Name    : in String);

   --  Returns True if the faces messages that are queued in the faces context must be
   --  preserved so they are accessible through the flash instance at the next request.
   function Is_Keep_Messages (Flash : in Flash_Context) return Boolean;

   --  Set the keep messages property which controls whether the faces messages
   --  that are queued in the faces context must be preserved so they are accessible through
   --  the flash instance at the next request.
   procedure Set_Keep_Messages (Flash : in out Flash_Context;
                                Value : in Boolean);

   --  Returns True if the <b>Redirect</b> property was set on the previous flash instance.
   function Is_Redirect (Flash : in Flash_Context) return Boolean;

   --  Set this property to True to indicate to the next request on this session will be
   --  a redirect.  After this call, the next request will return the <b>Redirect</b> value
   --  when the <b>Is_Redirect</b> function will be called.
   procedure Set_Redirect (Flash    : in out Flash_Context;
                           Redirect : in Boolean);

   --  Perform any specific action before processing the phase referenced by <b>Phase</b>.
   --  This operation is used to restore the flash context for a new request.
   procedure Do_Pre_Phase_Actions (Flash   : in out Flash_Context;
                                   Phase   : in ASF.Events.Phases.Phase_Type;
                                   Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Perform any specific action after processing the phase referenced by <b>Phase</b>.
   --  This operation is used to save the flash context
   procedure Do_Post_Phase_Actions (Flash   : in out Flash_Context;
                                    Phase   : in ASF.Events.Phases.Phase_Type;
                                    Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  Perform the last actions that must be made to save the flash context in the session.
   procedure Do_Last_Phase_Actions (Flash   : in out Flash_Context;
                                    Context : in out ASF.Contexts.Faces.Faces_Context'Class);

private

   type Flash_Bean is new Util.Beans.Basic.Readonly_Bean with record
      --  Attributes bound to this flash instance.
      Attributes   : aliased Util.Beans.Objects.Maps.Map;

      --  Messages that have been saved in the flash context.
      Messages     : ASF.Applications.Messages.Vectors.Vector;

      Redirect     : Boolean := False;
   end record;
   type Flash_Bean_Access is access all Flash_Bean'Class;

   --  Get the value identified by the name.
   --  If the name cannot be found, the method should return the Null object.
   overriding
   function Get_Value (From : in Flash_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   procedure Get_Active_Flash (Flash   : in out Flash_Context;
                               Result  : out Flash_Bean_Access);

   procedure Get_Execute_Flash (Flash   : in out Flash_Context;
                                Result  : out Flash_Bean_Access);

   type Flash_Context is tagged limited record
      --  When not null, the previous flash information.
      Previous        : Flash_Bean_Access := null;

      --  The next flash information to save (initialized on the first use).
      Next            : Flash_Bean_Access := null;

      --  The previous flash information (kept as an object for reference counting).
      Object          : Util.Beans.Objects.Object;

      --  Whether the Do_Last_Phase_Actions action was done.
      Last_Phase_Done : Boolean := False;

      --  Whether the messages that are queued in the faces context must be saved in the flash.
      Keep_Messages   : Boolean := False;
   end record;

end ASF.Contexts.Flash;
