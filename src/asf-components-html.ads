-----------------------------------------------------------------------
--  html -- ASF HTML Components
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
with ASF.Contexts.Writer;
with ASF.Components.Core;
with Util.Strings;
package ASF.Components.Html is

   use ASF.Contexts.Writer;

   type UIHtmlComponent is new ASF.Components.Core.UIComponentBase with private;

   procedure Render_Attributes (UI      : in UIHtmlComponent;
                                Context : in out Faces_Context'Class;
                                Writer  : in ResponseWriter_Access);

   --  Render the attributes which are defined on the component and which are
   --  in the list specified by <b>names</b>.
   procedure Render_Attributes (UI      : in UIHtmlComponent;
                                Context : in out Faces_Context'Class;
                                Names   : in Util.Strings.String_Set.Set;
                                Writer  : in ResponseWriter_Access);

private

   type UIHtmlComponent is new ASF.Components.Core.UIComponentBase with record
      Value1 : EL.Objects.Object;
   end record;

end ASF.Components.Html;
