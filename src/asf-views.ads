-----------------------------------------------------------------------
--  asf-views -- Views
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

--  The <b>ASF.Views</b> package defines the abstractions to represent
--  an XHTML view that can be instantiated to create the JSF component
--  tree.  The <b>ASF.Views</b> are read only once and they are shared
--  by the JSF component trees that are instantiated from it.
--
--  The XHTML view is read using a SAX parser which creates nodes
--  and attributes to represent the view.
--
--  The <b>ASF.Views</b> is composed of nodes represented by <b>Tag_Node</b>
--  and attributes represented by <b>Tag_Attribute</b>.  In a sense, this
--  is very close to an XML DOM tree.
package ASF.Views is

end ASF.Views;
