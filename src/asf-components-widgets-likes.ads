-----------------------------------------------------------------------
--  components-widgets-likes -- Social Likes Components
--  Copyright (C) 2013, 2019 Stephane Carrez
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

with ASF.Applications;
with ASF.Components.Html;
with ASF.Contexts.Faces;
package ASF.Components.Widgets.Likes is

   --  ------------------------------
   --  UILike
   --  ------------------------------
   --  The <b>UILike</b> component displays a social like button to recommend a page.
   type UILike is new ASF.Components.Html.UIHtmlComponent with null record;

   --  Get the link to submit in the like action.
   function Get_Link (UI      : in UILike;
                      Context : in ASF.Contexts.Faces.Faces_Context'Class) return String;

   --  Render the like button for Facebook or Google+.
   overriding
   procedure Encode_Begin (UI      : in UILike;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  ------------------------------
   --  Like Generator
   --  ------------------------------
   --  The <tt>Like_Generator</tt> represents the specific method for the generation of
   --  a social like generator.  A generator is registered under a given name with the
   --  <tt>Register_Like</tt> operation.  The current implementation provides a Facebook
   --  like generator.
   type Like_Generator is limited interface;
   type Like_Generator_Access is access all Like_Generator'Class;

   --  Render the like button according to the generator and the component attributes.
   procedure Render_Like (Generator : in Like_Generator;
                          UI        : in UILike'Class;
                          Href      : in String;
                          Context   : in out ASF.Contexts.Faces.Faces_Context'Class) is abstract;

   --  Maximum number of generators that can be registered.
   MAX_LIKE_GENERATOR : constant Positive := 5;

   --  Register the like generator under the given name.
   procedure Register_Like (Name      : in Util.Strings.Name_Access;
                            Generator : in Like_Generator_Access);

   --  ------------------------------
   --  Facebook like generator
   --  ------------------------------
   type Facebook_Like_Generator is new Like_Generator with null record;

   overriding
   procedure Render_Like (Generator : in Facebook_Like_Generator;
                          UI        : in UILike'Class;
                          Href      : in String;
                          Context   : in out ASF.Contexts.Faces.Faces_Context'Class);

   --  The application configuration parameter that defines which Facebook client ID must be used.
   package P_Facebook_App_Id is
     new ASF.Applications.Parameter ("facebook.client_id", "");

   --  ------------------------------
   --  Twitter like generator
   --  ------------------------------
   type Twitter_Like_Generator is new Like_Generator with null record;

   overriding
   procedure Render_Like (Generator : in Twitter_Like_Generator;
                          UI        : in UILike'Class;
                          Href      : in String;
                          Context   : in out ASF.Contexts.Faces.Faces_Context'Class);

end ASF.Components.Widgets.Likes;
