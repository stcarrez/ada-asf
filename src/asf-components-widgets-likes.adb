-----------------------------------------------------------------------
--  components-widgets-likes -- Social Likes Components
--  Copyright (C) 2013, 2014, 2015, 2019 Stephane Carrez
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
with Util.Locales;
with Util.Beans.Objects;
with Util.Strings.Tokenizers;

--  GNAT bug, the with clause is necessary to call Get_Global on the application.
pragma Warnings (Off, "*is not referenced");
with ASF.Applications.Main;
pragma Warnings (On, "*is not referenced");

with ASF.Requests;
with ASF.Contexts.Writer;
package body ASF.Components.Widgets.Likes is

   FB_LAYOUT_ATTR      : aliased constant String := "data-layout";
   FB_SHOW_FACES_ATTR  : aliased constant String := "data-show-faces";
   FB_WIDTH_ATTR       : aliased constant String := "data-width";
   FB_ACTION_ATTR      : aliased constant String := "data-action";
   FB_FONT_ATTR        : aliased constant String := "data-font";
   FB_COlORSCHEME_ATTR : aliased constant String := "data-colorscheme";
   FB_REF_ATTR         : aliased constant String := "data-ref";
   FB_KIDS_ATTR        : aliased constant String := "data-kid_directed_site";
   FB_SEND_ATTR        : aliased constant String := "data-send";

   TW_VIA_ATTR         : aliased constant String := "data-via";
   TW_COUNT_ATTR       : aliased constant String := "data-count";
   TW_SIZE_ATTR        : aliased constant String := "data-size";
   TW_COUNTURL_ATTR    : aliased constant String := "data-counturl";
   TW_TEXT_ATTR        : aliased constant String := "data-text";
   TW_RELATED_ATTR     : aliased constant String := "data-related";
   TW_LANG_ATTR        : aliased constant String := "data-lang";
   TW_HASHTAGS_ATTR    : aliased constant String := "data-hashtags";

   FACEBOOK_ATTRIBUTE_NAMES  : Util.Strings.String_Set.Set;
   FACEBOOK_SCRIPT_ATTRIBUTE : constant String := "asf.widgets.facebook.script";

   TWITTER_ATTRIBUTE_NAMES    : Util.Strings.String_Set.Set;
   TWITTER_SCRIPT_ATTRIBUTE   : constant String := "asf.widgets.twitter.script";

   type Like_Generator_Binding is record
      Name      : Util.Strings.Name_Access;
      Generator : Like_Generator_Access;
   end record;

   type Like_Generator_Array is array (1 .. MAX_LIKE_GENERATOR) of Like_Generator_Binding;

   FB_NAME      : aliased constant String := "facebook";
   FB_GENERATOR : aliased Facebook_Like_Generator;

   TWITTER_NAME       : aliased constant String := "twitter";
   TWITTER_GENERATOR  : aliased Twitter_Like_Generator;

   Generators : Like_Generator_Array := (1 => (FB_NAME'Access, FB_GENERATOR'Access),
                                         2 => (TWITTER_NAME'Access, TWITTER_GENERATOR'Access),
                                         others => (null, null));

   --  ------------------------------
   --  Render the facebook like button according to the component attributes.
   --  ------------------------------
   overriding
   procedure Render_Like (Generator : in Facebook_Like_Generator;
                          UI        : in UILike'Class;
                          Href      : in String;
                          Context   : in out ASF.Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (Generator);

      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
      Request : constant ASF.Requests.Request_Access := Context.Get_Request;
   begin
      if not Context.Is_Ajax_Request and then
        Util.Beans.Objects.Is_Null (Request.Get_Attribute (FACEBOOK_SCRIPT_ATTRIBUTE))
      then
         Request.Set_Attribute (FACEBOOK_SCRIPT_ATTRIBUTE, Util.Beans.Objects.To_Object (True));
         Writer.Queue_Script ("(function(d, s, id) { var js, fjs = d.getElementsByTagName(s)[0];"
                              & "if (d.getElementById(id)) return;"
                              & "js = d.createElement(s); js.id = id;js.async=true;"
                              & "js.src = ""//connect.facebook.net/");
         Writer.Queue_Script (Util.Locales.To_String (Context.Get_Locale));
         Writer.Queue_Script ("/all.js#xfbml=1&;appId=");
         declare
            App_Id : constant String
              := Context.Get_Application.Get_Config (P_Facebook_App_Id.P);
         begin
            if App_Id'Length = 0 then
               UI.Log_Error ("The facebook client application id is empty");
               UI.Log_Error ("Please, configure the '{0}' property "
                             & "in the application", P_Facebook_App_Id.PARAM_NAME);
            else
               Writer.Queue_Script (App_Id);
            end if;
         end;
         Writer.Queue_Script (""";fjs.parentNode.insertBefore(js, fjs);"
                              & "}(document, 'script', 'facebook-jssdk'));");
         Writer.Start_Element ("div");
         Writer.Write_Attribute ("id", "fb-root");
         Writer.End_Element ("div");
      end if;
      Writer.Start_Element ("div");
      Writer.Write_Attribute ("class", "fb-like");
      Writer.Write_Attribute ("data-href", Href);
      UI.Render_Attributes (Context, FACEBOOK_ATTRIBUTE_NAMES, Writer);
      Writer.End_Element ("div");
   end Render_Like;

   --  ------------------------------
   --  Tweeter like generator
   --  ------------------------------
   overriding
   procedure Render_Like (Generator : in Twitter_Like_Generator;
                          UI        : in UILike'Class;
                          Href      : in String;
                          Context   : in out ASF.Contexts.Faces.Faces_Context'Class) is
      pragma Unreferenced (Generator);

      Writer  : constant Contexts.Writer.Response_Writer_Access := Context.Get_Response_Writer;
      Request : constant ASF.Requests.Request_Access := Context.Get_Request;
      Lang    : constant String := Util.Locales.Get_ISO3_Language (Context.Get_Locale);
   begin
      if not Context.Is_Ajax_Request and then
        Util.Beans.Objects.Is_Null (Request.Get_Attribute (TWITTER_SCRIPT_ATTRIBUTE))
      then
         Request.Set_Attribute (TWITTER_SCRIPT_ATTRIBUTE, Util.Beans.Objects.To_Object (True));
         Writer.Queue_Script ("!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],"
                              & "p=/^http:/.test(d.location)?'http':'https';"
                              & "if(!d.getElementById(id)){js=d.createElement(s);js.id=id;"
                              & "js.src=p+'://platform.twitter.com/widgets.js';"
                              & "fjs.parentNode.insertBefore(js,fjs);}}"
                              & "(document, 'script', 'twitter-wjs');");
      end if;
      Writer.Start_Element ("a");
      Writer.Write_Attribute ("href", "https://twitter.com/share");
      Writer.Write_Attribute ("class", "twitter-share-button");
      Writer.Write_Attribute ("data-url", Href);
      Writer.Write_Attribute ("data-lang", Lang);
      UI.Render_Attributes (Context, TWITTER_ATTRIBUTE_NAMES, Writer);
      Writer.Write_Text ("Tweet");
      Writer.End_Element ("a");
   end Render_Like;

   --  ------------------------------
   --  Get the link to submit in the like action.
   --  ------------------------------
   function Get_Link (UI      : in UILike;
                      Context : in ASF.Contexts.Faces.Faces_Context'Class) return String is
      Href : constant String := UI.Get_Attribute ("href", Context, "");
   begin
      if Href'Length > 0 then
         return Href;
      else
         return Context.Get_Request.Get_Request_URI;
      end if;
   end Get_Link;

   --  ------------------------------
   --  Render an image with the source link created from an email address to the Gravatars service.
   --  ------------------------------
   overriding
   procedure Encode_Begin (UI      : in UILike;
                           Context : in out ASF.Contexts.Faces.Faces_Context'Class) is
   begin
      if UI.Is_Rendered (Context) then
         declare
            Writer : constant Contexts.Writer.Response_Writer_Access
              := Context.Get_Response_Writer;
            Kind   : constant String := UI.Get_Attribute ("type", Context, "");
            Href   : constant String := UILike'Class (UI).Get_Link (Context);
            Style  : constant String := UI.Get_Attribute ("style", Context, "");
            Class  : constant String := UI.Get_Attribute ("styleClass", Context, "");

            procedure Render (Name : in String; Done : out Boolean);

            procedure Render (Name : in String;
                              Done : out Boolean) is
               use type Util.Strings.Name_Access;
            begin
               Done := False;
               for I in Generators'Range loop
                  exit when Generators (I).Name = null;
                  if Generators (I).Name.all = Name then
                     Writer.Start_Element ("div");
                     if Style'Length > 0 then
                        Writer.Write_Attribute ("style", Style);
                     end if;
                     if Class'Length > 0 then
                        Writer.Write_Attribute ("class", Class);
                     end if;
                     Generators (I).Generator.Render_Like (UI, Href, Context);
                     Writer.End_Element ("div");
                     return;
                  end if;
               end loop;
               UI.Log_Error ("Like type {0} is not recognized", Name);
            end Render;

         begin
            if Kind'Length = 0 then
               UI.Log_Error ("The like type is empty.");
            else
               Util.Strings.Tokenizers.Iterate_Tokens (Content => Kind, Pattern => ",",
                                                       Process => Render'Access);
            end if;
         end;
      end if;
   end Encode_Begin;

   --  ------------------------------
   --  Register the like generator under the given name.
   --  ------------------------------
   procedure Register_Like (Name      : in Util.Strings.Name_Access;
                            Generator : in Like_Generator_Access) is
      use type Util.Strings.Name_Access;
   begin
      for I in Generators'Range loop
         if Generators (I).Name = null then
            Generators (I).Name := Name;
            Generators (I).Generator := Generator;
            return;
         end if;
      end loop;
   end Register_Like;

begin
   FACEBOOK_ATTRIBUTE_NAMES.Insert (FB_LAYOUT_ATTR'Access);
   FACEBOOK_ATTRIBUTE_NAMES.Insert (FB_SHOW_FACES_ATTR'Access);
   FACEBOOK_ATTRIBUTE_NAMES.Insert (FB_WIDTH_ATTR'Access);
   FACEBOOK_ATTRIBUTE_NAMES.Insert (FB_ACTION_ATTR'Access);
   FACEBOOK_ATTRIBUTE_NAMES.Insert (FB_FONT_ATTR'Access);
   FACEBOOK_ATTRIBUTE_NAMES.Insert (FB_COlORSCHEME_ATTR'Access);
   FACEBOOK_ATTRIBUTE_NAMES.Insert (FB_REF_ATTR'Access);
   FACEBOOK_ATTRIBUTE_NAMES.Insert (FB_KIDS_ATTR'Access);
   FACEBOOK_ATTRIBUTE_NAMES.Insert (FB_SEND_ATTR'Access);

   TWITTER_ATTRIBUTE_NAMES.Insert (TW_SIZE_ATTR'Access);
   TWITTER_ATTRIBUTE_NAMES.Insert (TW_COUNT_ATTR'Access);
   TWITTER_ATTRIBUTE_NAMES.Insert (TW_VIA_ATTR'Access);
   TWITTER_ATTRIBUTE_NAMES.Insert (TW_COUNTURL_ATTR'Access);
   TWITTER_ATTRIBUTE_NAMES.Insert (TW_TEXT_ATTR'Access);
   TWITTER_ATTRIBUTE_NAMES.Insert (TW_RELATED_ATTR'Access);
   TWITTER_ATTRIBUTE_NAMES.Insert (TW_LANG_ATTR'Access);
   TWITTER_ATTRIBUTE_NAMES.Insert (TW_HASHTAGS_ATTR'Access);
end ASF.Components.Widgets.Likes;
