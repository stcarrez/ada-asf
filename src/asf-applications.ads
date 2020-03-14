-----------------------------------------------------------------------
--  applications -- Ada Web Application
--  Copyright (C) 2009, 2010, 2013, 2015, 2020 Stephane Carrez
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

with Util.Strings;
with Util.Properties;
package ASF.Applications is

   use Util.Properties;

   type Config is new Util.Properties.Manager with private;

   type Config_Param is private;

   --  Directory where the facelet files are stored.
   VIEW_DIR_PARAM      : constant Config_Param;
   VIEW_DIR            : aliased constant String := "view.dir";
   DEF_VIEW_DIR        : aliased constant String := "web";

   --  Extension used by requests
   VIEW_EXT_PARAM      : constant Config_Param;
   VIEW_EXT            : aliased constant String := "view.ext";
   DEF_VIEW_EXT        : aliased constant String := "";

   --  Extension used by facelet files
   VIEW_FILE_EXT_PARAM : constant Config_Param;
   VIEW_FILE_EXT       : aliased constant String := "view.file_ext";
   DEF_VIEW_FILE_EXT   : aliased constant String := ".xhtml";

   --  Whether white spaces in XHTML files are ignored.
   VIEW_IGNORE_WHITE_SPACES_PARAM : constant Config_Param;
   VIEW_IGNORE_WHITE_SPACES       : aliased constant String := "view.ignore_white_spaces";
   DEF_IGNORE_WHITE_SPACES        : aliased constant String := "false";

   --  Whether empty lines in XHTML files are ignored.
   VIEW_IGNORE_EMPTY_LINES_PARAM  : constant Config_Param;
   VIEW_IGNORE_EMPTY_LINES        : aliased constant String := "view.ignore_empty_lines";
   DEF_IGNORE_EMPTY_LINES         : aliased constant String := "false";

   --  Whether the unknown tags are escaped in the output
   VIEW_ESCAPE_UNKNOWN_TAGS_PARAM : constant Config_Param;
   VIEW_ESCAPE_UNKNOWN_TAGS       : aliased constant String := "view.escape_unknown_tags";
   DEF_ESCAPE_UNKNOWN_TAGS        : aliased constant String := "false";

   --  Extensions used by static pages
   VIEW_STATIC_EXT_PARAM : constant Config_Param;
   VIEW_STATIC_EXT       : aliased constant String := "view.static.ext";
   DEF_STATIC_EXT        : aliased constant String := "";

   --  Directories which contain static pages
   VIEW_STATIC_DIR_PARAM : constant Config_Param;
   VIEW_STATIC_DIR       : aliased constant String := "view.static.dir";
   DEF_STATIC_DIR        : aliased constant String := "css,js,scripts,themes";

   --  The 404 error page to render
   ERROR_404_PARAM       : constant Config_Param;
   ERROR_404_PAGE        : aliased constant String := "view.error.404";
   DEF_ERROR_404         : aliased constant String := "errors/404";

   --  The 500 error page to render
   ERROR_500_PARAM       : constant Config_Param;
   ERROR_500_PAGE        : aliased constant String := "view.error.500";
   DEF_ERROR_500         : aliased constant String := "errors/500";

   --  Get the configuration parameter;
   function Get (Self  : Config;
                 Param : Config_Param) return String;

   --  Get the configuration parameter;
   function Get (Self  : Config;
                 Param : Config_Param) return Ada.Strings.Unbounded.Unbounded_String;

   --  Get the configuration parameter;
   function Get (Self  : Config;
                 Param : Config_Param) return Boolean;

   function Get (Self  : Config;
                 Param : Config_Param) return Integer;

   --  Create the configuration parameter definition instance.
   generic
      --  The parameter name.
      Name    : in String;

      --  The default value.
      Default : in String;
   package Parameter is

      --  Returns the configuration parameter.
      function P return Config_Param;
      pragma Inline_Always (P);

      PARAM_NAME  : aliased constant String := Name;
      PARAM_VALUE : aliased constant String := Default;
   end Parameter;

private

   type Config_Param is record
      Name    : Util.Strings.Name_Access;
      Default : Util.Strings.Name_Access;
   end record;

   subtype P is Config_Param;

   VIEW_DIR_PARAM : constant Config_Param := P '(Name    => VIEW_DIR'Access,
                                                 Default => DEF_VIEW_DIR'Access);

   VIEW_EXT_PARAM : constant Config_Param := P '(Name    => VIEW_EXT'Access,
                                                 Default => DEF_VIEW_EXT'Access);

   VIEW_FILE_EXT_PARAM : constant Config_Param := P '(Name    => VIEW_FILE_EXT'Access,
                                                      Default => DEF_VIEW_FILE_EXT'Access);

   VIEW_IGNORE_WHITE_SPACES_PARAM : constant Config_Param
     := P '(Name    => VIEW_IGNORE_WHITE_SPACES'Access,
            Default => DEF_IGNORE_WHITE_SPACES'Access);

   VIEW_IGNORE_EMPTY_LINES_PARAM : constant Config_Param
     := P '(Name    => VIEW_IGNORE_EMPTY_LINES'Access,
            Default => DEF_IGNORE_EMPTY_LINES'Access);

   VIEW_ESCAPE_UNKNOWN_TAGS_PARAM : constant Config_Param
     := P '(Name    => VIEW_ESCAPE_UNKNOWN_TAGS'Access,
            Default => DEF_ESCAPE_UNKNOWN_TAGS'Access);

   VIEW_STATIC_EXT_PARAM : constant Config_Param := P '(Name    => VIEW_STATIC_EXT'Access,
                                                        Default => DEF_STATIC_EXT'Access);

   VIEW_STATIC_DIR_PARAM : constant Config_Param := P '(Name    => VIEW_STATIC_DIR'Access,
                                                        Default => DEF_STATIC_DIR'Access);

   ERROR_404_PARAM : constant Config_Param := P '(Name    => ERROR_404_PAGE'Access,
                                                  Default => DEF_ERROR_404'Access);

   ERROR_500_PARAM : constant Config_Param := P '(Name    => ERROR_500_PAGE'Access,
                                                  Default => DEF_ERROR_500'Access);

   type Config is new Util.Properties.Manager with null record;
end ASF.Applications;
