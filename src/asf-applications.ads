-----------------------------------------------------------------------
--  applications -- Ada Web Application
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


   --  Get the configuration parameter;
   function Get (Self  : Config;
                 Param : Config_Param) return String;

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

   type Config is new Util.Properties.Manager with null record;
end ASF.Applications;
