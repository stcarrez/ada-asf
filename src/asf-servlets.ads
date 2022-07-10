-----------------------------------------------------------------------
--  asf.servlets -- ASF Servlets
--  Copyright (C) 2010, 2011, 2012, 2013, 2015, 2016, 2017, 2018, 2022 Stephane Carrez
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
with Servlet.Core;
with Servlet.Requests;
with Servlet.Responses;
package ASF.Servlets is

   subtype Servlet_Access is Servlet.Core.Servlet_Access;
   subtype Servlet_Registry is Servlet.Core.Servlet_Registry;
   subtype Servlet_Registry_Access is Servlet.Core.Servlet_Registry_Access;
   subtype Request_Dispatcher is Servlet.Core.Request_Dispatcher;
   subtype Filter_Config is Servlet.Core.Filter_Config;
   subtype Filter_Chain is Servlet.Core.Filter_Chain;

   --  Get the servlet context associated with the filter chain.
   function Get_Servlet_Context (Chain : in Filter_Chain) return Servlet_Registry_Access
     renames Servlet.Core.Get_Servlet_Context;

   --  Causes the next filter in the chain to be invoked, or if the calling
   --  filter is the last filter in the chain, causes the resource at the end
   --  of the chain to be invoked.
   procedure Do_Filter (Chain    : in out Filter_Chain;
                        Request  : in out Servlet.Requests.Request'Class;
                        Response : in out Servlet.Responses.Response'Class)
                        renames Servlet.Core.Do_Filter;

   function Get_Init_Parameter (Config  : in Filter_Config;
                                Name    : in String;
                                Default : in String := "") return String
                                renames Servlet.Core.Get_Init_Parameter;
   function Get_Init_Parameter (Config : in Filter_Config;
                                Name    : in String;
                                Default : in String := "")
                                return Ada.Strings.Unbounded.Unbounded_String
                                renames Servlet.Core.Get_Init_Parameter;
   function Get_Init_Parameter (Context : in Servlet_Registry;
                                Name    : in String;
                                Default : in String := "") return String
                                renames Servlet.Core.Get_Init_Parameter;
   function Get_Init_Parameter (Context : in Servlet_Registry;
                                Name    : in String;
                                Default : in String := "")
                                return Ada.Strings.Unbounded.Unbounded_String
                                renames Servlet.Core.Get_Init_Parameter;

end ASF.Servlets;
