-----------------------------------------------------------------------
--  asf-filters-cache_control -- HTTP response Cache-Control settings
--  Copyright (C) 2015 Stephane Carrez
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

package body ASF.Filters.Cache_Control is

   --  ------------------------------
   --  The Do_Filter method of the Filter is called by the container each time
   --  a request/response pair is passed through the chain due to a client request
   --  for a resource at the end of the chain.  The <b>Cache_Control</b> filter adds
   --  a <tt>Cache-Control</tt>, <tt>Expires</tt>, <tt>Pragma</tt> and optionally a
   --  <tt>Vary</tt> header in the HTTP response.
   --  ------------------------------
   overriding
   procedure Do_Filter (F        : in Cache_Control_Filter;
                        Request  : in out Requests.Request'Class;
                        Response : in out Responses.Response'Class;
                        Chain    : in out ASF.Servlets.Filter_Chain) is
      use Ada.Strings.Unbounded;
   begin
      if Length (F.Cache_Control_Header) > 0 then
         Response.Add_Header ("Cache-Control", To_String (F.Cache_Control_Header));
      end if;
      if Length (F.Vary) > 0 then
         Response.Add_Header ("Vary", To_String (F.Vary));
      end if;
      ASF.Servlets.Do_Filter (Chain    => Chain,
                              Request  => Request,
                              Response => Response);
   end Do_Filter;

   --  ------------------------------
   --  Called by the servlet container to indicate to a filter that the filter
   --  instance is being placed into service.
   --  ------------------------------
   procedure Initialize (Server  : in out Cache_Control_Filter;
                         Config  : in ASF.Servlets.Filter_Config) is
   begin
      Server.Vary                 := Servlets.Get_Init_Parameter (Config, VARY_HEADER_PARAM);
      Server.Cache_Control_Header := Servlets.Get_Init_Parameter (Config, CACHE_CONTROL_PARAM);
   end Initialize;

end ASF.Filters.Cache_Control;
