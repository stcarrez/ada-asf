/*
 *  asf-widgets -- Widgets specific javascript
 *  Copyright (C) 2013 Stephane Carrez
 *  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
(function($, undefined) {

    $.widget( "ui.complete", $.ui.autocomplete, {
        _create: function() {
            var f, inp, self = this;

            $.ui.autocomplete.prototype._create.apply(this, arguments);

            /* Find the form and container to update */
            f = $(self.element).closest("form");
            inp = $(self.element);
            if (f.length > 0) {
                self._setOption("source", function(request, response) {
                    var url, params, term = encodeURIComponent(request.term);

                    url = $(f).attr('action');

                    params = $(inp).attr('name') + ".match=" + term + "&" + $(f).serialize();

                    /* Perform the HTTP POST */
                    jQuery.ajax({
                        type: "POST",
                        url: url,
                        data: params,
                        context: document.body,
                        success: function(data, status, jqXHDR) {
                            var contentType = jqXHDR.getResponseHeader('Content-type');

                            if (contentType != null && contentType.match(/^application\/json(;.*)?$/i)) {
                                response(data);
                            }
                        },
                        error: function(jqXHDR, status, error) {
                            ASF.AjaxError(jqXHDR, status, error, inp);
                        }
                    });
                });
            }
        }
    });

    /**
     * A collapsible panel using <w:panel>.
     */
    $.widget("ui.panel", {
        options: {
            delay: "fast"
        },
        _create: function() {
            var self = this;

            this.element.children(".ui-panel-header").find("a").bind('click', function(event) {
                var t = event.target;

                if ($(t).hasClass("ui-icon-closethick")) {
                    self.element.slideUp(self.options.delay);
                } else if ($(t).hasClass("ui-icon-minusthick")) {
                    self.element.children().not(".ui-panel-header").slideToggle(self.options.delay);
                }
                return false;
            });
        }
    });

})( jQuery );