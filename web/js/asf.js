/*
 *  asf -- Ada Server Faces
 *  Copyright (C) 2009, 2010, 2011 Stephane Carrez
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
var ASF = {};

(function () {
    /**
     * Execute the AJAX response action represented by the JSON object <b>action</b>.
     *
     * @param node the current node
     * @param action the JSON action object
     */
    ASF.ExecuteOne = function(node, action) {
        var id = action.id === null ? node : action.id;
        if (action.action === "update") {
	        $(id).html(action.data);

        } else if (action.action === "prepend") {
	        $(id).prepend(action.data);

        } else if (action.action === "append") {
	        $(id).append(action.data);

        } else if (action.action === "show") {
            $(id).show('slow');

        } else if (action.action === "hide") {
            $(id).hide('slow');

        } else if (action.action === "delete") {
	        $(id).remove();

        } else if (action.action === "removeClass") {
	        $(id).removeClass(action.data);

        } else if (action.action === "addClass") {
	        $(id).addClass(action.data);

        } else if (action.action === "fadeIn") {
            $(id).fadeIn('slow');

        } else if (action.action === "fadeOut") {
            $(id).fadeOut('slow');

        } else if (action.action === "slideUp") {
            $(id).slideUp('slow');

        } else if (action.action === "slideDown") {
            $(id).slideDown('slow');

        } else if (action.action === "closeDialog") {
            $(id).dialog('close');

        } else if (action.action === "redirect") {
            document.location = action.url;
	    }
    };

    ASF.Execute = function(node, data) {
        if (data != null) {
            for (var i = 0; i < data.length; i++) {
                ASF.ExecuteOne(node, data[i]);
            }
        }
    };

    /**
     * Update the target container with the content of the AJAX GET request
     * on the given URL.  The target parameter is optional.
     *
     * @param node the current element
     * @param url the URL to fetch using an HTTP GET
     * @param target the optional target element
     * @param complete an optional function called when the response is received
     */
    ASF.Update = function(node, url, target, complete) {
        /* Find the container to update */
        var d;
        if (target != null) {
            d = $(target);
        } else {
            d = $(node);
            if (!d.is('.asf-container')) {
                d = d.parent('.asf-container');
            }
        }
        if (d.length > 0) {
            /* Perform the HTTP GET */
            jQuery.ajax({
                url: url,
                context: document.body,
                success: function(data, status, jqXHDR) {
                    var contentType = jqXHDR.getResponseHeader('Content-type');
                    if (contentType == null) {
                        contentType = "text/html";
                    }
                    if (contentType.match(/^text\/(html|xml)(;.*)?$/i)) {
                        d.html(jqXHDR.responseText);

                    } else if (contentType.match(/^application\/json(;.*)?$/i)) {
                        ASF.Execute(d, data);
                    }
                    if (complete != null) {
                        complete(d, jqXHDR);
                    }
               }
            });
        }
    };

    /**
     * Submit the current form.  The <b>node</b> element refers to the element being clicked.
     * From that element, we can identify the form to submit.  The element to refresh upon
     * the form submit is an outer element that should have the <b>asf-container</b> class.
     * If not such element as parent node, the parent node of the form is used.
     *
     * After the form is submitted, the response is executed if it is a JSON response.
     * If the response is an HTML content, the container element is refreshed with it.
     *
     * @param node the current element
     * @param url the URL to fetch using an HTTP GET
     * @param target the optional target element
     */
    ASF.Submit = function(node) {
        /* Find the form and container to update */
        var f = $(node).closest("form");
        var d = $(f);
        if (!d.is('.asf-container,ui-dialog-content')) {
            d = d.closest('.asf-container,.ui-dialog-content');
        }
        if (d.length == 0) {
            d = $(f).parent();
        }
        if (d.length > 0) {
            var params;
            var params = $(f).serialize();
            var url = $(f).attr('action');

            if (node.tagName == 'a' || node.tagName == 'A') {
                params = node.id + "=1&" + $(f).serialize();
            } else {
                params = node.name + "=1&" + $(f).serialize();
            }
            /* Perform the HTTP POST */
            jQuery.ajax({
                type: "POST",
                url: url,
                data: params,
                context: document.body,
                success: function(data, status, jqXHDR) {
                    var contentType = jqXHDR.getResponseHeader('Content-type');
                    if (contentType == null) {
                        contentType = "text/html";
                    }
                    if (contentType.match(/^text\/(html|xml)(;.*)?$/i)) {
                        d.fadeOut("fast", function() {
                            d.html(jqXHDR.responseText);
                            d.fadeIn("fast");
                        });

                    } else if (contentType.match(/^application\/json(;.*)?$/i)) {
                        ASF.Execute(d, data);
                    }
               }
            });
        }
        return false;
    };

    /**
     * Open a dialog box and fetch the dialog content from the given URI.
     *
     * @param node the current element
     * @param id the dialog id
     * @param url the URL to fetch using an HTTP GET
     * @param params the optional dialog options
     */
    ASF.OpenDialog = function(node, id, url, params) {
        var div = $(id);
        if (div.length > 0) {
            return false;
        }

        div = document.createElement("div");
        div.id = id;
        div = $(div);

        var d = $(document);
        if (d.length == 0) {
            return false;
        }

        $(d).append($(div));

        /* Perform the HTTP GET */
        jQuery.ajax({
            url: url,
            context: document.body,
            success: function(data, status, jqXHDR) {
                var contentType = jqXHDR.getResponseHeader('Content-type');
                if (contentType == null) {
                    contentType = "text/html";
                }
                if (contentType.match(/^text\/(html|xml)(;.*)?$/i)) {
                    $(div).dialog({
                        autoOpen: false,
                        show: "blind",
                        hide: "explode",
                        minWidth: 600,
                        close: function() {
                            $(div).dialog('destroy');
                            $(div).remove();
                        }
                     });

                     $(div).html(jqXHDR.responseText);
                     var dTitle = $(div).children('div').attr('title');
                     if (dTitle != null) {
                        $(div).dialog("option", "title", dTitle );
                        /* $(div).attr('title', title); */
                     }
                     $(div).dialog('open');

                } else if (contentType.match(/^application\/json(;.*)?$/i)) {
                    ASF.Execute(node, data);
                }
            }
        });
        return false;
    };

    /**
     * Close the dialog box identified by the given identifier.
     *
     * @param id the dialog box identifier
     */
    ASF.CloseDialog = function(id) {
        $('#' + id).dialog('close');
        return false;
    };

})();

