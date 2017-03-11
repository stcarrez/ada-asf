/*
 *  asf -- Ada Server Faces
 *  Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015, 2016, 2017 Stephane Carrez
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

/**
 * @class Ada Server Faces namespace.
 */
var ASF = {};

(function () {
    ASF.Actions = {
        update: function(id, action) {
            $(id).html(action.data);
        },
        prepend: function(id, action) {
            $(id).prepend(action.data);
        },
        append: function(id, action) {
            $(id).append(action.data);
        },
        show: function(id, action) {
            $(id).show('slow');
        },
        hide: function(id, action) {
            $(id).hide('slow');
        },
        "delete": function(id, action) {
            $(id).remove();
        },
        removeClass: function(id, action) {
            $(id).removeClass(action.data);
        },
        addClass: function(id, action) {
            $(id).addClass(action.data);
        },
        fadeIn: function(id, action) {
            $(id).fadeIn('slow');
        },
        fadeOut: function(id, action) {
            $(id).fadeOut('slow');
        },
        slideUp: function(id, action) {
            $(id).slideUp('slow');
        },
        slideDown: function(id, action) {
            $(id).slideDown('slow');
        },
        closeDialog: function(id, action) {
            $(id).dialog('close');
        },
        closePopup: function(id, action) {
            $(id).popup('close');
        },
        redirect: function(id, action) {
            document.location = action.url;
        },
        message: function(id, action, node) {
            ASF.Message(action.attach ? action.attach : node, action.id, action.data);
        },
        notification: function(id, action, node) {
	        ASF.Message(action.attach ? action.attach : node,
	                    action.id, action.data, 'asf-notification').message('autoClose');
        },
        get: function(id, action) {
            ASF.Update(null, action.url, id);
        },
        script: function(id, action) {
            try {
                eval(action.script);
            } catch (e) {
                alert(e);
            }
        },
        clear: function(id, action) {
	        $(id).each(function() {
                switch (this.type) {
                case 'password':
                case 'select-multiple':
                case 'select-one':
                case 'text':
                case 'textarea':
                    $(this).val('');
                    break;
                case 'checkbox':
                case 'radio':
                    this.checked = false;
                    break;
                }
            });
        }
    };

    /**
     * Execute the AJAX response action represented by the JSON object <b>action</b>.
     *
     * @param node the current node
     * @param action the JSON action object
     */
    ASF.ExecuteOne = function(node, action) {
        var id = action.id ? action.id : node;
        if (action.child) {
            id = $(id).children(action.child);
        }
        if (ASF.Actions.hasOwnProperty(action.action)) {
            var handler = ASF.Actions[action.action];
            handler(id, action, node);
        }
    };

    ASF.Execute = function(node, data) {
        if (data) {
            for (var i = 0; i < data.length; i++) {
                ASF.ExecuteOne(node, data[i]);
            }
        }
    };

    /**
     * Handle an AJAX operation that failed.
     */
    ASF.AjaxError = function(jqXHDR, status, error, node) {
        var contentType = jqXHDR.getResponseHeader('Content-type');
        if (contentType && contentType.match(/^application\/json(;.*)?$/i)) {
            var data = $.parseJSON(jqXHDR.responseText);
            ASF.Execute(node, data);
        } else {
            ASF.Message(node, null, "Operation failed");
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
               },
               error: function(jqXHDR, status, error) {
                   ASF.AjaxError(jqXHDR, status, error, d);
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
            /**
             * Send notification on form to indicate a form submit is now in progress.
             */
            $(f).triggerHandler("notifySubmit", d);

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
                    $(f).triggerHandler("successSubmit", d);
                    if (contentType.match(/^text\/(html|xml)(;.*)?$/i)) {
                        d.fadeOut("fast", function() {
                            d.html(jqXHDR.responseText);
                            d.fadeIn("fast");
                        });

                    } else if (contentType.match(/^application\/json(;.*)?$/i)) {
                        ASF.Execute(d, data);
                    }
               },
               error: function(jqXHDR, status, error) {
                   ASF.AjaxError(jqXHDR, status, error, d);
               }
            });
        }
        return false;
    };

    /**
     * Perform an HTTP POST on the given URL.
     *
     * @param node the node element that will be updated by default on the AJAX response.
     * @param url the HTTP URL.
     * @param params the HTTP POST parameters.
     */
    ASF.Post = function(node, url, params) {
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
                    $(node).html(jqXHDR.responseText);

                } else if (contentType.match(/^application\/json(;.*)?$/i)) {
                    ASF.Execute(node, data);
                }
            },
            error: function(jqXHDR, status, error) {
                ASF.AjaxError(jqXHDR, status, error, node);
            }
        });
        return false;
    };

    /**
     * Open a popup form attached to the node element and which is filled by
     * the given URL.
     *
     * The popup form is created when the page refered by <tt>url</tt> is fetched.
     * It can be referenced by the unique id given in <tt>id</tt>.
     *
     * @param node the node element where the popup is attached to.
     * @param id the popup unique id.
     * @param url the URL to fetch to fill the popup content.
     * @param params the popup creation parameters.
     */
    ASF.Popup = function(node, id, url, params) {
        jQuery.ajax({
            type: "GET",
            url: url,
            context: document.body,
            success: function(data, status, jqXHDR) {
                var contentType = jqXHDR.getResponseHeader('Content-type');
                if (contentType == null) {
                    contentType = "text/html";
                }
                var div = $('#' + id);
                if (div.length == 0) {
                    div = document.createElement("div");
                    div.id = id;
                    $(document.body).append($(div));
                }
                if (contentType.match(/^text\/(html|xml)(;.*)?$/i)) {
                    $(div).html(jqXHDR.responseText);

                } else if (contentType.match(/^application\/json(;.*)?$/i)) {
                    ASF.Execute(div, data);
                }
                $(div).popup(params).popup('open');
                $(div).find('form:not(.filter) :input:visible:first').focus();
            },
            error: function(jqXHDR, status, error) {
                ASF.AjaxError(jqXHDR, status, error, node);
            }
        });
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

        var d = $(document.body);
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
                            if (params && params.close) {
                                params.close(node);
                            }
                        }
                     });

                     $(div).html(jqXHDR.responseText);

                     /**
                      * Extract a title from the inner form to setup the dialog box.
                      */
                     var dTitle, dBox = $(div).children('div');
                     if (dBox.length == 0) {
                        dTitle = $(div).children('h2');
                     } else {
                        dTitle = $(dBox).children('h2');
                     }
                     if (dTitle.length > 0) {
                        $(div).dialog("option", "title", dTitle.html());
                        dTitle.remove();
                     } else {
                        dTitle = $(div).children('div').attr('title');
                        if (dTitle != null) {
                            $(div).dialog("option", "title", dTitle );
                            /* $(div).attr('title', title); */
                        }
                     }
                     $(div).dialog('open');

                } else if (contentType.match(/^application\/json(;.*)?$/i)) {
                    ASF.Execute(node, data);
                }
            },
            error: function(jqXHDR, status, error) {
                ASF.AjaxError(jqXHDR, status, error, d);
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

    /**
     * Create and display a popup message near the node element.
     *
     * @param node the element giving the location of the message
     * @param id the message id or null
     * @param message the HTML message content
     * @param css the optional CSS to set on the message content.
     */
    ASF.Message = function(node, id, message, css) {
        if (!id) {
            id = "#asf-message";
        }
        var div = $(id);
        if (div.length == 0) {
            div = document.createElement("div");
            div.id = id.substring(1);
            $(document.body).append($(div));
        } else {
            $(div).removeClass();
        }
        if (css) {
            $(div).addClass(css);
        }
        return $(div).html(message).message({}).message("attachment", node);
    };
})();

