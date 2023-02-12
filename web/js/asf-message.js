/*
 *  asf-message -- Message component
 *  Copyright (C) 2013, 2018, 2023 Stephane Carrez
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
 * @namespace ui.message
 * @class ui.message
 */
(function($, undefined) {

    /**
     * @class ui.message
     * @extends ui.widget
     *
     * The message component is a small popup element intended to display some message.
     * The component onto which it is applied is turned into a popup element.  The popup
     * is closed automatically after a delay or if the user clicks on the popup element.
     */
    var proto = {
        options: {
	    /**
	     * @cfg {Object} attachment The attachment object.
	     */
            attachment: null,

	    /**
	     * @cfg {String} css The CSS to apply on the popup div.
	     */
            css: null,

            /**
             * @cfg {String} delay The delay in milliseconds before closing.
             */
            delay: 2000
        },

        _create: function() {
            var self = this;

            this.element.bind('click', function(event) {
                return self.click(event);
            });

            this.element.addClass("asf-message");

            if (this.options.attachment) {
                this.attachment(this.options.attachment);
            }
        },

	    _setOption: function(key, value){
		    switch (key) {
			case "css":
			    this.options.css = value;
                this.element.addClass("asf-message " + value);
			    break;

            case "triggerHandler":
                self.options.triggerHandler = value;
                break;
			}
			$.Widget.prototype._setOption.apply(this, arguments);
	    },

        /**
         * Set the attachment node.
         *
         * The message popup is moved near the attachment node.
         * Display the message if it is hidden.
         */
        attachment: function(node) {
            this.options.attachment = $(node);

            this.element.show('fast');
            if (this.options.attachment) {
               let itemWidth = $(this.element).width();
               let width = window.innerWidth;
               let offset = $(this.options.attachment).offset();
               if (offset.left + itemWidth > width) {
                  offset.left = width - itemWidth;
               }
               this.element.offset(offset);
            }
        },

        /**
         * Click action called when the user clicks on the message.
         * Hide the message.
         *
         * @param event the click event
         */
        click: function(event) {
            this.element.hide('fast');
        },

        /**
         * Schedule a close and removal of the message after a delay.
         */
        autoClose: function() {
            var self = this;

            this.element.delay(self.options.delay).fadeOut('slow', function() {
                self.element.remove();
            });
        }
    };

    $.widget("ui.message", proto);

})( jQuery );
