/*
 *  asf-message -- Message component
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

    /**
     * The message component is a small popup element intended to display some message.
     */
    $.widget("ui.message", {
        options: {
            attachment: null,
            css: null
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
			    this.options.css = value.
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
                var offset = $(this.options.attachment).offset();
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

            this.element.delay(1000).fadeOut('slow', function() {
                self.element.remove();
            });
        }
    });

})( jQuery );