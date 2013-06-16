/*  asf-popup -- Popup Component
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
    $.widget("ui.popup", {
        options: {
            /**
             * Defines the optional element where the popup is attached.
             */
            attachment: null,

            /**
             * A handler that gets called when the popup is opened or closed.
             */
            triggerHandler: null
        },
        keyPressHandler: null,
        clickHandler: null,

        _create: function() {
            var self = this;

            this.element.addClass("asf-popup");

            if (this.options.attachment) {
                this.attachment(this.options.attachment);
            }

            this.clickHandler = function(event) {
                if (!$(event.target).closest('.asf-popup').length) {
                    self.close();
                }
            };
            this.keyPressHandler = function(event) {
                if (event.keyCode == 27) {
                    self.close();
                }
            };
        },

	    _setOption: function(key, value){
		    var self = this;

		    switch (key) {
			case "attachment":
			    self.attachment(value);
			    break;

            case "triggerHandler":
                self.options.triggerHandler = value;
                break;
			}
			$.Widget.prototype._setOption.apply(self, arguments);
	    },

        /**
         * Set the attachment node.
         *
         * The message popup is moved near the attachment node.
         */
        attachment: function(node) {
            this.options.attachment = $(node);

            if (this.options.attachment) {
                var props = {},
                    offset = $(this.options.attachment).offset();
                props.top = offset.top;
                props.left = offset.left;
                $(this.element).css(props);
            }
        },

        /**
         * Open the popup and install the Escape key and click handlers
         * on the document to close the popup.
         */
        open: function() {
            var self = this;

            this.element.show("fast", function() {
                if (self.options.triggerHandler) {
                    self.options.triggerHandler("open");
                }

                /**
                 * Catch Escape and click outside of popup to hide it.
                 */
                $(document).bind('keyup', self.keyPressHandler).bind('click', self.clickHandler);
            });
        },

        /**
         * Close the popup.
         */
        close: function() {
            var self = this;

            this.element.hide("fast", function() {
                if (self.options.triggerHandler) {
                    self.options.triggerHandler("close");
                }
                $(document).unbind('keyup', self.keyPressHandler).unbind('click', self.clickHandler);
            });
        },

        destroy: function() {
            this.element.removeClass("asf-popup");
            $.Widget.prototype.destroy.apply(this, arguments);
        }
    });

})( jQuery );