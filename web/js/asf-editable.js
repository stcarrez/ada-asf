/*
 *  asf-editable -- Editable component
 *  Copyright (C) 2012 Stephane Carrez
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
     * The editable component enters in edit mode when the user clicks on it.
     * The editing mode is activated by the 'asf-editing' CSS which puts the component in editing mode
     * and makes the input fields visible.
     */
    $.widget("ui.editable", {
        options: {

        },
        editing: false,

        _create: function() {
            var self = this;

            this.element.bind('click', function(event) {
                return self.click(event);
            });

            var f = $(this.element).closest('form');
            $(f).bind("successSubmit", function(d) {
                return self.finishEdit(null);
            });
        },

        /**
         * Finish the edition by removing the asf-editing CSS and turning the component in view only mode.
         */
        finishEdit: function(node) {
            this.element.removeClass("asf-editing");
            this.editing = false;
            return false;
        },

        /**
         * Blur action called when the component looses the focus.
         * The 'asf-editing' CSS is removed from the element and the element goes back in normal display mode.
         */
        blur: function(event) {
            if (this.editing == true) {
                var node = $(event.currentTarget).closest(".asf-editing");
                if (node.length == 0) {
                   this.finishEdit(null);
                   return false;
                }
            }
            return true;
        },

        /**
         * Click action called when the user clicks on the element.
         * This activates the editing mode of the component by setting the 'asf-editing' CSS class
         * and setting the input focus on the first input field.
         *
         * @param event the click event
         */
        click: function(event) {
            var self = this;

            if (this.editing == false) {
                this.editing = true;
                this.element.addClass("asf-editing");
                var node = this.element.find("input[type = 'text']");
                $(node).focus();
                $(node).keypress(function (e) {
                    var keycode = e.keyCode || e.which;
                    if (keycode == 13 && self.editing) {
                        // The Enter key was pressed.  Fire the submit action.
                        e.preventDefault();
                        ASF.Submit(self.element.find("input[type = 'submit']")[0]);
                        return false;
                    } else {
                        // Pass through all other keypresses.
                        return true;
                    }
                }).bind("blur", function(event) {
                    return self.blur(event);
                });
            }
        }
    });

})( jQuery );