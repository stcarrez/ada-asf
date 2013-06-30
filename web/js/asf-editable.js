/*
 *  asf-editable -- Editable component
 *  Copyright (C) 2012, 2013 Stephane Carrez
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
     * The editfield component switches the content of a <div> element to an <input> field
     * that can be edited.  The <tt>enterEdit</tt> operation switch the <div> element to
     * the <input> field and sets the ".asf-editing" CSS class on the element.
     * When <tt>finishEdit</tt> operation is called, the input value is compared to
     * the original and the <tt>onChange</tt> operation is called.  The ".asf-editing" CSS
     * class is removed and the element is restored (ie, the input field is removed).
     */
    $.widget("ui.editfield", {
        options: {
            /**
             * Operation called when the component has finished to be edited and the value changed.
             */
            onChange: null,
        },
        originalValue: null,
        inputField: null,
        keyPressHandler: null,
        clickHandler: null,

        _create: function() {
            var self = this;

            this.clickHandler = function(event) {
                if (!$(event.target).closest('.asf-editing').length) {
                    self.finishEdit();
                }
            };
            this.keyPressHandler = function(event) {
                if (event.keyCode == 27) {
                    self.cancelEdit();
                } else if (event.keyCode == 13) {
                    self.finishEdit();
                }
            };
        },

	    _setOption: function(key, value){
		    var self = this;

		    switch (key) {
			case "onChange":
			    self.options.onChange = value;
			    break;
			}
			$.Widget.prototype._setOption.apply(self, arguments);
	    },

        createInput: function() {
            this.inputField = document.createElement("input");
        },

        /**
         * Enter the edit mode for the item.
         *
         */
        enterEdit: function() {
            this.originalValue = this.element.text();
            this.element.addClass("asf-editing");
            if (this.inputField == null) {
                this.createInput();
            }
            this.inputField.value = this.originalValue;
            this.element.empty();
            this.element.append(this.inputField);
            $(this.inputField).focus();

            /**
             * Catch Escape and click outside of field to leave the edit mode.
             */
            $(document).bind('keyup', this.keyPressHandler).bind('click', this.clickHandler);
        },

        /**
         * Cancel the edit mode leaving the field unchanged.
         */
        cancelEdit: function() {
            this.inputField.value = this.originalValue;
            this.finishEdit();
        },

        /**
         * Finish the edit mode.
         */
        finishEdit: function() {
            $(this.inputField).detach();
            this.element.text(this.inputField.value);
            this.element.removeClass("asf-editing");
            $(document).unbind('keyup', this.keyPressHandler).unbind('click', this.clickHandler);
            if (this.inputField.value != this.originalValue && this.options.onChange != null) {
                this.options.onChange(this.element, this.inputField.value);
            }
        }
    });

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
            }).bind("mouseleave", function(event) {
                return self.finishEdit(null);
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
                });
            }
        }
    });

})( jQuery );