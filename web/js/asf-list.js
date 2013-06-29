/*
 *  asf-list -- List helpers
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

    $.widget( "ui.list", $.ui.mouse, {
        currentNode: null,

        /**
         * The active or highlighted item in the list.  This is the item which has the action element
         * as last child.
         */
        activeItem: null,

        /**
         * Indicates whether the bloc is active (has the mouse focus).
         */
        isActive: false,

        /**
         * The current item
         */
        currentItem: null,

        /**
         * The current inline edit node.
         */
        currentEdit: null,
        options: {
            min: 0,

            /**
             * The CSS class that is set on active elements.
             */
            activeClass: "asf-active",

            /**
             * The CSS class that is set on the list element when it has the focus.
             */
            activeListClass: "asf-active-list",

            /**
             * The CSS class that is set on the selected element.
             */
            selectClass: "asf-selected",

            /**
             * The CSS class that indicates an element is being deleted.
             */
            deletingClass: "asf-deleting",

            /**
             * URL to edit an item of the list.
             */
            editUrl: null,

            /**
             * URL to delete an item of the list.
             */
            deleteUrl: null,

            /**
             * Relative URL to refresh the item (after edit or view mode for example).
             */
            refreshUrl: null,

            /**
             * Disable the mouseover effect.  When set to 'true', the mouseover effect implemented by
             * setting the 'active' CSS class on the item is disabled.
             */
            disableMouseover: false,

            /**
             * Prefix for item ids in the DOM tree.
             */
            itemPrefix: '',

            /**
             * A function which is called when an element of the list is selected.
             */
            selectAction: null,

            /**
             * The optional ID of a DOM element that must be re-parented under the active
             * element when the mouse is entered in the element.  This DOM element is intended to
             * contain a set of actions that a user can make on the active element.
             * The action ID must NOT start with the itemPrefix.
             */
            actionId: null
        },

        _create: function() {
            var self = this;

            $.ui.mouse.prototype._create.apply(this, arguments);
            this.element.addClass("ui-list");
            this.element.bind('click', function(event) {
                return self.click(event);
            }).bind('blur', function(event) {
                return self.blur(event);
            });
            this.setMouseOver(!this.options.disableMouseover);

            /**
             * Get the action element (a div in most cases) which contains the actions
             * to be displayed for the active/highlighted element.
             */
            this.action = $(this.options.actionId);
            if (this.options.currentItem != null) {
                this.selectAction($(this.options.currentItem));
            }
        },

        /**
         * Enable or disable the mouseover effect on the full list.
         *
         * @param status when true, enable the mouseover effect.
         */
        setMouseOver: function(status) {
            var self = this;

            if (status == true) {
                this.element.bind('mouseover', function(event) {
                    return self.mouseOver(event);
                }).bind('mouseout', function(event) {
                    return self.mouseOut(event);
                });
            } else {
                this.element.unbind('mouseover');
                this.element.unbind('mouseout');
            }
            this.mouseOverEnable = status;
        },

        /**
         * Select the list item identified by <tt>node</tt> as the current selected item.
         *
         * @param node the list item to select
         */
        selectAction: function(node, event) {
            if (this.currentItem != null) {
                $(this.currentItem).removeClass(this.options.selectClass);
            }
            this.currentItem = node;
            $(this.currentItem).addClass(this.options.selectClass);
            if (this.options.selectAction != null) {
                this.options.selectAction(this, node);
            }
            return false;
        },

        /**
         * Set the active item.  The active item is marked with the 'active' class and kept as reference
         * in the 'activeItem' member.
         *
         * @param item the active item or null to disable any active item
         */
        setActiveItem: function(item) {
            if (this.activeItem != null && this.activeItem[0] != item) {
                // Ensure that previously active line is deactivated.
                this.activeItem.removeClass(this.options.activeClass);
            }
            if (item != null) {
                this.activeItem = $(item);
                this.activeItem.addClass(this.options.activeClass);
            } else {
                this.activeItem = null;
            }
        },

        /**
         * Get the object ID of the selected node.  This operation assumes that the HTML element has
         * an id of the form 'itemPrefix' + object Id.  This operation removes the item prefix.
         *
         * @param node the node element that was selected
         * @return the ID of the object
         */
        getSelectedId: function(node) {
            if (node == null) {
                return null;
            }
            var id = $(node).attr('id');
            return id == null ? null : id.substring(this.options.itemPrefix.length);
        },

        /**
         * Find the parent node that should receive the event.
         *
         */
        _getTargetNode: function(node) {
            while (node) {
                var name = node.tagName;
                if (name) {
                    name = name.toUpperCase();
                    if (name != "EM" && name != "I" && name != "B" && name != "IMG" && name != "SPAN") {
                        if (node.id && node.id != "" && this.action[0] != node) {
                            return node;
                        }
                        if (name == 'A') {
                            return node;
                        }
                    }
                }
                if (node == this.element[0]) {
                    return null;
                }

                node = node.parentNode;
            }
            return null;
        },

        /**
         * Get the item list node that contains the inner element represented by <tt>node</tt>.
         * Returns null if the node element is not part of a list item component.
         */
        getTarget: function(node) {
            if (node == this.element[0]) {
                return null;
            }
            while (node) {
                var name = node.tagName;
                if (name) {
                    name = name.toUpperCase();
                    if (name == "DIV" || name == "DL") {
                        var id = node.id;
                        if (id && id.indexOf(this.options.itemPrefix) === 0
                            && id != this.options.actionId) {
                            return node;
                        }
                    }
                }
                node = node.parentNode;
                if (node == this.element[0]) {
                    return null;
                }
            }
            /* $("#current").html(node.id); */
            return node;
        },

        /**
         * Mouse over callback invoked when the mouse over event is received on a list element.
         *
         * @param event the mouse over event
         */
        mouseOver: function(event) {
            /*
             * We want to know when a new list item is selected.  Find the list item which has the focus.
             */
            var node = this.getTarget(event.target);
            if (node && this.currentNode != node) {
                /* $("#current").html("Mover " + node.id + " Cnt=" + this.counter
                + " T=" + event.target.id); */
                this.setActiveItem(node);
                if (this.action[0]) {
                    this.action.detach();
                    this.action.prependTo(this.activeItem);
                }
                this.currentNode = node;
            }
            if (this.isActive == false) {
                this.isActive = true;
                this.element.addClass(this.options.activeListClass);
            }
        } ,

        /**
         * Mouse out callback invoked when the mouse out event is received on a list element.
         *
         * @param event the mouse out event
         */
        mouseOut: function(event) {
            if (!this.disableMouseover) {
                var node = event.target;
                while (node && node != document) {
                    if (node == this.activeItem) {
                        this.setActiveItem(null);
                        /* $("#current").html("Mouse out"  + " Cnt=" + this.counter);*/
                        break;
                    } else {
                        if (node == this.element[0]) {
                            break;
                        }
                        node = node.parentNode;
                    }
                }
                if (node && node != document) {
                    this.isActive = false;
                    this.element.removeClass(this.options.activeListClass);
                }
            }
        },

        /**
         * Get the URL to execute an AJAX operation on the given item.  Override this operation to customize
         * the URL.
         *
         * @param item the item
         * @param id the item identifier
         * @param url the relative URL
         * @return the URL to invoke an AJAX operation on the given item
         */
        getOperationUrl: function(item, id, url) {
            return url ? url + id : null;
        },

        /**
         * Enter the edit mode for the current active list element.
         */
        enterEdit: function(event) {
            var url, item = this.activeItem,
                id = this.getSelectedId(item);
            if (id) {
                url = this.getOperationUrl(item, id, this.options.editUrl);
                if (url) {
                    ASF.Update(item, url, item);
                }
            }
            return false;
        },

        /**
         * Enter delete mode for the current active element.
         * The 'deletingCss' is applied on the element being deleted.  The delete dialog box is opened
         * to confirm the deletion of our item.  The 'deletingCSS' is removed once the dialog box is closed.
         */
        enterDelete: function(event) {
            var url, css,
                item = this.activeItem,
                id   = this.getSelectedId(item);
            if (id) {
                url = this.getOperationUrl(item, id, this.options.deleteUrl);
                if (url) {
                    css = this.options.deletingClass;

                    item.addClass(css);
                    ASF.OpenDialog(item, 'deleteDialog_' + id, url, {
                        close: function() {
                            item.removeClass(css);
                        }
                    });
                }
            }
            return false;
        },
        enterCreate: function(event) {

        },

        /**
         * Click action executed when one inner HTML element is clicked.
         *
         * @param event the event that was clicked
         */
        click: function(event) {
            var name, node = this._getTargetNode(event.target);
            if (node) {

                if ($(node).hasClass("asf-edit")) {
                    this.enterEdit(event);
                } else if ($(node).hasClass("asf-delete")) {
                    this.enterDelete(event);

                } else if ($(node).hasClass("asf-editable")) {
                    if (! $(node).hasClass("asf-editing")) {
                        this.currentEdit = node;
                        $(node).addClass("asf-editing");
                        this.enterCreate(event);
                    }
                } else {
                    this.selectAction(node, event);
                }
            } else {
                name = event.target.tagName;
                node = event.target;

                if ($(node).hasClass("asf-edit")) {
                    this.enterEdit(event);
                } else if ($(node).hasClass("asf-delete")) {
                    this.enterDelete(event);
                }

            }
            event.stopPropagation();
        },
        blur: function(event) {
            if (this.currentEdit) {
                this.currentEdit.remove();
                this.currentEdit = null;
            }
        },
        destroy: function() {
            this.element.removeClass("ui-list");
            $.Widget.prototype.destroy.apply(this, arguments);
        }

    });
})( jQuery );
