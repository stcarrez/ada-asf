### Updating content

Several actions are available to update the content of
some DOM components:

* The update action replaces a complete DOM content,
* The prepend action adds some content before a DOM node,
* The append action adds some content after a DOM node.

The DOM component is identified by a jQuery
identication string and passed in the `id` attribute.
The content to replace, prepend or append is specified
in the `data` attribute which can contain HTML tags.

{
  "action": "update",
  "id": "<name>",
  "data": "<content>"
}


### Hide or show

Several actions are available to hide, show and provide visual
effects when displaying or hiding some component.

* show to make a DOM component visible,
* hide to make a DOM component invisible,
* fadeIn to show a component after a fade-in visual effect,
* fadeOut to hide a component after a fade-out visual effect,
* slideUp to slide a component up,
* slideDown to slide a component down.

The id attribute is used to defined the DOM component onto
which the action is made.

{
  "action": "fadeIn",
  "id": "<name>"
}


### Updating CSS class

* addClass to add a CSS class to some DOM components,
* removeClass to remove a CSS class to some DOM components.

{
  "action": "addClass",
  "id": "<name>",
  "data": "<class-name>"
}

### Redirect

The redirect action can be used to redirect the browser to a new
page.  The redirection page is defined by the url attribute.

{
  "action": "redirect",
  "url": "<redirection-url>"
}

### Get content

The redirect action can be used to redirect the browser to a new
page.  The redirection page is defined by the url attribute.

{
  "action": "get",
  "id": "<name>",
  "url": "<get-url>"
}

### clear action
