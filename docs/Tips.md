# Tips

## Open dialog box

The `ASF.OpenDialog` Javascript operation can be used to open
a dialog box by fetching its content from the server.
First, create the dialog box file in a separate XHTML file.
The file should start with a `<f:view>` component with a
valid `contentType` attribute.

```
<f:view contentType="text/html; charset=UTF-8"
        xmlns:f="http://java.sun.com/jsf/core"
        xmlns:w="http://code.google.com/p/ada-asf/widget"
        xmlns:h="http://java.sun.com/jsf/html">
  <div>
    <h:form id='subscribe-send'>
...
      <ul class='awa-buttons'>
        <li>
          <h:commandButton id='send-mail'
             title="..."
             value='...'
             styleClass="ui-button ui-state-default ui-corner-all"
             action="#{action}"/>
        </li>
      </ul>
    </h:form>
  </div>
</f:view>
```

The `ASF.OpenDialog` is simply called to trigger the opening of the dialog.
The second parameter shoudl be the name of the dialog box JavaScript variable.
The last parameter is the URL to fetch to get the dialog box content.

```
<a class='awa-button' href="#"
   onclick="return ASF.OpenDialog(this, 'openDialog', '#{contextPath}/');">
   Open Dialog
</a>
```

## ASF Actions

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

```
{
  "action": "update",
  "id": "<name>",
  "data": "<content>"
}
```


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

```
{
  "action": "fadeIn",
  "id": "<name>"
}
```


### Updating CSS class

* addClass to add a CSS class to some DOM components,
* removeClass to remove a CSS class to some DOM components.

```
{
  "action": "addClass",
  "id": "<name>",
  "data": "<class-name>"
}
```

### Redirect

The redirect action can be used to redirect the browser to a new
page.  The redirection page is defined by the url attribute.

```
{
  "action": "redirect",
  "url": "<redirection-url>"
}
```

### Get content

The redirect action can be used to redirect the browser to a new
page.  The redirection page is defined by the url attribute.

```
{
  "action": "get",
  "id": "<name>",
  "url": "<get-url>"
}
```

### clear action

TBW
