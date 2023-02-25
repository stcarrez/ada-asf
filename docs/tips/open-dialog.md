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
