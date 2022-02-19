# Util Components
The `util` components are specific to Ada Server Faces and they are provided to help in
writing server faces pages.  The component is defined in the following namespace:

```
xmlns:util="http://code.google.com/p/ada-asf/util"
```

## util:escape

Render the inner component children and escape the resulting HTML text using
Javascript or XML escape rules.  Using the `util:escape` component is useful
when rendering a Javascript extract, a Json response or some XML content.

### Attributes

| Name      | Required | Type            | Description                                        |
| --------- | -------- | --------------- | -------------------------------------------------- |
| mode      | false    | String          | When set to `xml`, use the XML escape rules to escape the content. Otherwise, use Javascript escape rules. |
| rendered  | false | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |


### Example

```
<div xmlns:util="http://code.google.com/p/ada-asf/util">
    <util:script>
        var code = "<util:escape>This is a javascript message.
        <b>It can be inserted in an HTML element.</b>
        <p>
            It can contain HTML code as well as special characters
            such as quotes (' or ")
        </p>
    </util:escape>";

        $('#code-raw').html(code);
    </util:script>

    <div id='code-raw'/>

</div>
```

## util:file

This component allows to include an external file in the render response phase.

### Attributes

| Name      | Required | Type            | Description                                        |
| --------- | -------- | --------------- | -------------------------------------------------- |
| src       | true     | String          | The relative path for the file to be included. |
| rendered  | false | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |


### Example

```
<div xmlns:util="http://code.google.com/p/ada-asf/util"
     xmlns:h="http://java.sun.com/jsf/html">
    <h:form>
        <input type="submit" name="file" value="Display 'file.xhtml'"
               onclick="return ASF.Submit(this);"/>
        <input type="submit" name="escape" value="Display 'escape.xhtml'"
               onclick="return ASF.Submit(this);"/>
        <div  style='overflow: hidden; width: 100%;'>
            <code>
                <util:file src="/util/file.xhtml" escape="true"
                           rendered="#{param['file'] eq '1'}"/>
                <util:file src="/util/escape.xhtml" escape="true"
                           rendered="#{param['file'] ne '1'}"/>
            </code>
        </div>
    </h:form>
</div>
```

## util:flush

This component is used in the render response phase only.
It flushes the javascript code that has been queued either by some component
or by the `util:script` tag.  This allows to flush the javascript at
well known places.  When `response` is specified and true, it also
flushes the response stream.


### Attributes

| Name      | Required | Type            | Description                                        |
| --------- | -------- | --------------- | -------------------------------------------------- |
| response  | false | Boolean         | Flag indicating whether the response stream must be flushed. |


### Example

```
<div xmlns:util="http://code.google.com/p/ada-asf/util">
    <!-- A first javascript piece to fade out the block
         and update its content  -->
    <util:script>
        $('#code-raw').fadeOut('slow', function() {
            var code = "Fade in code";
            $('#code-raw').html(code).fadeIn();
        });
    </util:script>
    <div id='code-raw'/>
    <div id='code-raw-2'/>
    <util:script>
        $('#code-raw').fadeOut('slow', function() {
            var code = "<util:escape>Code</util:escape>";

            $('#code-raw-2').html(code).fadeIn();
        });
    </util:script>

    <!-- Javascript enclosed by <util:script> generated here -->
    <util:flush/>
</div>
````

## util:script

In the render response phase, queue some Javascript code in the response stream
or queue a Javascript file inclusion.
The Javascript code is automatically flushed before sending the response stream.
It can be flushed explicitly by using the `util:flush` component.

### Attributes

| Name      | Required | Type            | Description                                        |
| --------- | -------- | --------------- | -------------------------------------------------- |
| async     | false | Boolean         | Flag indicating whether the external Javascript file is loaded asynchronously. |
| src       | false | String          | When not empty, render the inclusion of an external Javascript file. The Javascript file location is defined by the src attribute.  The Javascript file inclusions are rendered before all javascript code by the `util:flush` component. |
| rendered  | false | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |


### Example

```
<div xmlns:util="http://code.google.com/p/ada-asf/util">
    <!-- A first javascript piece to fade out the block
         and update its content  -->
    <util:script>
        $('#code-raw').fadeOut('slow', function() {
            var code = "<util:escape>Code appears</util:escape>";

            $('#code-raw').html(code).fadeIn();
        });
    </util:script>

    <div id='code-raw'/>

    <util:flush/>
</div>
```


## util:set

Sets a value on a manage bean attribute.  The `var` attribute is a Value expression
that describe a manage bean attribute to set.  The `value` attribute corresponds to
the value that will be assigned to the value expression.  This allows to invoke
the `Set_Value` method of the managed bean to assign a named value.

### Attributes

| Name  | Type            | Description                                        |
| ----- | --------------- | -------------------------------------------------- |
| var   | ValueExpression | Value expression to set.                           |
| value | any             | Value to assign.                                   |

### Example

```
<html xmlns:h="http://java.sun.com/jsf/html"
      xmlns:c="http://java.sun.com/jstl/core"
      xmlns:util="http://code.google.com/p/ada-asf/util">
  <body>
    <util:set var="#{form.email}" value="Potter@gmail.com" />
    <util:set var="#{form.name}" value="Harry" />
    Email:  #{form.email}
    Name: #{form.name}
  </body>
</html>
```


