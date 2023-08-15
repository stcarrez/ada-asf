# HTML Components
The `html` components provide the HTML components.

```
xmlns:h="http://java.sun.com/jsf/html"
```

## h:body

Render an html `body` element.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| onchange               | false    | String          | Javascript code executed when the value of the element changes. |
| onclick                | false    | String          | Javascript code executed when a pointer button is clicked over this element. |
| ondbclick              | false    | String          | Javascript code executed when a pointer button is double clicked over this element. |
| onkeydown              | false    | String          | Javascript code executed when a key is pressed down over this element. |
| onkeypress             | false    | String          | Javascript code executed when a key is pressed or released over this element. |
| onkeyup                | false    | String          | Javascript code executed when a key is released over this element. |
| onmousedown            | false    | String          | Javascript code executed when a pointer button is pressed down over this element. |
| onmousemove            | false    | String          | Javascript code executed when a pointer button is moved within this element. |
| onmouseout             | false    | String          | Javascript code executed when a pointer button is moved away from this element. |
| onmouseover            | false    | String          | Javascript code executed when a pointer button is moved onto this element. |
| onmouseup              | false    | String          | Javascript code executed when a pointer button is released over this element. |
| onload           | false    | String           | Javascript code executed when the user agent finishes loading a window or all frames within a frameset. |
| onunload         | false    | String           | Javascript code executed when the user agent removes a document from a window or frame. |

### Example

```
<f:view xmlns:f="http://java.sun.com/jsf/core"
        xmlns:h="http://java.sun.com/jsf/html"
        contentType="text/html; charset=UTF-8">
    <h:head dir="ltr" lang="en">
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/samples.css"/>
    </h:head>
    <h:body dir="ltr" lang="en">
        <p>Hello world!</p>
    </h:body>
 </f:view>
```
## h:commandButton

Renders an HTML `input` element.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| value            | true     | ValueExpression | The current value of this component. |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| action            | false    | String          | MethodExpression representing the application action to invoke when this component is activated by the user. The expression must evaluate to a public method that takes no parameters, and returns an Object (the toString() of which is called to derive the logical outcome) which is passed to the NavigationHandler for this application. |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| accesskey              | false    | String          | Access key that, when pressed, transfers focus to this element. |
| tabindex               | false    | Natural         | Position of this element in the tabbing order for the current document. This value must be an integer between 0 and 32767. |
| alt              | false    | String           | Alternate textual description of the element rendered by this component. |
| image            | false    | String           | Absolute or relative URL of the image to be displayed for this button. If specified, this "input" element will be of type "image". Otherwise, it will be of the type specified by the "type" property with a label specified by the "value" property. Note that if the value of this attribute starts with "/", the rendered value for this attribute will be prefixed with the context-root for this application. |
| disabled         | false    | Boolean          | Flag indicating that this element must never receive focus or be included in a subsequent submit. A value of false causes no attribute to be rendered, while a value of true causes the attribute to be rendered as disabled="disabled". |
| label            | false    | String           | A localized user presentable name for this component. |
| onblur           | false    | String           | Javascript code executed when this element loses focus. |
| onchange         | false    | String           | Javascript code executed when this element loses focus and its value has been modified since gaining focus. |
| onfocus          | false    | String           | Javascript code executed when this element receives focus. |
| onselect         | false    | String           | Javascript code executed when text within this element is selected by the user. |
| readonly         | false    | String           | Flag indicating that this component will prohibit changes by the user. The element may receive focus unless it has also been disabled. A value of false causes no attribute to be rendered, while a value of true causes the attribute to be rendered as readonly="readonly". |
| type             | false    | String           | Type of button to create. Valid values are "submit", "button", and "reset". If not specified, or not a valid value, the default value is "submit". |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <h:form id='text-form'>
        <dl class='ui-widget container_12'>
            <dt><label for='height'>Height</label><h:message for='height'/></dt>
            <dd>
                <h:inputText id='height' size='10' value='#{compute.height}'
                             styleClass="ui-corner-all">
                    <f:converter converterId="float" />
                </h:inputText>
            </dd>
            <dt><label for='radius'>Radius</label><h:message for='radius'/></dt>
            <dd>
                <h:inputText id='radius' size='10' value='#{compute.radius}'
                             styleClass="ui-corner-all">
                    <f:converter converterId="float"/>
                </h:inputText>
            </dd>
        </dl>
        <ul class='container_12 buttons'>
            <li>
                <h:commandButton id='run' value='Compute' action="#{compute.run}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```
## h:form

Renders the HTML `form` element.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| accept           | false    | String          | List of content types that a server processing this form will handle correctly. |
| acceptcharset    | false    | String          | List of character encodings accepted by the server for this form. |
| enctype          | false    | String          | Content type used to submit the form to the server. If not specified, the default value is `application/x-www-form-urlencoded` |
| validate         | false    | Natural         | The validity duration of the CSRF token created for the form submit protection. |
| expireMessage    | false    | String          | A ValueExpression enabled attribute that, if present, will be used as the text of the expiration message when the form CSRF token has expired. |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| onchange               | false    | String          | Javascript code executed when the value of the element changes. |
| onclick                | false    | String          | Javascript code executed when a pointer button is clicked over this element. |
| ondbclick              | false    | String          | Javascript code executed when a pointer button is double clicked over this element. |
| onkeydown              | false    | String          | Javascript code executed when a key is pressed down over this element. |
| onkeypress             | false    | String          | Javascript code executed when a key is pressed or released over this element. |
| onkeyup                | false    | String          | Javascript code executed when a key is released over this element. |
| onmousedown            | false    | String          | Javascript code executed when a pointer button is pressed down over this element. |
| onmousemove            | false    | String          | Javascript code executed when a pointer button is moved within this element. |
| onmouseout             | false    | String          | Javascript code executed when a pointer button is moved away from this element. |
| onmouseover            | false    | String          | Javascript code executed when a pointer button is moved onto this element. |
| onmouseup              | false    | String          | Javascript code executed when a pointer button is released over this element. |
| onreset          | false    | String          | Javascript code executed when this form is reset. |
| onsubmit         | false    | String          | Javascript code executed when this form is submitted. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <h:form id='text-form'>
        <dl class='ui-widget container_12'>
            <dt><label for='height'>Height</label>
                <h:message for='height'/></dt>
            <dd>
                <h:inputText id='height' size='10'
                             value='#{compute.height}'
                             styleClass="ui-corner-all">
                    <f:converter converterId="float" />
                </h:inputText>
            </dd>
            <dt><label for='radius'>Radius</label>
                <h:message for='radius'/></dt>
            <dd>
                <h:inputText id='radius' size='10'
                             value='#{compute.radius}'
                             styleClass="ui-corner-all">
                    <f:converter converterId="float"/>
                </h:inputText>
            </dd>
        </dl>
        <ul class='container_12 buttons'>
            <li>
                <h:commandButton id='run' value='Compute'
                                 action="#{compute.run}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```
## h:head

Renders an HTML `head` element.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |

### Example

```
<f:view xmlns:f="http://java.sun.com/jsf/core"
        xmlns:h="http://java.sun.com/jsf/html"
        contentType="text/html; charset=UTF-8">
    <h:head dir="ltr" lang="en">
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <link media="screen" type="text/css" rel="stylesheet" href="#{contextPath}/css/samples.css"/>
    </h:head>
    <h:body dir="ltr" lang="en">
        <p>Hello world!</p>
    </h:body>
 </f:view>
```
## h:inputFile

Renders an HTML `input` element of type `file`.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| converterMessage        | false    | String          | A ValueExpression enabled attribute that, if present, will be used as the text of the converter message, replacing any message that comes from the converter. |
| required               | false    | Boolean          | Flag indicating that the user is required to provide a submitted value for this input component. |
| requiredMessage        | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validation message for the "required" facility, if the "required" facility is used. |
| validatorMessage       | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validator message, replacing any message that comes from the validator. |
| value                  | false    | ValueExpression  | The current value of this component. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <div class="ui-widget ui-widget-header">
        Compute the volume of a cylinder
    </div>
    <h:form id='text-form' enctype='multipart/form-data'>
        <dl>
            <dt><label for='file'>File to upload</label></dt>
            <dd>
                <h:inputFile id='file' size='50' value='#{image.image}'
                             styleClass="ui-corner-all">
                </h:inputFile>
                <h:message for='file'/>
            </dd>
        </dl>
        <ul class='buttons'>
            <li>
                <h:commandButton id='run' value='Upload' action="#{image.post}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```
## h:inputHidden

Renders an HTML `input` element of type `hidden`.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| converterMessage        | false    | String          | A ValueExpression enabled attribute that, if present, will be used as the text of the converter message, replacing any message that comes from the converter. |
| required               | false    | Boolean          | Flag indicating that the user is required to provide a submitted value for this input component. |
| requiredMessage        | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validation message for the "required" facility, if the "required" facility is used. |
| validatorMessage       | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validator message, replacing any message that comes from the validator. |
| value                  | false    | ValueExpression  | The current value of this component. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <h:form id='text-form'>
        <h:inputHidden id='radius' value='#{compute.radius}'>
            <f:converter converterId="float"/>
        </h:inputHidden>
        <dl class='ui-widget container_12'>
            <dt><label for='height'>Height</label><h:message for='height'/></dt>
            <dd>
                <h:inputText id='height' size='10' value='#{compute.height}'
                             styleClass="ui-corner-all">
                    <f:converter converterId="float" />
                </h:inputText>
            </dd>
        </dl>
        <ul class='container_12 buttons'>
            <li>
                <h:commandButton id='run' value='Compute' action="#{compute.run}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```
## h:inputSecret

Renders an HTML `input` element of type `password`.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| converterMessage        | false    | String          | A ValueExpression enabled attribute that, if present, will be used as the text of the converter message, replacing any message that comes from the converter. |
| style               | false    | String          | CSS style(s) to be applied when this component is rendered. |
| styleClass          | false    | String          | Space-separated list of CSS style class(es) to be applied when this element is rendered. This value must be passed through as the "class" attribute on generated markup. |
| title               | false    | String          | Advisory title information about markup elements generated for this component. |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| onchange               | false    | String          | Javascript code executed when the value of the element changes. |
| onclick                | false    | String          | Javascript code executed when a pointer button is clicked over this element. |
| ondbclick              | false    | String          | Javascript code executed when a pointer button is double clicked over this element. |
| onkeydown              | false    | String          | Javascript code executed when a key is pressed down over this element. |
| onkeypress             | false    | String          | Javascript code executed when a key is pressed or released over this element. |
| onkeyup                | false    | String          | Javascript code executed when a key is released over this element. |
| onmousedown            | false    | String          | Javascript code executed when a pointer button is pressed down over this element. |
| onmousemove            | false    | String          | Javascript code executed when a pointer button is moved within this element. |
| onmouseout             | false    | String          | Javascript code executed when a pointer button is moved away from this element. |
| onmouseover            | false    | String          | Javascript code executed when a pointer button is moved onto this element. |
| onmouseup              | false    | String          | Javascript code executed when a pointer button is released over this element. |
| required               | false    | Boolean          | Flag indicating that the user is required to provide a submitted value for this input component. |
| requiredMessage        | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validation message for the "required" facility, if the "required" facility is used. |
| validatorMessage       | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validator message, replacing any message that comes from the validator. |
| value                  | false    | ValueExpression  | The current value of this component. |
| accesskey              | false    | String          | Access key that, when pressed, transfers focus to this element. |
| tabindex               | false    | Natural         | Position of this element in the tabbing order for the current document. This value must be an integer between 0 and 32767. |
| maxlength        | false    | Natural          | The maximum number of characters that may be entered in this field. |
| size             | false    | Natural          | The number of characters used to determine the width of this field. |
| redisplay        | false    | Boolean          | Flag indicating that any existing value in this field should be rendered when the form is created. Because this is a potential security risk, password values are not displayed by default. |
| readonly         | false    | Boolean          | Flag indicating that this component will prohibit changes by the user. The element may receive focus unless it has also been disabled. A value of false causes no attribute to be rendered, while a value of true causes the attribute to be rendered as readonly="readonly". |
| disabled         | false    | Boolean          | Flag indicating that this element must never receive focus or be included in a subsequent submit. A value of false causes no attribute to be rendered, while a value of true causes the attribute to be rendered as disabled="disabled". |
| label            | false    | String           | A localized user presentable name for this component. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <div class="ui-widget ui-widget-header">
        Compute the volume of a cylinder
    </div>
    <h:form id='text-form'>
        <dl>
            <dt><label for='height'>Secret height</label></dt>
            <dd>
                <h:inputSecret id='height' size='10' value='#{compute.height}'
                               styleClass="ui-corner-all">
                    <f:converter converterId="float" />
                </h:inputSecret>
                <h:message for='height'/>
            </dd>
            <dt><label for='radius'>Secret radius</label></dt>
            <dd>
                <h:inputSecret id='radius' size='10' value='#{compute.radius}'
                               styleClass="ui-corner-all">
                    <f:converter converterId="float"/>
                </h:inputSecret>
                <h:message for='radius'/>
            </dd>
        </dl>
        <ul class='buttons'>
            <li>
                <h:commandButton id='run' value='Compute' action="#{compute.run}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```
## h:inputText

Renders an HTML `input` element of type `text`.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| converterMessage        | false    | String          | A ValueExpression enabled attribute that, if present, will be used as the text of the converter message, replacing any message that comes from the converter. |
| style               | false    | String          | CSS style(s) to be applied when this component is rendered. |
| styleClass          | false    | String          | Space-separated list of CSS style class(es) to be applied when this element is rendered. This value must be passed through as the "class" attribute on generated markup. |
| title               | false    | String          | Advisory title information about markup elements generated for this component. |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| onchange               | false    | String          | Javascript code executed when the value of the element changes. |
| onclick                | false    | String          | Javascript code executed when a pointer button is clicked over this element. |
| ondbclick              | false    | String          | Javascript code executed when a pointer button is double clicked over this element. |
| onkeydown              | false    | String          | Javascript code executed when a key is pressed down over this element. |
| onkeypress             | false    | String          | Javascript code executed when a key is pressed or released over this element. |
| onkeyup                | false    | String          | Javascript code executed when a key is released over this element. |
| onmousedown            | false    | String          | Javascript code executed when a pointer button is pressed down over this element. |
| onmousemove            | false    | String          | Javascript code executed when a pointer button is moved within this element. |
| onmouseout             | false    | String          | Javascript code executed when a pointer button is moved away from this element. |
| onmouseover            | false    | String          | Javascript code executed when a pointer button is moved onto this element. |
| onmouseup              | false    | String          | Javascript code executed when a pointer button is released over this element. |
| required               | false    | Boolean          | Flag indicating that the user is required to provide a submitted value for this input component. |
| requiredMessage        | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validation message for the "required" facility, if the "required" facility is used. |
| validatorMessage       | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validator message, replacing any message that comes from the validator. |
| value                  | false    | ValueExpression  | The current value of this component. |
| accesskey              | false    | String          | Access key that, when pressed, transfers focus to this element. |
| tabindex               | false    | Natural         | Position of this element in the tabbing order for the current document. This value must be an integer between 0 and 32767. |
| maxlength        | false    | Natural          | The maximum number of characters that may be entered in this field. |
| size             | false    | Natural          | The number of characters used to determine the width of this field. |
| redisplay        | false    | Boolean          | Flag indicating that any existing value in this field should be rendered when the form is created. Because this is a potential security risk, password values are not displayed by default. |
| readonly         | false    | Boolean          | Flag indicating that this component will prohibit changes by the user. The element may receive focus unless it has also been disabled. A value of false causes no attribute to be rendered, while a value of true causes the attribute to be rendered as readonly="readonly". |
| disabled         | false    | Boolean          | Flag indicating that this element must never receive focus or be included in a subsequent submit. A value of false causes no attribute to be rendered, while a value of true causes the attribute to be rendered as disabled="disabled". |
| label            | false    | String           | A localized user presentable name for this component. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <div class="ui-widget ui-widget-header">
        Compute the volume of a cylinder
    </div>
    <h:form id='text-form'>
        <dl>
            <dt><label for='height'>Height</label></dt>
            <dd>
                <h:inputText id='height' size='10' value='#{compute.height}'
                             styleClass="ui-corner-all">
                    <f:converter converterId="float" />
                </h:inputText>
                <h:message for='height'/>
            </dd>
            <dt><label for='radius'>Radius</label></dt>
            <dd>
                <h:inputText id='radius' size='10' value='#{compute.radius}'
                             styleClass="ui-corner-all">
                    <f:converter converterId="float"/>
                </h:inputText>
                <h:message for='radius'/>
            </dd>
        </dl>
        <ul class='buttons'>
            <li>
                <h:commandButton id='run' value='Compute' action="#{compute.run}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```
## h:inputTextarea

Renders an HTML `textarea` element.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| converterMessage        | false    | String          | A ValueExpression enabled attribute that, if present, will be used as the text of the converter message, replacing any message that comes from the converter. |
| style               | false    | String          | CSS style(s) to be applied when this component is rendered. |
| styleClass          | false    | String          | Space-separated list of CSS style class(es) to be applied when this element is rendered. This value must be passed through as the "class" attribute on generated markup. |
| title               | false    | String          | Advisory title information about markup elements generated for this component. |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| onchange               | false    | String          | Javascript code executed when the value of the element changes. |
| onclick                | false    | String          | Javascript code executed when a pointer button is clicked over this element. |
| ondbclick              | false    | String          | Javascript code executed when a pointer button is double clicked over this element. |
| onkeydown              | false    | String          | Javascript code executed when a key is pressed down over this element. |
| onkeypress             | false    | String          | Javascript code executed when a key is pressed or released over this element. |
| onkeyup                | false    | String          | Javascript code executed when a key is released over this element. |
| onmousedown            | false    | String          | Javascript code executed when a pointer button is pressed down over this element. |
| onmousemove            | false    | String          | Javascript code executed when a pointer button is moved within this element. |
| onmouseout             | false    | String          | Javascript code executed when a pointer button is moved away from this element. |
| onmouseover            | false    | String          | Javascript code executed when a pointer button is moved onto this element. |
| onmouseup              | false    | String          | Javascript code executed when a pointer button is released over this element. |
| required               | false    | Boolean          | Flag indicating that the user is required to provide a submitted value for this input component. |
| requiredMessage        | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validation message for the "required" facility, if the "required" facility is used. |
| validatorMessage       | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validator message, replacing any message that comes from the validator. |
| value                  | false    | ValueExpression  | The current value of this component. |
| accesskey              | false    | String          | Access key that, when pressed, transfers focus to this element. |
| tabindex               | false    | Natural         | Position of this element in the tabbing order for the current document. This value must be an integer between 0 and 32767. |
| rows             | false    | Natural          | The number of rows to be displayed. |
| cols             | false    | Natural          | The number of columns to be displayed. |
| readonly         | false    | Boolean          | Flag indicating that this component will prohibit changes by the user. The element may receive focus unless it has also been disabled. A value of false causes no attribute to be rendered, while a value of true causes the attribute to be rendered as readonly="readonly". |
| label            | false    | String           | A localized user presentable name for this component. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">

    <div class="ui-widget ui-widget-header">
        Write a message in a textarea
    </div>
    <h:form id='textarea-form'>
        <dl>
            <dt><label for='email'>Email</label> <h:message for='email'/></dt>
            <dd>
                <h:inputText id='email' size='80' value='#{message.email}'
                             styleClass="ui-corner-all">
                </h:inputText>
            </dd>
            <dt><label for='message'>Message</label> <h:message for='message'/></dt>
            <dd>
                <h:inputTextarea id='message' rows='20' cols='30' value='#{message.text}'/>
            </dd>
        </dl>
        <ul class='buttons'>
            <li>
                <h:commandButton id='send' value='Send' action="#{message.post}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```
## h:list

Renders a list of items.

When the list layout is `orderedList` an `ol/li` list is generated,
when the layout is `unorderedList` an `ul/li` list is generated.
The default (`simple`) renders the list as is.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| var              | true     | String           | Name of the variable which holds the current row value. |
| value            | true     | ValueExpression  | The value expression representing the list to iterate on. |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| style               | false    | String          | CSS style(s) to be applied when this component is rendered. |
| styleClass          | false    | String          | Space-separated list of CSS style class(es) to be applied when this element is rendered. This value must be passed through as the "class" attribute on generated markup. |
| title               | false    | String          | Advisory title information about markup elements generated for this component. |
| layout           | false    | String           | The layout of the list: simple, unorderedList, orderedList. |
| itemStyleClass   | false    | String           | The CSS class attribute to apply to the li or div items. |

### Example

```
<div xmlns:h="http://java.sun.com/jsf/html">
    <h:list value="#{messages}" var="msg">
        <div class="message">
            <div class="email">
                #{msg.email}
            </div>
            <div class="text">
                #{msg.text}
            </div>
        </div>
    </h:list>
    <!-- Demo Hint: use the 'Forum' demo to populate the above list if it is empty  -->
</div>
```
## h:outputFormat

Render parameterized text.

Obtain the style, styleClass, dir, and lang attributees
from this component. If any are present, render a `span` element. Output the
styleClass attribute (if present) as the value of the class attribute. Output the
style attribute as the value of the style attribute. Output the dir and lang attributes
as pass through attributes. Accrue a list of the values of all child `UIParameter` components
of this component. If there are one or more accumulated parameter values, convert the list
of parameter values to an Object array, call MessageFormat.format(), passing the value of
this component as the first argument, and the array of parameter values as the second argument,
and render the result. Otherwise, render the value of this component unmodified.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| style               | false    | String          | CSS style(s) to be applied when this component is rendered. |
| styleClass          | false    | String          | Space-separated list of CSS style class(es) to be applied when this element is rendered. This value must be passed through as the "class" attribute on generated markup. |
| title               | false    | String          | Advisory title information about markup elements generated for this component. |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| accesskey              | false    | String          | Access key that, when pressed, transfers focus to this element. |
| tabindex               | false    | Natural         | Position of this element in the tabbing order for the current document. This value must be an integer between 0 and 32767. |
| onchange               | false    | String          | Javascript code executed when the value of the element changes. |
| onclick                | false    | String          | Javascript code executed when a pointer button is clicked over this element. |
| ondbclick              | false    | String          | Javascript code executed when a pointer button is double clicked over this element. |
| onkeydown              | false    | String          | Javascript code executed when a key is pressed down over this element. |
| onkeypress             | false    | String          | Javascript code executed when a key is pressed or released over this element. |
| onkeyup                | false    | String          | Javascript code executed when a key is released over this element. |
| onmousedown            | false    | String          | Javascript code executed when a pointer button is pressed down over this element. |
| onmousemove            | false    | String          | Javascript code executed when a pointer button is moved within this element. |
| onmouseout             | false    | String          | Javascript code executed when a pointer button is moved away from this element. |
| onmouseover            | false    | String          | Javascript code executed when a pointer button is moved onto this element. |
| onmouseup              | false    | String          | Javascript code executed when a pointer button is released over this element. |
| for              | false    | String          | Client identifier of the component for which this element is a label. |
| value            | false    | String          | The current value of this component. |
| escape           | false    | Boolean         | Flag indicating that characters that are sensitive in HTML and XML markup must be escaped. This flag is set to "true" by default. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <!-- Write a text string escaping special characters -->
    <p><h:outputFormat value="The application name is: {0}">
        <f:param value="#{sampleName}"/>
    </h:outputFormat>
    </p>
    <!-- Write a text with a style.  Generate a <span> element.  -->
    <p><h:outputFormat value="The context path is: #{contextPath}"
                       style="font-weight: bold;">
        <f:param value="#{contextPath}"/>
    </h:outputFormat>
    </p>
</div>
```
## h:outputLabel

Renders an HTML `label` element. Render the current value of the component as label
text if it is specified. If a `for` attribute is specified, find the component specified
by the value of the `for` attribute, and render its client id as the value of
the `for` attribute. If "styleClass" attribute is specified, render its value as the
value of the "class" attribute.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| style               | false    | String          | CSS style(s) to be applied when this component is rendered. |
| styleClass          | false    | String          | Space-separated list of CSS style class(es) to be applied when this element is rendered. This value must be passed through as the "class" attribute on generated markup. |
| title               | false    | String          | Advisory title information about markup elements generated for this component. |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| accesskey              | false    | String          | Access key that, when pressed, transfers focus to this element. |
| tabindex               | false    | Natural         | Position of this element in the tabbing order for the current document. This value must be an integer between 0 and 32767. |
| onchange               | false    | String          | Javascript code executed when the value of the element changes. |
| onclick                | false    | String          | Javascript code executed when a pointer button is clicked over this element. |
| ondbclick              | false    | String          | Javascript code executed when a pointer button is double clicked over this element. |
| onkeydown              | false    | String          | Javascript code executed when a key is pressed down over this element. |
| onkeypress             | false    | String          | Javascript code executed when a key is pressed or released over this element. |
| onkeyup                | false    | String          | Javascript code executed when a key is released over this element. |
| onmousedown            | false    | String          | Javascript code executed when a pointer button is pressed down over this element. |
| onmousemove            | false    | String          | Javascript code executed when a pointer button is moved within this element. |
| onmouseout             | false    | String          | Javascript code executed when a pointer button is moved away from this element. |
| onmouseover            | false    | String          | Javascript code executed when a pointer button is moved onto this element. |
| onmouseup              | false    | String          | Javascript code executed when a pointer button is released over this element. |
| for              | false    | String          | Client identifier of the component for which this element is a label. |
| value            | false    | String          | The current value of this component. |
| escape           | false    | Boolean         | Flag indicating that characters that are sensitive in HTML and XML markup must be escaped. This flag is set to "true" by default. |

### Example

```
<dl xmlns:f="http://java.sun.com/jsf/core"
    xmlns:h="http://java.sun.com/jsf/html">
    <dt><h:outputLabel for='height' value="Height"/><h:message for='height'/></dt>
    <dd>
        <h:selectOneMenu id='height' size='10' value='#{compute.height}'
                         styleClass="ui-state-default ui-corner-all">
            <f:selectItem itemLabel="1 inch" itemValue="25.4"/>
            <f:selectItem itemLabel="1 feet" itemValue="304.8"/>
            <f:selectItem itemLabel="1 yard" itemValue="914.4"/>
            <f:converter converterId="float" />
        </h:selectOneMenu>
    </dd>
    <dt><h:outputLabel for='radius' value="Radius"/><h:message for='radius'/></dt>
    <dd>
        <h:selectOneMenu id='radius' size='10' value='#{compute.radius}'
                         styleClass="ui-state-default ui-corner-all">
            <f:selectItem itemLabel="1 inch" itemValue="25.4"/>
            <f:selectItem itemLabel="1 feet" itemValue="304.8"/>
            <f:selectItem itemLabel="1 yard" itemValue="914.4"/>
            <f:converter converterId="float"/>
        </h:selectOneMenu>
    </dd>
</dl>
```
## h:outputLink

Render an HTML "a" anchor element. The value of the component is rendered as the value
of the "href" attribute. Any child UIParameter components are appended to the String
to be output as the value of the "href" attribute as query parameters before rendering.
The entire "href" string must be passed through a call to the encodeResourceURL() method
of the ExternalContext. The name of the UIParameter goes on the left hand side, and the value
of the UIParameter on the right hand side. The name and the value must be URLEncoded.
Each UIParameter instance is separeted by an ampersand, as dictated in the URL spec.
If the "styleClass" attribute is specified, render its value as the value of the "class" attribute.
If the "id" attribute is specified, follow the same steps as mentioned in
the "General Notes on Encoding" regarding the "id" attribute for UIInput components.
If the "disabled" attribute is specified, do not render the HTML "a" anchor element or
the "href" element. Instead, render a "span" element. If the "styleClass" attribute is specified,
render its value as the value of the "class" attribute on the "span".

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| style               | false    | String          | CSS style(s) to be applied when this component is rendered. |
| styleClass          | false    | String          | Space-separated list of CSS style class(es) to be applied when this element is rendered. This value must be passed through as the "class" attribute on generated markup. |
| title               | false    | String          | Advisory title information about markup elements generated for this component. |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| accesskey              | false    | String          | Access key that, when pressed, transfers focus to this element. |
| tabindex               | false    | Natural         | Position of this element in the tabbing order for the current document. This value must be an integer between 0 and 32767. |
| onchange               | false    | String          | Javascript code executed when the value of the element changes. |
| onclick                | false    | String          | Javascript code executed when a pointer button is clicked over this element. |
| ondbclick              | false    | String          | Javascript code executed when a pointer button is double clicked over this element. |
| onkeydown              | false    | String          | Javascript code executed when a key is pressed down over this element. |
| onkeypress             | false    | String          | Javascript code executed when a key is pressed or released over this element. |
| onkeyup                | false    | String          | Javascript code executed when a key is released over this element. |
| onmousedown            | false    | String          | Javascript code executed when a pointer button is pressed down over this element. |
| onmousemove            | false    | String          | Javascript code executed when a pointer button is moved within this element. |
| onmouseout             | false    | String          | Javascript code executed when a pointer button is moved away from this element. |
| onmouseover            | false    | String          | Javascript code executed when a pointer button is moved onto this element. |
| onmouseup              | false    | String          | Javascript code executed when a pointer button is released over this element. |
| value            | false    | String          | The current value of this component. |


### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <!-- Link is enabled -->
    <p><h:outputLink value="https://github.com/stcarrez/ada-asf">Ada Server Faces</h:outputLink></p>
    <!-- Write a text with a style.  Generate a <span> element.  -->
    <p><h:outputLink value="https://github.com/stcarrez/ada-el" disabled="true"
                     style="font-weight: bold;">Ada EL</h:outputLink></p>
</div>
```

## h:outputText

Renders the value of the associated UIOutput component.
If this element has an ID or CSS style properties, the text is wrapped in a span element.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| style               | false    | String          | CSS style(s) to be applied when this component is rendered. |
| styleClass          | false    | String          | Space-separated list of CSS style class(es) to be applied when this element is rendered. This value must be passed through as the "class" attribute on generated markup. |
| title               | false    | String          | Advisory title information about markup elements generated for this component. |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| value            | false    | String          | The current value of this component. |
| escape           | false    | Boolean         | Flag indicating that characters that are sensitive in HTML and XML markup must be escaped. This flag is set to "true" by default. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <!-- Write a text string escaping special characters -->
    <p><h:outputText value="The application name is: #{sampleName}"/></p>
    <!-- Write a text with a style.  Generate a <span> element.  -->
    <p><h:outputText value="The context path is: #{contextPath}"
                     style="font-weight: bold;"/></p>
    <!-- Write a text without escaping -->
    <p><h:outputText escape="false" value="&lt;i&gt;This string is not escaped.&lt;/i&gt;"/></p>
</div>
```
## h:panelGroup

This element is used to group other components where the specification requires one child element.
If any of the HTML or CSS attributes are set, its content is rendered within a span or div element.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| style               | false    | String          | CSS style(s) to be applied when this component is rendered. |
| styleClass          | false    | String          | Space-separated list of CSS style class(es) to be applied when this element is rendered. This value must be passed through as the "class" attribute on generated markup. |
| title               | false    | String          | Advisory title information about markup elements generated for this component. |
| layout           | false    | String          | The type of layout markup to use when rendering this group. If the value is "block" the renderer must produce an HTML "div" element. Otherwise HTML "span" element must be produced. |


### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <h:panelGroup layout="block">
        A div element
    </h:panelGroup>
    <h:panelGroup rendered="#{sampleName eq 'demo'}">
        A text block which is not present (rendered = false)
    </h:panelGroup>
    <h:panelGroup rendered="#{sampleName ne 'demo'}">
        A text block which is present (rendered = true)
    </h:panelGroup>
    <h:panelGroup rendered="#{sampleName ne 'demo'}" style="padding-top: 20px; color: red;">
        A text block which is present (rendered = true)
    </h:panelGroup>
</div>
```

## h:selectBooleanCheckbox

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| style               | false    | String          | CSS style(s) to be applied when this component is rendered. |
| styleClass          | false    | String          | Space-separated list of CSS style class(es) to be applied when this element is rendered. This value must be passed through as the "class" attribute on generated markup. |
| title               | false    | String          | Advisory title information about markup elements generated for this component. |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| onchange               | false    | String          | Javascript code executed when the value of the element changes. |
| onclick                | false    | String          | Javascript code executed when a pointer button is clicked over this element. |
| ondbclick              | false    | String          | Javascript code executed when a pointer button is double clicked over this element. |
| onkeydown              | false    | String          | Javascript code executed when a key is pressed down over this element. |
| onkeypress             | false    | String          | Javascript code executed when a key is pressed or released over this element. |
| onkeyup                | false    | String          | Javascript code executed when a key is released over this element. |
| onmousedown            | false    | String          | Javascript code executed when a pointer button is pressed down over this element. |
| onmousemove            | false    | String          | Javascript code executed when a pointer button is moved within this element. |
| onmouseout             | false    | String          | Javascript code executed when a pointer button is moved away from this element. |
| onmouseover            | false    | String          | Javascript code executed when a pointer button is moved onto this element. |
| onmouseup              | false    | String          | Javascript code executed when a pointer button is released over this element. |
| required               | false    | Boolean          | Flag indicating that the user is required to provide a submitted value for this input component. |
| requiredMessage        | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validation message for the "required" facility, if the "required" facility is used. |
| validatorMessage       | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validator message, replacing any message that comes from the validator. |
| value                  | false    | ValueExpression  | The current value of this component. |
| layout           | false    | String           | The type of layout markup to use when rendering this group. If the value is "block" the renderer must produce an HTML "div" element. Otherwise HTML "span" element must be produced. |


### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <div class="ui-widget ui-widget-header">
        Compute the volume of a cylinder
    </div>
    <h:form id='text-form'>
        <dl>
            <dt><label for='flag'>Is active</label> <h:message for='flag'/></dt>
            <dd>
                <h:selectBooleanCheckbox id="flag" value="{empty compute.radius}"/>

            </dd>
            <dt><label for='radius'>Radius</label></dt>
            <dd>
                <h:inputText id='radius' size='10' value='#{compute.radius}'
                             styleClass="ui-corner-all">
                    <f:converter converterId="float"/>
                </h:inputText>
                <h:message for='radius'/>
            </dd>
        </dl>
        <ul class='buttons'>
            <li>
                <h:commandButton id='run' value='Compute' action="#{compute.run}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```

## h:selectOneMenu

Allow the user to choose one option from a set of options.
Renders a drop-down menu (aka "combo-box") containing a set of choices, of which
only one can be chosen at a time. The available choices are defined via child
`f:selectItem` or `f:selectItems` elements.

The value attribute of this component is read to determine which of the available
options is initially selected; its value should match the "value" property of one of the
child `SelectItem` objects.

On submit of the enclosing form, the value attribute's bound property is updated to contain
the "value" property from the chosen `SelectItem`.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| style               | false    | String          | CSS style(s) to be applied when this component is rendered. |
| styleClass          | false    | String          | Space-separated list of CSS style class(es) to be applied when this element is rendered. This value must be passed through as the "class" attribute on generated markup. |
| title               | false    | String          | Advisory title information about markup elements generated for this component. |
| dir               | false    | String          | Direction indication for text that does not inherit directionality. Valid values are "LTR" (left-to-right) and "RTL" (right-to-left). |
| lang              | false    | String          | Code describing the language used in the generated markup for this component. |
| onchange               | false    | String          | Javascript code executed when the value of the element changes. |
| onclick                | false    | String          | Javascript code executed when a pointer button is clicked over this element. |
| ondbclick              | false    | String          | Javascript code executed when a pointer button is double clicked over this element. |
| onkeydown              | false    | String          | Javascript code executed when a key is pressed down over this element. |
| onkeypress             | false    | String          | Javascript code executed when a key is pressed or released over this element. |
| onkeyup                | false    | String          | Javascript code executed when a key is released over this element. |
| onmousedown            | false    | String          | Javascript code executed when a pointer button is pressed down over this element. |
| onmousemove            | false    | String          | Javascript code executed when a pointer button is moved within this element. |
| onmouseout             | false    | String          | Javascript code executed when a pointer button is moved away from this element. |
| onmouseover            | false    | String          | Javascript code executed when a pointer button is moved onto this element. |
| onmouseup              | false    | String          | Javascript code executed when a pointer button is released over this element. |
| required               | false    | Boolean          | Flag indicating that the user is required to provide a submitted value for this input component. |
| requiredMessage        | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validation message for the "required" facility, if the "required" facility is used. |
| validatorMessage       | false    | String           | A ValueExpression enabled attribute that, if present, will be used as the text of the validator message, replacing any message that comes from the validator. |
| value                  | false    | ValueExpression  | The current value of this component. |
| layout           | false    | String           | The type of layout markup to use when rendering this group. If the value is "block" the renderer must produce an HTML "div" element. Otherwise HTML "span" element must be produced. |


### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <div class="ui-widget ui-widget-header">
        Compute the volume of a cylinder
    </div>
    <h:form id='select-form'>
        <dl>
            <dt><label for='height'>Height</label> <h:message for='height'/></dt>
            <dd>
                <h:selectOneMenu id='height' size='10' value='#{compute.height}'
                                 styleClass="ui-state-default ui-corner-all">
                    <f:selectItem itemLabel="1 inch" itemValue="25.4"/>
                    <f:selectItem itemLabel="1 feet" itemValue="304.8"/>
                    <f:selectItem itemLabel="1 yard" itemValue="914.4"/>
                    <f:converter converterId="float" />
                </h:selectOneMenu>
            </dd>
            <dt><label for='radius'>Radius</label> <h:message for='radius'/></dt>
            <dd>
                <h:selectOneMenu id='radius' size='10' value='#{compute.radius}'
                                 styleClass="ui-state-default ui-corner-all">
                    <f:selectItem itemLabel="1 inch" itemValue="25.4"/>
                    <f:selectItem itemLabel="1 feet" itemValue="304.8"/>
                    <f:selectItem itemLabel="1 yard" itemValue="914.4"/>
                    <f:converter converterId="float"/>
                </h:selectOneMenu>
            </dd>
        </dl>
        <ul class='buttons'>
            <li>
                <h:commandButton id='run' value='Compute' action="#{compute.run}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```


