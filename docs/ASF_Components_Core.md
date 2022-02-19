# Core Components
The facelets is the default view declaration language that uses XML and XHTML.
It is a composition and templating framework that allows to create the component
tree.

The core components are defined in the following namespace:
```
xmlns:f="http://java.sun.com/jsf/core"
```

The core components are implemented by the `ASF.Components.Core`
package which defines the `UIComponent` that describes the various elements
provided by the core components.  These components are instantiated when the
view is created from the facelet tree that was read from the XHTML view description.

## f:attribute

This tag associates an attribute with the nearest parent `UIComponent`.
When the value is not an EL expression, this tag has the same effect as calling
`Component.Set_Attribute (Name, Value)`. When the attribute name specified
matches a standard property of the component, that property is set.
However it is also valid to assign attributes to components using any
arbitrary name; the component itself won't make any use of these but
other objects such as custom renderers, validators or action listeners
can later retrieve the attribute from the component by name.

When the value is an EL expression, this tag has the same effect as
calling `Component.Set_Attribute (Tag, Value)` A call to method
`Component.Get_Attribute (Name)` will then cause that expression to
be evaluated and the result of the expression is returned,
not the original EL expression string.

See the `ASF.Components.Base` package for more details.

Unless otherwise specified, all attributes accept static values or EL expressions.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| name             | false    | String          | The name of the attribute. |
| value            | false    | String          | The attribute's value. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <h:panelGroup layout="block">
        <h:outputText value="Hello world!">
            <!-- The 'styleClass' attribute is applied
                 on the h:outputText -->
            <f:attribute name="styleClass" value="left"/>
        </h:outputText>
        <p>
            <!-- The 'styleClass' attribute is applied
                 on the h:panelGroup -->
            <f:attribute name="styleClass"
                         value="ui-widget asf-container ui-corner-all"/>
        </p>
    </h:panelGroup>
</div>
```
## f:convertDateTime

This tag registers an instance of a `Date_Time_Converter`, and associates it
with the nearest parent UIComponent.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| dateStyle        | false    | String          | Predefined formatting style which determines how the date component of a date string is to be formatted and parsed. Applied only if type is "date" or "both". Valid values are "default", "short", "medium", "long", and "full". Default value is "default". |
| locale           | false    | String          | Locale whose predefined styles for dates and times are used during formatting or parsing. If not specified, the Locale returned by `FacesContext.Get_View_Root().Get_Locale()` will be used. Value must be either a VB expression that evaluates to a valid String locale. |
| pattern          | false    | String          | Custom formatting pattern which determines how the date/time string should be formatted and parsed. |
| timeStyle        | false    | String          | Predefined formatting style which determines how the time component of a date string is to be formatted and parsed. Applied only if type is "time" or "both". Valid values are "default", "short", "medium", "long", and "full". Default value is "default". |
| timeZone         | false    | String          | Time zone in which to interpret any time information in the date String. Value must be either a VB expression that evaluates to a valid string that is a timezone ID. |
| type             | false    | String          | Specifies what contents the string value will be formatted to include, or parsed expecting. Valid values are "date", "time", and "both". Default value is "date". |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <dl>
        <dt>No converter</dt>
        <dd>
            <!-- pi is a float, use a default converter -->
            #{messages.today}
        </dd>
        <dt>With converter</dt>
        <dd>
            <h:outputText value='#{messages.today}'>
                <!-- use the 'float' converter defined by the application  -->
                <f:convertDateTime dateStyle="short"/>
            </h:outputText>
        </dd>
    </dl>
    <h4>dateStyle</h4>
    <dl class='list'>
        <dt>short</dt>
        <dd>
            <h:outputText value='#{messages.today}'>
                <f:convertDateTime dateStyle="short"/>
            </h:outputText>
        </dd>
        <dt>medium</dt>
        <dd>
            <h:outputText value='#{messages.today}'>
                <f:convertDateTime dateStyle="medium"/>
            </h:outputText>
        </dd>
        <dt>long</dt>
        <dd>
            <h:outputText value='#{messages.today}'>
                <f:convertDateTime dateStyle="long"/>
            </h:outputText>
        </dd>
        <dt>full</dt>
        <dd>
            <h:outputText value='#{messages.today}'>
                <f:convertDateTime dateStyle="full"/>
            </h:outputText>
        </dd>
    </dl>
    <h4>timeStyle</h4>
    <dl class='list'>
        <dt>short</dt>
        <dd>
            <h:outputText value='#{messages.today}'>
                <f:convertDateTime timeStyle="short"/>
            </h:outputText>
        </dd>
        <dt>medium</dt>
        <dd>
            <h:outputText value='#{messages.today}'>
                <f:convertDateTime timeStyle="long"/>
            </h:outputText>
        </dd>
    </dl>
</div>
```
## f:converter

This tag creates an instance of the specified Converter, and associates it
with the nearest parent UIComponent.

Register a named Converter instance on the UIComponent associated with the
closest parent UIComponent custom action.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| converterId      | true     | String          | The converter's registered identifier.    |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <dl>
        <dt>No converter</dt>
        <dd>
            <!-- pi is a float, use a default converter -->
            #{compute.pi}
        </dd>
        <dt>With converter</dt>
        <dd>
            <h:outputText value='#{compute.pi}'>
                <!-- use the 'float' converter defined by the application  -->
                <f:converter converterId="float"/>
            </h:outputText>
        </dd>
    </dl>
</div>
```
## f:facet

This tag allows to register the named facet to the closest parent UIComponent.

Facets are used by some components to render and control specific parts of the component:
for example a table header or footer, the column header, the widget panel titles.
Example of components that use facets: `h:dataTable`, `h:panelGrid`, `w:panel`.

Warning: if a facet is used within a component that does not recognize the name,
the facet content will be ignored.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| name             | true     | String          | The facet name. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <f:facet name="title">
        title
    </f:facet>
</div>
```
## f:metadata

Declares the metadata facet for the view.

### Attributes

None.

### Example

```
<f:view xmlns:f="http://java.sun.com/jsf/core">
    <f:metadata>
        <f:viewParam id='height' value='#{compute.height}'>
            <f:converter converterId="float" />
        </f:viewParam>
        <f:viewParam id='radius' value='#{compute.radius}'>
            <f:converter converterId="float" />
        </f:viewParam>
        <f:viewAction action="#{compute.run}"/>
    </f:metadata>
    <dl>
        <dt>Height</dt>
        <dd>
            #{compute.height}
        </dd>
         <dt>Radius</dt>
        <dd>
            #{compute.radius}
        </dd>
        <dt>Volume</dt>
        <dd>
            #{compute.volume}
        </dd>
    </dl>
</f:view>
```
## f:param

This tag associates a parameter name-value pair with the nearest parent UIComponent.
A UIComponent is created to represent this name-value pair, and stored as a child
of the parent component; what effect this has depends upon the renderer of that
parent component.

Unless otherwise specified, all attributes accept static values or EL expressions.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| name             | false    | String          | The name under which the value is stored. |
| value            | false    | String          | The value of this component. |
| id               | false    | String          | Get a string which uniquely identifies this UIComponent within the nearest ancestor naming component. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <h:outputFormat value="The value of PI is {0} and 2 x PI is {1}.">
        <f:param value="#{compute.pi}"/>
        <f:param value="#{compute.pi + compute.pi}"/>
    </h:outputFormat>
</div>
```
## f:selectItem

This tag associates a single `SelectItem` with the nearest parent UIComponent.
The item represents a single option for a component such as an
`h:selectBooleanCheckbox` or `h:selectOneMenu`.
See also component `f:selectItems`.

Unless otherwise specified, all attributes accept static values or EL expressions.

UISelectItem should be nested inside a `UISelectMany` or
`UISelectOne` component, and results in the addition of a `SelectItem`
instance to the list of available options for the parent component.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| itemDisabled     | false    | Boolean         | Determine whether this item can be chosen by the user. When true, this item cannot be chosen by the user. If this method is ever called, then any EL-binding for the disabled property will be ignored. |
| escape           | false    | Boolean         | The escape setting for the label of this selection item. |
| itemDescription  | false    | String          | The item description. |
| itemLabel        | false    | String          | The string which will be presented to the user for this option. |
| itemValue        | false    | String          | The value for this item. |
| value            | false    | ValueExpression | The initial value of this component. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <h:selectOneMenu id='height' size='10' value='#{compute.height}'
                     styleClass="ui-state-default ui-corner-all">
        <f:selectItem itemLabel="1 inch" itemValue="25.4"/>
        <f:selectItem itemLabel="1 feet" itemValue="304.8"/>
        <f:selectItem itemLabel="1 yard" itemValue="914.4"/>
        <f:converter converterId="float" />
    </h:selectOneMenu>
</div>
```
## f:selectItems

This tag associates a list of `SelectItem` with the nearest parent UIComponent.
The list of items is retrieved via a value-binding.
See also component `f:selectItem`.

Unless otherwise specified, all attributes accept static values or EL expressions.

UISelectItem should be nested inside a `UISelectMany` or
`UISelectOne` component, and results in the addition of one or more `SelectItem`
instance to the list of available options for the parent component.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| itemDisabled     | false    | Boolean         | Determine whether this item can be chosen by the user. When true, this item cannot be chosen by the user. If this method is ever called, then any EL-binding for the disabled property will be ignored. |
| escape           | false    | Boolean         | The escape setting for the label of this selection item. |
| itemDescription  | false    | String          | The item description. |
| itemLabel        | false    | String          | The string which will be presented to the user for this option. |
| itemValue        | false    | String          | The value for this item. |
| value            | false    | ValueExpression | The initial value of this component. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:h="http://java.sun.com/jsf/html">
    <h:selectOneMenu id='height' size='10' value='#{compute.height}'
                     styleClass="ui-state-default ui-corner-all">
        <f:selectItems value="#{countries}"/>
    </h:selectOneMenu>
</div>
```
## f:validateLength

Creates a validator and associates it with the nearest parent `UIComponent`.
When invoked, the validator ensures that values are valid strings with a length
that lies within the minimum and maximum values specified. Commonly associated
with a `h:inputText` entity. Unless otherwise specified, all attributes
accept static values or EL expressions.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| maximum          | false    | Natural         | The largest value that should be considered valid. |
| minimum          | false    | Natural         | The smallest value that should be considered valid. |

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
                    <f:validateLength minimum="2" maximum="3"/>
                </h:inputText>
            </dd>
            <dt><label for='radius'>Radius</label><h:message for='radius'/></dt>
            <dd>
                <h:inputText id='radius' size='10' value='#{compute.radius}'
                             styleClass="ui-corner-all">
                    <f:converter converterId="float"/>
                    <f:validateLength minimum="1" maximum="4"/>
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
## f:validateLongRange

Creates a validator and associates it with the nearest parent `UIComponent`.
When invoked, the validator ensures that values are valid longs that lie within the
minimum and maximum values specified. Commonly associated
with a `h:inputText` entity. Unless otherwise specified, all attributes
accept static values or EL expressions.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| maximum          | false    | Long            | The largest value that should be considered valid. |
| minimum          | false    | Long            | The smallest value that should be considered valid. |

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
                    <f:validateLongRange minimum="1" maximum="3"/>
                </h:inputText>
            </dd>
            <dt><label for='radius'>Radius</label><h:message for='radius'/></dt>
            <dd>
                <h:inputText id='radius' size='10' value='#{compute.radius}'
                             styleClass="ui-corner-all">
                    <f:converter converterId="float"/>
                    <f:validateLongRange minimum="1" maximum="5"/>
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
## f:validator

Creates a validator and associates it with the nearest parent UIComponent.

During the validation phase (or the apply-request-values phase for immediate components),
if the associated component has any submitted value and the conversion of that
value to the required type has succeeded then the specified validator type is invoked
to test the validity of the converted value.

Commonly associated with an `h:inputText` entity, but may be applied to any input component.
Some validators may allow the component to use attributes to define component-specific
validation constraints; see the f:attribute tag. See also the "validator" attribute of
all input components, which allows a component to specify an arbitrary validation
`method` (rather than a registered validation type, as this tag does).

Unless otherwise specified, all attributes accept static values or EL expressions.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| validatorId      | false    | String          | The registered ID of the desired Validator.  |

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
                    <f:validator validatorId="validateDimension"/>
                </h:inputText>
            </dd>
            <dt><label for='radius'>Radius</label><h:message for='radius'/></dt>
            <dd>
                <h:inputText id='radius' size='10' value='#{compute.radius}'
                             styleClass="ui-corner-all">
                    <f:converter converterId="float"/>
                    <f:validator validatorId="validateDimension"/>
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
## f:view

Creates a JSF View, which is a container that holds all of the components
that are part of the view.  The `UIView` represents the root of
the component tree.

Unless otherwise specified, all attributes accept static values or EL expressions.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| locale           | false    | String          | The locale of this view. Default: the default locale from the configuration file. |
| contentType      | false    | String          | The content type to be placed in the response header. The default content type is `text/html`. |

### Example

```
<f:view contentType="application/json"
        xmlns:f="http://java.sun.com/jsf/core"
        xmlns:util="http://code.google.com/p/ada-asf/util"
        xmlns:h="http://java.sun.com/jsf/html">
[
 { "action": "update", "id": "#result", "data": "<util:escape>
     <h:panelGroup rendered="#{not empty compute.volume}">
         <h2>The cylinder volume is
             <h:outputText value="#{compute.volume}">
                <f:converter converterId="float"/>
             </h:outputText>
         </h2>
     </h:panelGroup>
    </util:escape>" }
]
</f:view>
```
## f:viewAction

The viewAction element is used in a medata facet.  It allows to execute an Ada bean action
method when a request is processed. The Ada bean method is executed before rendering the page.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| itemDisabled     | false    | Boolean         | Determine whether this item can be chosen by the user. When true, this item cannot be chosen by the user. If this method is ever called, then any EL-binding for the disabled property will be ignored. |
| escape           | false    | Boolean         | The escape setting for the label of this selection item. |
| itemDescription  | false    | String          | The item description. |
| itemLabel        | false    | String          | The string which will be presented to the user for this option. |
| itemValue        | false    | String          | The value for this item. |
| value            | false    | ValueExpression | The initial value of this component. |

### Example

```
<f:view xmlns:f="http://java.sun.com/jsf/core">
    <f:metadata>
        <f:viewParam id='height' value='#{compute.height}'>
            <f:converter converterId="float" />
        </f:viewParam>
        <f:viewParam id='radius' value='#{compute.radius}'>
            <f:converter converterId="float" />
        </f:viewParam>
        <f:viewAction action="#{compute.run}"/>
    </f:metadata>
    <dl>
        <dt>Height</dt>
        <dd>
            #{compute.height}
        </dd>
         <dt>Radius</dt>
        <dd>
            #{compute.radius}
        </dd>
        <dt>Volume</dt>
        <dd>
            #{compute.volume}
        </dd>
    </dl>
</f:view>
```
## f:viewParam

The viewParam element is used in a metadata facet.  It allows to initialize
a bean attribute from a request parameter.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| name             | false    | String          | The name of the request parameter from which the value for this component is retrieved on an initial request or to override the stored value on a postback. |

### Example

```
<f:view xmlns:f="http://java.sun.com/jsf/core">
    <f:metadata>
        <f:viewParam id='height' value='#{compute.height}'>
            <f:converter converterId="float" />
        </f:viewParam>
        <f:viewParam id='radius' value='#{compute.radius}'>
            <f:converter converterId="float" />
        </f:viewParam>
        <f:viewAction action="#{compute.run}"/>
    </f:metadata>
    <dl>
        <dt>Height</dt>
        <dd>
            #{compute.height}
        </dd>
         <dt>Radius</dt>
        <dd>
            #{compute.radius}
        </dd>
        <dt>Volume</dt>
        <dd>
            #{compute.volume}
        </dd>
    </dl>
</f:view>
```

