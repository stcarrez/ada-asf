# Widget Components
The `widget` components are specific to Ada Server Faces and they provide high level
components to help in designing and providing a web interface.

```
xmlns:w="http://code.google.com/p/ada-asf/widget"
```

## w:accordion

The `w:accordion` component provides a vertical tab component.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| collapsible      | false    | Boolean         | This flag indicates whether the accordion must close all the sections at once. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |

### Example

```XML
<div xmlns:w="http://code.google.com/p/ada-asf/widget"
     style='overflow: auto; padding: 10px'>
    <w:accordion id="people" collapsible="true">
        <w:tab title="Dennis">
            Dennis MacAlistair Ritchie was an American computer scientist who "helped shape the digital era."
            He created the C programming language and, with long-time colleague Ken Thompson,
            the Unix operating system. Ritchie and Thompson received the Turing Award from the ACM in 1983,
            the Hamming Medal from the IEEE in 1990 and the National Medal of Technology from President Clinton
            in 1999. Ritchie was the head of Lucent Technologies System Software Research Department when he
            retired in 2007. He was the 'R' in K&amp;R C and commonly known by his username dmr.
        </w:tab>
        <w:tab title="Alan">
            Alan Mathison Turing, was an English mathematician, logician, cryptanalyst, and computer scientist.
            He was highly influential in the development of computer science, giving a formalisation of the
            concepts of "algorithm" and "computation" with the Turing machine, which can be considered a model
            of a general purpose computer. Turing is widely considered to be the father of computer science
            and artificial intelligence.

            http://en.wikipedia.org/wiki/Alan_Turing
        </w:tab>
        <w:tab title="Ada">
            Augusta Ada King, Countess of Lovelace (10 December 1815 - 27 November 1852),
            born Augusta Ada Byron and now commonly known as Ada Lovelace, was an English mathematician
            and writer chiefly known for her work on Charles Babbage's early mechanical general-purpose computer,
            the Analytical Engine. Her notes on the engine include what is recognised as the first algorithm
            intended to be processed by a machine. Because of this, she is often described as the world's first
            computer programmer.
        </w:tab>
    </w:accordion>
</div>
```
## w:autocomplete

The `w:autocomplete` component combines the `h:inputText` and `h:message`
components and provides autocomplete functionality on the input field.  It renders the title
and the input form field.  The error message associated
with the input field is rendered if there is one.  The title, input field and message
are combined within an HTML `dl`, `dt` and `dd` elements.

When the user enters some text, the form is submitted for autocompletion.
The `w:autocomplete` component handles the form submission and uses the
`autocompleteList` attribute to find out possible completions.  It then returns
that list that is then displayed by the client.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| title            | false    | String          | The title to display for the input field. |
| autocompleteList | false    | String          | The list of values for the autocompletion. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:w="http://code.google.com/p/ada-asf/widget"
     xmlns:h="http://java.sun.com/jsf/html">
    <div class="ui-widget ui-widget-header">
        Compute the volume of a cylinder
    </div>
    <h:form id='text-form'>
        <w:autocomplete title="Country" id='country' size='30' autocompleteList="#{countries}"
                        value='#{messages.email}'
                        styleClass="ui-corner-all">
        </w:autocomplete>
        <ul class='buttons'>
            <li>
                <h:commandButton id='run' value='Compute' action="#{compute.run}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```
## w:chosen

The `w:chosen` component is a `h:selectOne` component that uses jQuery Chosen support
It renders the `select` component with its options and activates the jQuery Chosen support on it.

The `w:chosen` component uses the `options` facet to allow to provide specific options
to the jQuery Chosen library.  The `events` facet can be used to invoke jQuery specific
operations on the Chosen selector and bind some events.

### Attributes

| Name      | Required | Type            | Description                                        |
| --------- | -------- | --------------- | -------------------------------------------------- |
| id        | true     | String          | The id of the element (this is mandatory for the correct jQuery Chosen support). |
| rendered  | false | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:w="http://code.google.com/p/ada-asf/widget"
     style='padding: 10px'>
    <div style="padding: 10px">
        <w:chosen value='#{compute.height}' id="chosen-example-1">
            <f:facet name="options">width: "95%", disable_search: true</f:facet>
            <f:facet name="events">.bind("change", function(event, params) {
                alert("Selected country: " + $(this).val());
            } )
            </f:facet>
            <f:selectItems value="#{countries}"/>
        </w:chosen>
    </div>
    <div style="padding: 10px">
        <w:chosen value='#{compute.height}' id="chosen-example-2">
            <f:facet name="options">width: "95%"</f:facet>
            <f:facet name="events">.bind("change", function(event, params) {
                alert("Selected country: " + $(this).val());
            } )
            </f:facet>
            <f:selectItems value="#{countries}"/>
        </w:chosen>
    </div>
</div>
```
## w:gravatar

This component renders an image whose link is the gravatar's link of
a person's email address.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| email            | true     | String          | The email address to create the gravatar image link. |
| secure           | false    | Boolean         | When True, the secure link to the gravatar image is created (https).  The default creates an `http` link only. |
| size             | false    | Integer         | The size of the gravatar image (from 1 to 2048 pixels max). |
| default          | false    | String          | Default. |
| alt              | false    | String          | The image `alt` attribute. By default, the email address is used for the `alt` attribute. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:w="http://code.google.com/p/ada-asf/widget"
     xmlns:h="http://java.sun.com/jsf/html" style='overflow: auto;'>
    <div class="ui-widget ui-widget-header">
        Display a person's gravatar
    </div>
    <h:form id='text-form'>
        <w:inputText title="Type the person email address" id='email' size='40'
                     value='#{message.email}'
                     styleClass="ui-corner-all">
        </w:inputText>
        <div class='gravatar grid_6'>
            <span>default</span>
            <w:gravatar email='#{message.email}' size='120'/>
        </div>
        <div class='gravatar grid_6'>
            <span>wavatar</span>
            <w:gravatar email='#{message.email}' default='wavatar' size='120'/>
        </div>
        <div class='gravatar grid_6'>
            <span>retry</span>
            <w:gravatar email='#{message.email}' default='retro' size='120'/>
        </div>
        <div class='gravatar grid_6'>
            <span>monsterid</span>
            <w:gravatar email='#{message.email}' default='monsterid' size='120'/>
        </div>
        <ul class='buttons'>
            <li>
                <h:commandButton id='run' value='Display gravatar'
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```
## w:inputDate

The `w:inputDate` component combines the `h:inputText` and `h:message`
components and a date picker to select a date.  It renders the title and the input form field with
the Javascript support to activate the date picker.  The error message associated
with the input field is rendered if there is one.  The title, input field and message
are combined within an HTML `dl`, `dt` and `dd` elements.

The date picker is based on the jQuery date picker.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| title            | false    | String          | The title to display for the input field. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:w="http://code.google.com/p/ada-asf/widget"
     xmlns:h="http://java.sun.com/jsf/html">
    <div class="ui-widget ui-widget-header">
        Select a date
    </div>
    <h:form id='text-form'>
        <w:inputDate title="Message date" id='date' size='20'
                     value='#{message.text}'
                     dateFormat="yy-dd-mm"
                     styleClass="ui-corner-all">
        </w:inputDate>
        <w:inputText title="Email" id='email' size='10'
                     value='#{message.email}'
                     styleClass="ui-corner-all">
        </w:inputText>
        <ul class='buttons'>
            <li>
                <h:commandButton id='run' value='Post' action="#{message.post}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```
## w:inputText

The `w:inputText` component combines the `h:inputText` and `h:message`
components.  It renders the title and the input form field.  The error message associated
with the input field is rendered if there is one.  The title, input field and message
are combined within an HTML `dl`, `dt` and `dd` elements.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| title            | false    | String          | The title to display for the input field. |
| id               | false    | String          | The component identifier for this component. This value must be unique within the closest parent component that is a naming container. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |
| converterMessage        | false    | String          | A ValueExpression enabled attribute that, if present, will be used as the text of the converter message, replacing any message that comes from the converter. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:w="http://code.google.com/p/ada-asf/widget"
     xmlns:h="http://java.sun.com/jsf/html">
    <div class="ui-widget ui-widget-header">
        Compute the volume of a cylinder
    </div>
    <h:form id='text-form'>
        <w:inputText title="Height" id='height' size='10'
                     value='#{compute.height}'
                     styleClass="ui-corner-all">
            <f:converter converterId="float" />
        </w:inputText>
        <w:inputText title="Radius" id='radius' size='10'
                     value='#{compute.radius}'
                     styleClass="ui-corner-all">
            <f:converter converterId="float" />
        </w:inputText>
        <ul class='buttons'>
            <li>
                <h:commandButton id='run' value='Compute' action="#{compute.run}"
                                 styleClass="ui-button ui-state-default ui-corner-all"/>
            </li>
        </ul>
    </h:form>
</div>
```
## w:like

This component renders a like button for Facebook or Twitter (more like implementations
can be added programatically in Ada).  The like button code is rendered within a `div` element
whose style and class can be customized.

The `kind` attribute defines what like button must be generated.

### Facebook

The [Facebook like button](https://developers.facebook.com/docs/plugins/like-button/)
is generated with the `facebook` kind attribute value.

When using the Facebook button, the component will pass several attributes to the Facebook
button: data-layout, data-show-faces, data-width, data-action, data-font,
data-colorscheme, data-ref, data-kid_directed_site, data-send.

The Facebook like button requires that you register your application and get a facebook client ID.
The like component will use the configuration property `facebook.client_id` to retrieve
this client ID.

### Twitter

The [Tweet Button](https://dev.twitter.com/docs/tweet-button) is
generated with the `twitter` kind attribute value.  The following attributes are
passed to the Tweet button: data-via, data-count, data-size.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| kind             | true     | String          | The type of like button to generate: "facebook", "twitter". |
| href             | false    | String          | The optional URL to pass to the like button.  The default is to use the current page URL. |
| styleClass       | false    | String          | The CSS class to be applied in the div element that contains the like button. |
| style            | false    | String          | The CSS style to be applied in the div element that contains the like button. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |

### Example

```
<div xmlns:w="http://code.google.com/p/ada-asf/widget" style='overflow: auto;'>
    <div class="ui-widget ui-widget-header">
        Display a social like button
    </div>
    <w:like type="facebook" href="http://www.google.com"/>
    <w:like type="twitter" data-count="vertical"/>
</div>
```
## w:panel

The `w:panel` component provides a collapsible panel with a header, a content and an optional
footer.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| header           | true     | String          | The header title to display at the top of the panel. |
| footer           | false    | String          | The optional title to display at the bottom of the panel. |
| closable         | false    | Boolean         | When true, the panel can be closed by clicking on the close icon action in the header. |
| toggleable       | false    | Boolean         | When true, the panel can be collapsed by clicking on the expand/collapse icon action in the header. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:w="http://code.google.com/p/ada-asf/widget"
     xmlns:h="http://java.sun.com/jsf/html" style='overflow: auto; padding: 10px'>
    <w:panel header='Ada Lovelace' closable="true" toggleable="true">
        Augusta Ada King, Countess of Lovelace (10 December 1815 - 27 November 1852),
        born Augusta Ada Byron and now commonly known as Ada Lovelace, was an English mathematician
        and writer chiefly known for her work on Charles Babbage's early mechanical general-purpose computer,
        the Analytical Engine. Her notes on the engine include what is recognised as the first algorithm
        intended to be processed by a machine. Because of this, she is often described as the world's first
        computer programmer.

        <f:facet name="footer">
            <a href="http://en.wikipedia.org/wiki/Ada_Lovelace">
                More on wikipedia
            </a>
        </f:facet>
    </w:panel>
</div>
```
## w:tab

The `w:tab` component defines a tab content to be displayed within a tab selection.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| title            | false    | String          | The tab title. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |

### Example

```XML
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:w="http://code.google.com/p/ada-asf/widget"
     style='overflow: auto; padding: 10px'>
    <w:tabView header='Ada Lovelace' closable="true" toggleable="true" effect="blind" collapsible="true">
        <w:tab title="Dennis">
            Dennis MacAlistair Ritchie was an American computer scientist who "helped shape the digital era."
            He created the C programming language and, with long-time colleague Ken Thompson,
            the Unix operating system. Ritchie and Thompson received the Turing Award from the ACM in 1983,
            the Hamming Medal from the IEEE in 1990 and the National Medal of Technology from President Clinton
            in 1999. Ritchie was the head of Lucent Technologies System Software Research Department when he
            retired in 2007. He was the 'R' in K&amp;R C and commonly known by his username dmr.
        </w:tab>
        <w:tab title="Alan">
            Alan Mathison Turing, was an English mathematician, logician, cryptanalyst, and computer scientist.
            He was highly influential in the development of computer science, giving a formalisation of the
            concepts of "algorithm" and "computation" with the Turing machine, which can be considered a model
            of a general purpose computer. Turing is widely considered to be the father of computer science
            and artificial intelligence.

            http://en.wikipedia.org/wiki/Alan_Turing
        </w:tab>
        <w:tab title="Ada">
            Augusta Ada King, Countess of Lovelace (10 December 1815 - 27 November 1852),
            born Augusta Ada Byron and now commonly known as Ada Lovelace, was an English mathematician
            and writer chiefly known for her work on Charles Babbage's early mechanical general-purpose computer,
            the Analytical Engine. Her notes on the engine include what is recognised as the first algorithm
            intended to be processed by a machine. Because of this, she is often described as the world's first
            computer programmer.
        </w:tab>
    </w:tabView>
</div>
```
## w:tabView

The `w:tabView` component defines a tab selection.
It uses the [jQuery UI tabs](http://api.jqueryui.com/tabs/).
Each tab must be represented by a `w:tab` component which indicates the tab
title and content.

### Attributes

| Name             | Required | Type            | Description                                        |
| ---------------- | -------- | --------------- | -------------------------------------------------- |
| collapsible      | false    | Boolean         | When true, the tabs are collapsible. |
| effect           | false    | String          | The effect to use when switching tabs. |
| duration         | false    | Integer         | The effect duration. |
| rendered         | false    | Boolean         | Flag indicating whether or not this component should be rendered (during Render Response Phase), or processed on any subsequent form submit. The default value for this property is true. |

### Example

```
<div xmlns:f="http://java.sun.com/jsf/core"
     xmlns:w="http://code.google.com/p/ada-asf/widget"
     style='overflow: auto; padding: 10px'>
    <w:tabView header='Ada Lovelace' closable="true" toggleable="true" effect="blind" collapsible="true">
        <w:tab title="Dennis">
            Dennis MacAlistair Ritchie was an American computer scientist who "helped shape the digital era."
            He created the C programming language and, with long-time colleague Ken Thompson,
            the Unix operating system. Ritchie and Thompson received the Turing Award from the ACM in 1983,
            the Hamming Medal from the IEEE in 1990 and the National Medal of Technology from President Clinton
            in 1999. Ritchie was the head of Lucent Technologies System Software Research Department when he
            retired in 2007. He was the 'R' in K&amp;R C and commonly known by his username dmr.
        </w:tab>
        <w:tab title="Alan">
            Alan Mathison Turing, was an English mathematician, logician, cryptanalyst, and computer scientist.
            He was highly influential in the development of computer science, giving a formalisation of the
            concepts of "algorithm" and "computation" with the Turing machine, which can be considered a model
            of a general purpose computer. Turing is widely considered to be the father of computer science
            and artificial intelligence.

            http://en.wikipedia.org/wiki/Alan_Turing
        </w:tab>
        <w:tab title="Ada">
            Augusta Ada King, Countess of Lovelace (10 December 1815 - 27 November 1852),
            born Augusta Ada Byron and now commonly known as Ada Lovelace, was an English mathematician
            and writer chiefly known for her work on Charles Babbage's early mechanical general-purpose computer,
            the Analytical Engine. Her notes on the engine include what is recognised as the first algorithm
            intended to be processed by a machine. Because of this, she is often described as the world's first
            computer programmer.
        </w:tab>
    </w:tabView>
</div>
```

