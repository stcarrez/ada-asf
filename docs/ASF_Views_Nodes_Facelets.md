# Facelet Components
The facelets is the default view declaration language that uses XML and XHTML.
It is a composition and templating framework that allows to create the component
tree.

The facelet components are defined in the following namespace:
```
xmlns:ui="http://java.sun.com/jsf/facelets"
```

The facelet components are implemented by the `ASF.Views.Nodes.Facelets`
package which defines the pre-defined tags for composing a view.  Nodes of
this package are instantiated when the facelet XML tag is found when reading
the XHTML view description.

## ui:composition

Defines a composition that optionally uses a template, as outlined in the description
of the ui tag library. Multiple compositions can use the same template, thus encapsulating
and reusing layout. JSF disregards everything outside of the composition, which lets
developers embed compositions in well-formed XHTML pages that can be viewed in an XHTML viewer,
such as Dreamweaver or a browser, without including extraneous elements such as head and body.

### Attributes

| Name  | Required | Type            | Description                                        |
| ----- | -------- | --------------- | -------------------------------------------------- |
| template | false | String          | A URI that points to a template, also known as a layout, that inserts pieces of the page defined in the composition. |

### Example

```XML
<ui:composition xmlns:ui="http://java.sun.com/jsf/facelets">

    <h2>Title #{empty name ? '?' : name}</h2>
    <p>
        <ui:insert name="description">
            There is no description
        </ui:insert>
    </p>
    <h2>List</h2>
    <ul style='padding-left: 20px;'>
        <ui:insert name="list">
            <li>
                There is no list.
            </li>
        </ui:insert>
    </ul>
</ui:composition>
```
## ui:decorate

The decorate tag is identical to the composition tag, except that `ui:decorate`,
unlike `ui:composition`, does not disregard all content outside of the tag.
The decorate is useful when you want to decorate some content in a page, for example,
you might want to decorate a list of items.

### Attributes

| Name  | Required | Type            | Description                                        |
| ----- | -------- | --------------- | -------------------------------------------------- |
| template | true  | String          | A URI that points to a template, also known as a layout, that inserts pieces of the page defined in the decorator. |

### Example

```XML
<ui:decorate xmlns:ui="http://java.sun.com/jsf/facelets"
             template="/facelet/composition.xhtml">
    <ui:param name="name" value="decorate"/>
    <ui:define name="description">
        The decorate tag allows to use a template and insert data
        at various places.
    </ui:define>
    <ui:define name="list">
        <li>The decorate tag uses a template</li>
        <li>It includes optional ui:param elements</li>
        <li>It fills the template by using the ui:define element.</li>
    </ui:define>
</ui:decorate>
```
## ui:define

The define tag defines content that is inserted into a page by a template.
The define tag can be used inside `ui:composition`, `ui:component`, `ui:decorate`,
and `ui:fragment` tags.

### Attributes

| Name  | Required | Type            | Description                                        |
| ----- | -------- | --------------- | -------------------------------------------------- |
| name  | true     | String          | Assigns a name to the content inside a define tag. That name is used by corresponding ui:insert tags in a template that insert the named content into a page. |

### Example

```XML
<ui:decorate xmlns:ui="http://java.sun.com/jsf/facelets"
             template="/facelet/composition.xhtml">
    <ui:param name="name" value="decorate"/>
    <ui:define name="description">
        The decorate tag allows to use a template and insert data
        at various places.
    </ui:define>
    <ui:define name="list">
        <li>The decorate tag uses a template</li>
        <li>It includes optional ui:param elements</li>
        <li>It fills the template by using the ui:define element.</li>
    </ui:define>
</ui:decorate>
```
## ui:include

Use this tag—which is very similar to JSP's jsp:include to encapsulate and reuse
content among multiple XHTML pages. There are three things this tag can include:
plain XHTML, and XHTML pages that have either a composition tag or a component tag.

You supply a filename, through `ui:include`'s src attribute for JSF to include.
That filename is relative to the XHTML file that was rendered as a result of the
last request. So, for example, if JSF loaded the view `login.xhtml`, and that file
included `pageDecorations/header.xhtml`, and `pageDecorations/header.xhtml` included
`companyLogo.xhtml`, then `companyLogo.xhtml` will not be found if it's in the
`pageDecorations` directory, because `companyLogo.xhtml` has to be in the same
directory as `login.xhtml`.

### Attributes

| Name  | Required | Type            | Description                                        |
| ----- | -------- | --------------- | -------------------------------------------------- |
| src   | true     | String          | The filename of an XHTML page to include. The filename is relative to the XHTML page that was originally loaded. |

### Example

```XML
<div xmlns:ui="http://java.sun.com/jsf/facelets">
    <ui:include src="composition.xhtml">
        <ui:param name="name" value="include"/>
    </ui:include>
    <ui:include src="../jstl/if.xhtml"/>
</div>
```
## ui:insert

Inserts content into a template. That content is defined with the `ui:define` tag
in either a `ui:composition`, `ui:component`, `ui:decorate`, or `ui:fragment`.

### Attributes

| Name  | Required | Type            | Description                                        |
| ----- | -------- | --------------- | -------------------------------------------------- |
| name  | true     | String          | The fragment name to insert. |

### Example

```XML
<ui:composition xmlns:ui="http://java.sun.com/jsf/facelets">

    <h2>Title #{empty name ? '?' : name}</h2>
    <p>
        <ui:insert name="description">
            There is no description
        </ui:insert>
    </p>
    <h2>List</h2>
    <ul style='padding-left: 20px;'>
        <ui:insert name="list">
            <li>
                There is no list.
            </li>
        </ui:insert>
    </ul>
</ui:composition>
```
## ui:param

Use this tag to pass parameters to an included file (using `ui:include`),
or a template (linked to either a composition or decorator). Embed `ui:param` tags
in either `ui:include`, `ui:composition`, or `ui:decorate` to pass the parameters.

### Attributes

| Name  | Required | Type            | Description                                        |
| ----- | -------- | --------------- | -------------------------------------------------- |
| name  | true     | String          | The name of the parameter. |
| value | true     | String          | The value of the parameter. Notice that this attribute's value can be an EL expression, which means that you can pass objects to either an included file or a template. |

### Example

```XML
<ui:decorate xmlns:ui="http://java.sun.com/jsf/facelets"
             template="/facelet/composition.xhtml">
    <ui:param name="name" value="decorate"/>
    <ui:define name="description">
        The decorate tag allows to use a template and insert data
        at various places.
    </ui:define>
    <ui:define name="list">
        <li>The decorate tag uses a template</li>
        <li>It includes optional ui:param elements</li>
        <li>It fills the template by using the ui:define element.</li>
    </ui:define>
</ui:decorate>
```

