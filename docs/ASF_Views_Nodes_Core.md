# JSTL Components
The JSTL components are defined in the following namespace:
```
xmlns:c="http://java.sun.com/jstl/core"
```

The facelet components are implemented by the `ASF.Views.Nodes.Facelets`
package which defines the pre-defined tags for composing a view.  Nodes of
this package are instantiated when the facelet XML tag is found when reading
the XHTML view description.

## c:choose

This tag associates a parameter name-value pair with the nearest parent UIComponent.
A UIComponent is created to represent this name-value pair, and stored as a child
of the parent component; what effect this has depends upon the renderer of that
parent component.

Unless otherwise specified, all attributes accept static values or EL expressions.

### Attributes

No attributes are defined.

### Example

```XML
<c:choose>
  <c:when test="#{not empty compute.radius}">
    Radius is not empty: #{compute.radius}.
  </c:when>
  <c:when test="#{empty compute.radius}">
    Radius is empty
  </c:when>
</c:choose>
```
## c:if

Simple conditional tag, which evalutes its body if the supplied condition is true
and optionally exposes a Boolean scripting variable representing the evaluation of
this condition.

### Attributes

| Name  | Type            | Description                                        |
| ----- | --------------- | -------------------------------------------------- |
| test  | Boolean         | The test condition that determines whether or not the body content should be processed. |
| var   | String          | Name of the exported scoped variable for the resulting value of the test condition. The type of the scoped variable is Boolean. |

### Example

```XML
<c:if test="#{not empty compute.radius}">
  Radius is not empty: #{compute.radius}.
</c:if>
<c:if test="#{empty compute.radius}">
  Radius is empty
</c:if>
```

## c:otherwise

Subtag of `choose` that follows `when` tags and runs only if all of
the prior conditions evaluated to 'false'.

### Attributes

This tag has no attribute.

### Example

```XML
<c:choose>
  <c:when test="#{not empty compute.radius}">
    Radius is not empty: #{compute.radius}.
  </c:when>
  <c:otherwise>
    Radius is empty
  </c:otherwise>
</c:choose>
```
## c:set

Sets the result of an expression evaluation based on the value of the attributes.

### Attributes

| Name  | Type            | Description                                        |
| ----- | --------------- | -------------------------------------------------- |
| var   | String          | Name of the variable.                              |
| value | ValueExpression | Expression to be evaluated.                        |

### Example

```XML
<c:set var="name" value="23"/>
#{name}
<c:set var="name" value="#{23 + 1}"/>
#{name}
```

## c:when

Subtag of `choose` that includes its body if its condition evalutes to 'true'.

### Attributes

| Name  | Type    | Description                                        |
| ----- | --------| -------------------------------------------------- |
| test  | Boolean | The test condition that determines whether or not the body content should be processed. |

### Example

```XML
<c:choose>
  <c:when test="#{not empty compute.radius}">
    Radius is not empty: #{compute.radius}.
  </c:when>
  <c:when test="#{empty compute.radius}">
    Radius is empty
  </c:when>
</c:choose>
```

The <b>ASF.Views.Nodes.Core</b> package defines some pre-defined
core tag nodes which are mapped in the following namespaces:

```Ada
xmlns:c="http://java.sun.com/jstl/core"
xmlns:ui="http://java.sun.com/jsf/facelets"
xmlns:fn="http://java.sun.com/jsp/jstl/functions"
```

