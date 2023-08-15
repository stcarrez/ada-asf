# Converters
The `ASF.Converters` package defines an interface used by the conversion model
to translate an object into a string when formatting the response and translate
a string into an object during the apply request or validation phases (JSF postback).

The `Converter` interface defines two functions for the convertion of a object
to a string and (`To_String`) and convert back a string to an object (`To_Object`).
See JSR 314 - JavaServer Faces Specification 3.3.2 Converter
(To_String is the JSF getAsString method and To_Object is the JSF getAsObject method)

## Date converter
The `ASF.Converters.Dates` defines the date converter to format a date object
into a localized representation.  It is automatically created when the
`f:convertDateTime` tag is used in the facelet file, for example as follows:

```Ada
 <h:outputText value='#{messages.today}'>
    <f:convertDateTime dateStyle="short"/>
 </h:outputText>
```

## Number converter
The `ASF.Converters.Numbers` provides a floating point number converter.
It can be used to print floating point numbers in various formats.

## Size converter
The `ASF.Converters.Sizes` defines a converter to display a file size in bytes,
kilo bytes, mega bytes or giga bytes.

