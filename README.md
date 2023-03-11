# Ada Server Faces

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/serverfaces.json)](https://alire.ada.dev/crates/serverfaces)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/serverfaces_unit.json)](https://alire.ada.dev/crates/serverfaces_unit)
[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-Server-Faces.svg)](https://jenkins.vacs.fr/job/Ada-Server-Faces/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-Server-Faces.svg)](https://jenkins.vacs.fr/job/Ada-Server-Faces/)
[![codecov](https://codecov.io/gh/stcarrez/ada-asf/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/ada-asf)
[![Download](https://img.shields.io/badge/download-1.5.0-brightgreen.svg)](http://download.vacs.fr/ada-asf/ada-asf-1.5.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-asf)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-asf/1.5.0.svg)

Ada Server Faces allows to create web applications using the same pattern
as the Java Server Faces (See JSR 252, JSR 314 and JSR 344). 

It is part of [Ada Web Application](https://github.com/stcarrez/ada-awa/)
framework.

To build ASF, you will need:

* Ada Util     (https://github.com/stcarrez/ada-util          2.5.0)
* Ada EL       (https://github.com/stcarrez/ada-el            1.8.5)
* Ada Security (https://github.com/stcarrez/ada-security      1.4.1)
* Ada Servlet  (https://github.com/stcarrez/ada-servlet       1.6.0)
* AWS          (https://libre.adacore.com/libre/tools/aws/     2018 or 20)
* XML/Ada      (https://libre.adacore.com/libre/tools/xmlada/  4.4)

## Version 1.6.0   - Under development
  - Improvement of <f:viewParam> to accept a from EL expression to setup the value
  - Add util:parseJSON() EL function
  - Integrate jQuery UI 1.13.2
  - Fix #4: Support to build with -gnatW8

## Version 1.5.0   - Aug 2022
- New widget <w:progress> to display horizontal/vertical progress bars

[List all versions](https://github.com/stcarrez/ada-asf/blob/master/NEWS.md)

## Build

Build with the following commands:
```
   ./configure
   make
```

The samples can be built using:
```
   gnatmake -Psamples
```
   
The unit tests are built using:
```
   gnatmake -Ptests
```

And unit tests are executed with:
```
   bin/asf_harness
```

# Documentation

* [Ada Server Faces Programmer's Guide](https://ada-asf.readthedocs.io/en/latest/) [PDF](https://github.com/stcarrez/ada-asf/blob/master/docs/asf-book.pdf)
* [Ada Server Faces wiki (old documentation)](https://github.com/stcarrez/ada-asf/wiki)

## Tutorial

* [Ada Server Faces Application Example part 1: the presentation](http://blog.vacs.fr/index.php?post/2011/03/21/Ada-Server-Faces-Application-Example)
* [Ada Server Faces Application Example part 2: the Ada beans](http://blog.vacs.fr/index.php?post/2011/04/10/Ada-Server-Faces-Application-Example-part-2%3A-the-Ada-beans)
* [Ada Server Faces Application Example part 3: the action bean](http://blog.vacs.fr/index.php?post/2011/05/02/Ada-Server-Faces-Application-Example-part-3%3A-the-action-bean)
* [Ada Server Faces Application Example part 4: the server](http://blog.vacs.fr/index.php?post/2011/05/18/Ada-Server-Faces-Application-Example-part-3-the-server)

## Components

The following tags are supported:

| Documentation    | Namespace                                            | Tags                                                                  |
|----------|------------------------------------------------------|---------------------------------------------------------------------- |
| [JSTL]( https://demo.vacs.fr/demo/jstl/view.html) | `xmlns:c="http://java.sun.com/jstl/core"`            | <c:set>, <c:if>, <c:choose>, <c:when>, <c:otherwise> |
| [Facelets](https://demo.vacs.fr/demo/facelet/view.html) | `xmlns:ui="http://java.sun.com/jsf/facelets"`        | <ui:composition>, <ui:define>, <ui:decorate>, <ui:include>, <ui:insert>, <ui:param> |
| [JSF Core](https://demo.vacs.fr/demo/jsf/core/view.html) | `xmlns:f="http://java.sun.com/jsf/core"`             | <f:attribute>, <f:convertDateTime>, <f:converter>, <f:facet>, <f:metadata>, <f:param>, <f:selectItem>, <f:selectItems>, <f:validateLength>, <f:validateLongRange>, <f:validateRegex>, <f:validator>, <f:view>, <f:viewAction>, <f:viewParam> |
| [JSF HTML](https://demo.vacs.fr/demo/jsf/html/view.html) | `xmlns:h="http://java.sun.com/jsf/html"`             | <h:body>, <h:commandButton>, <h:form>, <h:head>, <h:inputFile>, <h:inputHidden>, <h:inputSecret>, <h:inputText>, <h:inputTextarea>, <h:list>, <h:message>, <h:messages>, <h:ouputFormat>, <h:outputLabel>, <h:outputLink>, <h:outputText>, <h:panelGroup>, <h:selectBooleanCheckbox>, <h:selectOneMenu>, <h:selectOneRadio> |
| [Widget](https://demo.vacs.fr/demo/widgets/view.html) | `xmlns:w="http://code.google.com/p/ada-asf/widget"`  | <w:accordion>, <w:autocomplete>, <w:chosen>, <w:inputDate>, <w:inputText>, <w:gravatar>, <w:like>, <w:panel>, <w:tab>, <w:tabView> |
| [Util](https://demo.vacs.fr/demo/util/view.html)     | `xmlns:util="http://code.google.com/p/ada-asf/util"` | <util:escape>, <util:file>, <util:flush>, <util:script> |



# Licenses

Ada Server Faces integrates the Javascript library jQuery licensed under
MIT or GPL (See https://jquery.org/license/).

Ada Server Faces integrates a generated version of 960 grid system
licensed under MIT or GPL (See https://960.gs/ and https://grids.heroku.com/
for the CSS generator). 
