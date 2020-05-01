# Ada Server Faces

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/serverfaces.json)](https://alire.ada.dev/crates/serverfaces)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/serverfaces_unit.json)](https://alire.ada.dev/crates/serverfaces_unit)
[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-Server-Faces.svg)](https://jenkins.vacs.fr/job/Ada-Server-Faces/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-Server-Faces.svg)](https://jenkins.vacs.fr/job/Ada-Server-Faces/)
[![codecov](https://codecov.io/gh/stcarrez/ada-asf/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/ada-asf)
[![Download](https://img.shields.io/badge/download-1.4.0-brightgreen.svg)](http://download.vacs.fr/ada-asf/ada-asf-1.4.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-asf/1.4.0.svg)

Ada Server Faces allows to create web applications using the same pattern
as the Java Server Faces (See JSR 252, JSR 314 and JSR 344). 

It is part of [Ada Web Application](https://github.com/stcarrez/ada-awa/)
framework.

To build ASF, you will need:

* Ada Util     (https://github.com/stcarrez/ada-util          2.1.0)
* Ada EL       (https://github.com/stcarrez/ada-el            1.8.0)
* Ada Security (https://github.com/stcarrez/ada-security      1.3.0)
* Ada Servlet  (https://github.com/stcarrez/ada-servlet       1.4.0)
* AWS          (https://libre.adacore.com/libre/tools/aws/     2018 or 20)
* XML/Ada      (https://libre.adacore.com/libre/tools/xmlada/  4.4)

## Version 1.4.0 - May 2020

- Performance improvement for the Facelet cache
- Integrate jQuery 3.4.1, jQuery UI 1.12.1, jQuery Chosen 1.8.7
- New <f:validateRegex> to validate an input field with a regular expression

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

The Ada Server Faces sources as well as a wiki documentation is provided on:

   https://github.com/stcarrez/ada-asf/wiki

The following tags are supported:

| Tag | Namespace | Documentation |
|-----|-----------|-------------- |
| JSTL | xmlns:c="http://java.sun.com/jstl/core" | https://demo.vacs.fr/demo/jstl/view.html |
| Facelet | xmlns:ui="http://java.sun.com/jsf/facelets" | https://demo.vacs.fr/demo/facelet/view.html |
| JSF | xmlns:f="http://java.sun.com/jsf/core" | https://demo.vacs.fr/demo/jsf/core/view.html |
| JSF | xmlns:h="http://java.sun.com/jsf/html" | https://demo.vacs.fr/demo/jsf/html/view.html |
| Widgets | xmlns:w="http://code.google.com/p/ada-asf/widget" | https://demo.vacs.fr/demo/widgets/view.html |
| Util | xmlns:util="http://code.google.com/p/ada-asf/util" | https://demo.vacs.fr/demo/util/view.html |


# Licenses

Ada Server Faces integrates the Javascript library jQuery licensed under
MIT or GPL (See https://jquery.org/license/).

Ada Server Faces integrates a generated version of 960 grid system
licensed under MIT or GPL (See https://960.gs/ and https://grids.heroku.com/
for the CSS generator). 
