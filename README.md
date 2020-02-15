# Ada Server Faces

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-Server-Faces.svg)](https://jenkins.vacs.fr/job/Ada-Server-Faces/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-Server-Faces.svg)](https://jenkins.vacs.fr/job/Ada-Server-Faces/)
[![codecov](https://codecov.io/gh/stcarrez/ada-asf/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/ada-asf)
[![Download](https://img.shields.io/badge/download-1.3.0-brightgreen.svg)](http://download.vacs.fr/ada-asf/ada-asf-1.2.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-asf/1.3.0.svg)

Ada Server Faces allows to create web applications using the same pattern
as the Java Server Faces (See JSR 252, JSR 314 and JSR 344). 

It is part of [Ada Web Application](https://github.com/stcarrez/ada-awa/)
framework.

To build ASF, you will need:

* Ada Util     (https://github.com/stcarrez/ada-util          2.0.0)
* Ada EL       (https://github.com/stcarrez/ada-el            1.7.0)
* Ada Security (https://github.com/stcarrez/ada-security      1.2.1)
* Ada Servlet  (https://github.com/stcarrez/ada-servlet       1.3.0)
* AWS          (https://libre.adacore.com/libre/tools/aws/     2018 or 20)
* XML/Ada      (https://libre.adacore.com/libre/tools/xmlada/  4.4)

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

# Licenses

Ada Server Faces integrates the Javascript library jQuery licensed under
MIT or GPL (See https://jquery.org/license/).

Ada Server Faces integrates a generated version of 960 grid system
licensed under MIT or GPL (See https://960.gs/ and https://grids.heroku.com/
for the CSS generator). 
