# Installation

This chapter explains how to build and install the framework.

## Before Building

Before building the framework, you will need:

* [Ada Utility Library](https://github.com/stcarrez/ada-util),
* [Ada Expression Language Library](https://github.com/stcarrez/ada-el),
* [Ada Security Library](https://github.com/stcarrez/ada-security),
* [Ada Servlet Library](https://github.com/stcarrez/ada-servlet),
* [XML/Ada](http://libre.adacore.com/libre/tools/xmlada/)
* [AWS](http://libre.adacore.com/libre/tools/aws/)

First get, build and install the above components and then get, build and install
the [Ada Server Faces](https://github.com/stcarrez/ada-asf).

## Configuration

The library uses the `configure` script to detect the build environment, check whether XML/Ada,
AWS support are available and configure everything before building.  If some component is missing, the
`configure` script will report an error or it will disable the feature.
The `configure` script provides several standard options
and you may use:

  * `--prefix=DIR` to control the installation directory,
  * `--enable-shared` to enable the build of shared libraries,
  * `--disable-static` to disable the build of static libraries,
  * `--enable-distrib` to build for a distribution and strip symbols,
  * `--disable-distrib` to build with debugging support,
  * `--enable-coverage` to build with code coverage support (`-fprofile-arcs -ftest-coverage`),
  * `--with-ada-util=PATH` to control the installation path of [Ada Utility Library](https://github.com/stcarrez/ada-util),
  * `--with-ada-el=PATH` to control the installation path of [Ada Expression Language Library](https://github.com/stcarrez/ada-el),
  * `--with-ada-security=PATH` to control the installation path of [Ada Security Library](https://github.com/stcarrez/ada-security),
  * `--with-ada-servlet=PATH` to control the installation path of [Ada Servlet Library](https://github.com/stcarrez/ada-servlet),
  * `--help` to get a detailed list of supported options.

In most cases you will configure with the following command:
```
./configure
```

Building to get a shared library can sometimes be a real challenge.  With GNAT 2021, you
can configure as follows:

```
./configure --enable-shared
```

## Build

After configuration is successful, you can build the library by running:
```
make
```

After building, it is good practice to run the unit tests before installing the library.
The unit tests are built and executed using:
```
make test
```
And unit tests are executed by running the `bin/asf_harness` test program.

## Installation
The installation is done by running the `install` target:

```
make install
```

If you want to install on a specific place, you can change the `prefix` and indicate the installation
direction as follows:

```
make install prefix=/opt
```

## Using

To use the library in an Ada project, add the following line at the beginning of your
GNAT project file:

```
with "asf";
```

and if you write unit tests for your server faces components, you can benefit from
the unit testing support by using the following GNAT project:

```
with "asf_unit";
```

