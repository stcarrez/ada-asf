# Installation

This chapter explains how to build and install the framework.

## Before Building

To build the Ada Keystore you will need the GNAT Ada compiler as well
as the [Alire](https://alire.ada.dev/) package manager.

The Ada Server Faces Library is available as several Alire crates to simplify the installation
and setup your project.  Run the following commands to setup your project to use the library:

```
alr index --update-all
alr with serverfaces
alr with serverfaces_unit
```

If you want to use the Ada Server Faces library in a web server, you must choose a servlet
web container that will handle the requests.  Two web server implementations are provided:

* [AWS](https://github.com/AdaCore/aws)
* [EWS](https://github.com/simonjwright/ews)

and you should run one of the following `alr` command depending on your choice:

```
alr with servletada_aws
alr with servletada_ews
```

## Build for Ada Server Faces development

You can also build and install the library and install it as follows
(but the Alire setup is prefered):

```
make
```

After building, it is good practice to run the unit tests before installing the library.
The unit tests are built and executed using:
```
make test
```
And unit tests are executed by running the `bin/asf_harness` test program.

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

