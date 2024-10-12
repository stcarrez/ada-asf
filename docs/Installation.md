# Installation

This chapter explains how to build and install the framework.

## Using Alire

If you are using [Alire](https://alire.ada.dev/) in your project, run the following command
within your [Alire](https://alire.ada.dev/) project to use the library:

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

## Using without Alire

If you don't have [Alire](https://alire.ada.dev/) or want to build and install the library
on a specific place, run a `setup` command to configure the build as well as installation
directory.

The `HAVE_ALIRE` configuration allows you to disable the build with [Alire](https://alire.ada.dev/).

```
make setup BUILD=debug PREFIX=/build/install HAVE_ALIRE=no
```

Since this build method does not verify that all dependencies are met, make sure that you
have already built and install the following components and they are available to `gprbuild`
through `ADA_PROJECT_PATH` if needed:

* [Ada Servlet](https://gitlab.com/stcarrez/ada-servlet/)
* [Ada Security Library](https://gitlab.com/stcarrez/ada-security/)
* [Ada EL Library](https://gitlab.com/stcarrez/ada-el/)
* [Ada Utility Library](https://gitlab.com/stcarrez/ada-util/)

Then build, run the unit tests and install by using:

```
make
make test
make install
```

To use the installed libraries, make sure your `ADA_PROJECT_PATH` contains the directory
where you installed the libraries (configured by the `PREFIX=<path>` option in the setup phase).
The installed GNAT projects are the same as those used when using [Alire](https://alire.ada.dev/).

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

