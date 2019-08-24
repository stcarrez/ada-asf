dnl Autoconf configure script for Ada Server Faces
dnl Copyright (C) 2009 - 2019 Stephane Carrez
dnl Written by Stephane Carrez (Stephane.Carrez@gmail.com)
dnl
dnl  Licensed under the Apache License, Version 2.0 (the "License");
dnl  you may not use this file except in compliance with the License.
dnl  You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl  Unless required by applicable law or agreed to in writing, software
dnl  distributed under the License is distributed on an "AS IS" BASIS,
dnl  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl  See the License for the specific language governing permissions and
dnl  limitations under the License.
dnl 
dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/asf.ads)

# Current release settings
ASF_MAJOR_VERSION=1
ASF_MINOR_VERSION=2
ASF_MICRO_VERSION=0
ASF_VERSION=$ASF_MAJOR_VERSION.$ASF_MINOR_VERSION.$ASF_MICRO_VERSION

# Set the version number of Ada Server Faces
AC_SUBST(ASF_VERSION)
AC_SUBST(ASF_MAJOR_VERSION)
AC_SUBST(ASF_MINOR_VERSION)
AC_SUBST(ASF_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(asf)
AM_GNAT_LIBRARY_SETUP(asf_core)
AM_GNAT_LIBRARY_SETUP(asf_aws)

AM_GNAT_FIND_ADA_UTIL([Ada Server Faces])
AM_GNAT_FIND_ADA_EL([Ada Server Faces])
AM_GNAT_FIND_ADA_SECURITY([Ada Server Faces])
AM_GNAT_FIND_ADA_SERVLET([Ada Server Faces])

AM_GNAT_CHECK_AWS(
  [
    ASF_HAVE_AWS=no
    ASF_SERVER=none
    WITH_SERVER=
  ], [
    ASF_SERVER=aws
    ASF_HAVE_AWS=yes
    WITH_SERVER=$ac_cv_gnat_project_with_aws
    WITH_ASF_AWS="with \"asf_aws\";";
  ])
AC_SUBST(ASF_SERVER)
AC_SUBST(ASF_HAVE_AWS)
AC_SUBST(WITH_SERVER)
AC_SUBST(WITH_ASF_AWS)

AC_OUTPUT(
Makefile.conf
asf.gpr
asf_tests.gpr
samples.gpr
asf_unit.gpr
)

