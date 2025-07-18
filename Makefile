NAME=asf
VERSION=1.6.2

DIST_DIR=ada-asf-$(VERSION)
DIST_FILE=ada-asf-$(VERSION).tar.gz

MAKE_ARGS += -XASF_BUILD=$(BUILD)

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XASF_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XASF_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XSERVLETADA_CORE_BUILD=relocatable
SHARED_MAKE_ARGS += -XSERVLET_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XSERVLETADA_UNIT_BUILD=relocatable
SHARED_MAKE_ARGS += -XSERVLET_UNIT_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XELADA_BUILD=relocatable -XEL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XSECURITYADA_BUILD=relocatable -XSECURITY_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable -XAWS_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTILADA_HTTP_AWS_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTILADA_HTTP_AWS_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_UNIT_BUILD=relocatable
SHARED_MAKE_ARGS += -XUTIL_UNIT_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

DEFAULT_ADA_PROJECT_PATH=$(SRC_ROOT):$(SRC_ROOT)/unit:$(ADA_PROJECT_PATH)

# Build and run the unit tests
check test:	build runtest

runtest:
	DIR=`pwd`; \
	export LD_LIBRARY_PATH="$$DIR/lib/asf/relocatable:$$DIR/lib/asfunit/relocatable:$$LD_LIBRARY_PATH"; \
	export PATH="$$DIR/lib/asf/relocatable:$$DIR/lib/asfunit/relocatable:$$PATH"; \
	bin/asf_harness -l $(NAME): -xml asf-aunit.xml -config test.properties

build-test::	lib-setup
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS) 

samples:
	cd samples && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

uninstall::
	rm -rf $(DESTDIR)${dynamodir}/asf

ASF_DOC= \
  title.md \
  pagebreak.tex \
  index.md \
  pagebreak.tex \
  Installation.md \
  pagebreak.tex \
  Tutorial.md \
  pagebreak.tex \
  ASF_Lifecycles.md \
  pagebreak.tex \
  ASF_Converters.md \
  pagebreak.tex \
  ASF_Validators.md \
  pagebreak.tex \
  ASF_Components.md \
  pagebreak.tex \
  ASF_Views_Nodes_Facelets.md \
  pagebreak.tex \
  ASF_Views_Nodes_Core.md \
  pagebreak.tex \
  ASF_Components_Core.md \
  pagebreak.tex \
  ASF_Components_Html.md \
  pagebreak.tex \
  ASF_Components_Utils.md \
  pagebreak.tex \
  ASF_Components_Widgets.md \
  pagebreak.tex \
  Tips.md \
  pagebreak.tex

DOC_OPTIONS=-f markdown --listings --number-sections --toc
HTML_OPTIONS=-f markdown --listings --number-sections --toc --css pandoc.css

$(eval $(call pandoc_build,asf-book,$(ASF_DOC),\
	rm -f docs/user-list.md docs/alloc-sequence.md docs/user_hbm.md))

$(eval $(call ada_library,asf,.))
$(eval $(call ada_library,asf_unit,unit))
$(eval $(call alire_publish,.,se/serverfaces,serverfaces-$(VERSION).toml))
$(eval $(call alire_publish,asfunit,se/serverfaces_unit,serverfaces_unit-$(VERSION).toml))

.PHONY: samples
