NAME=asf

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

# Build and run the unit tests
check test:	build runtest

runtest:
	DIR=`pwd`; \
	export LD_LIBRARY_PATH="$$DIR/lib/asf/relocatable:$$DIR/lib/asfunit/relocatable:$$LD_LIBRARY_PATH"; \
	export PATH="$$DIR/lib/asf/relocatable:$$DIR/lib/asfunit/relocatable:$$PATH"; \
	bin/asf_harness -l $(NAME): -xml asf-aunit.xml -config test.properties

build-test::	setup
	$(GNATMAKE) $(GPRFLAGS) -p -Pasf_tests $(MAKE_ARGS)

install:: install-data

install-data::
	${MKDIR} -p $(DESTDIR)${dynamodir}/asf/bundles
	${MKDIR} -p $(DESTDIR)${dynamodir}/asf/web
	(cd web && tar --exclude='*~' -cf - . )| (cd $(DESTDIR)${dynamodir}/asf/web && tar xf -)
	${CP} bundles/*.properties $(DESTDIR)${dynamodir}/asf/bundles/
	${CP} dynamo.xml $(DESTDIR)${dynamodir}/asf/
	${CP} NOTICE.txt $(DESTDIR)${dynamodir}/asf/
	${CP} LICENSE.txt $(DESTDIR)${dynamodir}/asf/

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
  pagebreak.tex

DOC_OPTIONS=-f markdown --listings --number-sections --toc
HTML_OPTIONS=-f markdown --listings --number-sections --toc --css pandoc.css

$(eval $(call pandoc_build,asf-book,$(ASF_DOC),\
	rm -f docs/user-list.md docs/alloc-sequence.md docs/user_hbm.md; \
	cat docs/ASF_Components.md > docs/ADO_Model.md))

$(eval $(call ada_library,asf))
$(eval $(call ada_library,asf_unit))
$(eval $(call alire_publish,alire.toml,se/serverfaces,serverfaces-$(VERSION).toml))
$(eval $(call alire_publish,alire-unit.toml,se/serverfaces_unit,serverfaces_unit-$(VERSION).toml))
