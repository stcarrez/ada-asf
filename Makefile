
-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XSERVLET_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XSERVLET_LIBRARY_TYPE=relocatable
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
	bin/asf_harness -xml asf-aunit.xml -config test.properties

build-test::	setup regtests/asf-testsuite.adb
	$(GNATMAKE) $(GPRFLAGS) -p -Pasf_tests $(MAKE_ARGS)

regtests/asf-testsuite.adb: regtests/asf-testsuite.gpb Makefile
	gnatprep -DASF_SERVER=$(ASF_SERVER) regtests/asf-testsuite.gpb $@

install::
	${MKDIR} -p ${dynamodir}/asf/bundles
	${MKDIR} -p ${dynamodir}/asf/web
	(cd web && tar --exclude=.svn --exclude='*~' -cf - . )| (cd ${dynamodir}/asf/web && tar xf -)
	${CP} bundles/*.properties ${dynamodir}/asf/bundles/
	${CP} dynamo.xml ${dynamodir}/asf/
	${CP} NOTICE.txt ${dynamodir}/asf/
	${CP} LICENSE.txt ${dynamodir}/asf/

uninstall::
	rm -rf ${dynamodir}/asf

$(eval $(call ada_library,asf))
$(eval $(call ada_library,asf_unit))
