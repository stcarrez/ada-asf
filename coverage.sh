#!/bin/sh
lcov --base-directory . --directory . -c -o asf.cov >/dev/null
bin/asf_harness -t 120 -xml asf-aunit.xml -config test.properties
(lcov --base-directory . --directory . -c -o asf.cov
lcov --remove asf.cov "/usr*" -o asf.cov 
lcov --remove asf.cov "/opt*" -o asf.cov 
lcov --remove asf.cov "regtests*" -o asf.cov
lcov --remove asf.cov "*/ada-util/*" -o asf.cov
lcov --remove asf.cov ada-asf/b__asf_harness.adb -o asf.cov ) >/dev/null
rm -rf cover
genhtml -o ./cover -t "test coverage" --num-spaces 4 asf.cov
 
