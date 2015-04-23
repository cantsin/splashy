.PHONY: all test

all:
	xbuild /verbosity:quiet src/splashy/splashy.fsproj

test:
	xbuild /verbosity:quiet tests/splashy.Tests/splashy.Tests.fsproj && \
	mono packages/NUnit.Runners/tools/nunit-console.exe -labels -nodots tests/splashy.Tests/bin/Debug/splashy.Tests.dll
