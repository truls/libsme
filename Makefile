mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))

runsme: lib
	sed 's~%libsmedir%~$(mkfile_dir)/lib~g' tools/runsme.in > tools/runsme
	chmod +x tools/runsme

lib:
	-rm lib/*
	stack build
	stack exec -- perl opts.pl libsme
	ln -rs lib/libHSlibsme-0.1.0.0-* lib/libsme.so

clean:
	stack clean
	-rm lib/*
	rm tools/runsme

.PHONY: lib
