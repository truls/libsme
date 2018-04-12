lib:
	-rm lib/*
	stack build
	stack exec -- perl opts.pl libsme
	ln -rs lib/libHSlibsme-0.1.0.0-* lib/libsme.so

.PHONY: lib
