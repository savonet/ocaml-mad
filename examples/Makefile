# OCaml-Mad examples.
#
# Copyright 2003-2006 by the Savonet team.
#
# $Id$

all:
	$(MAKE) -f Makefile.mp32wav
	$(MAKE) -f Makefile.mp3info

opt:
	$(MAKE) -f Makefile.mp32wav nc
	$(MAKE) -f Makefile.mp3info nc

clean:
	-$(MAKE) -f Makefile.mp32wav clean
	-$(MAKE) -f Makefile.mp3info clean

distclean: clean
	rm -rf autom4te.cache config.log config.status
	rm -f Makefile.mp32wav
	rm -f Makefile.mp3info

.PHONY: mp32wav mp3info
