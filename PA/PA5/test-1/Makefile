LEVEL = ..
TESTS	= $(wildcard *.cl)
all:	$(TESTS:.cl=.out)

CGEN    = $(PADIR)/src/cgen-1

include ../Makefile.common

$(CGEN) ::
	make -C ../src cgen-1



