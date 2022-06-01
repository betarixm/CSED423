LEVEL = ..
CGEN    = $(PADIR)/src/cgen-2
PA5=1
TESTS	= $(wildcard *.cl)
all:	$(TESTS:.cl=.out)

all-exe: $(TESTS:.cl=.exe)

include ../Makefile.common

$(CGEN) ::
	make -C ../src cgen-2
