all: cgen-1 cgen-2

LEVEL = ..
include $(LEVEL)/Makefile.common

PASRC = stringtab.cc str_aux.cc operand.cc value_printer.cc handle_flags.cc \
	utilities.cc dumptype.cc cgen_supp.cc cool-tree.cc tree.cc cgen-phase.cc \
	ast-lex.cc ast-parse.cc 

PAINCL = $(wildcard *.h) $(wildcard $(PADIR)/include/*.h)

SUPPORT_OBJS = $(PASRC:.cc=.o)

cgen-1: cgen-1.o  $(SUPPORT_OBJS)
	$(CXX) -o $@ $(LDFLAGS) $+ $(LDLIBS)

cgen-1.o: cgen.cc cgen.h cool-tree.handcode.h $(MPINCL)
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $< -o $@

cgen-2: cgen-2.o $(SUPPORT_OBJS)
	$(CXX) -o $@ $(LDFLAGS) $+ $(LDLIBS)

cgen-2.o : cgen.cc cgen.h cool-tree.handcode.h $(PAINCL)
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) -DPA5 $<  -o $@

VPATH = ../cool-support/src

coolrt.c : coolrt.h

coolrt.o : coolrt.c
	$(CC) -g $(EXTRAFLAGS) -c $< -o $@

coolrt.bc : coolrt.c coolrt.h
	$(LLVMGCC) $(EXTRAFLAGS) -emit-llvm -c coolrt.c -o $@

CLEAN_LOCAL= -rm -f core $(OBJS) cgen-1 cgen-2

