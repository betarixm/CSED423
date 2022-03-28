SUPPORTDIR= ../cool-support
LIB= 
SRC= semant.cc semant.h cool-tree.h cool-tree.handcode.h 
CSRC= semant-phase.cc symtab_example.cc  handle_flags.cc  ast-lex.cc ast-parse.cc utilities.cc stringtab.cc dumptype.cc tree.cc cool-tree.cc
CFLAGS= -g -Wall -Wno-unused -Wno-deprecated -DDEBUG ${CPPINCLUDE}
CPPINCLUDE= -I. -I${SUPPORTDIR}/include 
CC= g++
CFIL= semant.cc ${CSRC} 
OBJS= ${CFIL:.cc=.o}


all: semant

.cc.o:
	${CC} ${CFLAGS} -c $<

SEMANT_OBJS := ${filter-out symtab_example.o,${OBJS}}

semant:  ${SEMANT_OBJS} 
	${CC} ${CFLAGS} ${SEMANT_OBJS} ${LIB} -o semant

symtab_example: symtab_example.cc 
	${CC} ${CFLAGS} symtab_example.cc ${LIB} -o symtab_example

${CSRC}:
	-ln -s $(SUPPORTDIR)/src/$@ $@


clean :
	-rm -f core ${SEMANT_OBJS} semant *~ *.output

realclean: clean
	-rm -f ${CSRC} 
