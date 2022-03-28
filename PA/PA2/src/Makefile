SUPPORTDIR= ../cool-support
LIB= 
YSRC= cool.y
BISONCGEN= cool-parse.cc
BISONHGEN= cool-parse.h
COMMON_CSRC= stringtab.cc handle_flags.cc utilities.cc
BISON_CSRC= parser-phase.cc dumptype.cc tree.cc cool-tree.cc tokens-lex.cc 
BISON_CFILES= $(BISON_CSRC) ${BISONCGEN} ${COMMON_CSRC}
BISON_OBJS= ${BISON_CFILES:.cc=.o} 
CFLAGS= -g -Wall -Wno-unused -Wno-deprecated -DDEBUG ${CPPINCLUDE}
BFLAGS= -d -v -y -b cool --debug -p cool_yy
CPPINCLUDE= -I. -I${SUPPORTDIR}/include 
CC= g++
BISON= bison

all: parser
parser: ${BISON_OBJS}
	${CC} ${CFLAGS} ${BISON_OBJS} ${LIB} -o parser

.cc.o:
	${CC} ${CFLAGS} -c $<

${BISONCGEN} ${BISONHGEN}: ${YSRC}
	${BISON} ${BFLAGS} ${YSRC}
	mv -f ${YSRC:.y=.tab.c} ${BISONCGEN}

${BISON_CSRC} ${COMMON_CSRC}:
	-ln -s ${SUPPORTDIR}/src/$@ $@

clean :
	-rm -f core ${BISON_OBJS} ${BISONCGEN} ${BISONHGEN} ${YSRC:.y=.tab.h} \
        lexer parser *~ *.output

realclean: clean
	-rm -f ${BISON_CSRC} ${COMMON_CSRC}
