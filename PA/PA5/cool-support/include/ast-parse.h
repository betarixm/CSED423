#ifndef _AST_PARSE_H
#define _AST_PARSE_H
//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

#ifndef _COOL_H_
#define _COOL_H_

#include "cool-io.h"

/* a type renaming */
typedef int Boolean;
class Entry;
typedef Entry *Symbol;

Boolean copy_Boolean(Boolean);
void assert_Boolean(Boolean);
void dump_Boolean(ostream &,int,Boolean);

Symbol copy_Symbol(Symbol);
void assert_Symbol(Symbol);
void dump_Symbol(ostream &,int,Symbol);

#endif
#include "tree.h"
typedef class Program_class *Program;
typedef class Class__class *Class_;
typedef class Feature_class *Feature;
typedef class Formal_class *Formal;
typedef class Expression_class *Expression;
typedef class Case_class *Case;
typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;
#ifndef BISON_AST_TAB_H
# define BISON_AST_TAB_H

#ifndef YYSTYPE
typedef union {
  int lineno;
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
} yystype;
# define YYSTYPE yystype
# define YYSTYPE_IS_TRIVIAL 1
#endif
# define	PROGRAM	257
# define	CLASS	258
# define	METHOD	259
# define	ATTR	260
# define	FORMAL	261
# define	BRANCH	262
# define	ASSIGN	263
# define	STATIC_DISPATCH	264
# define	DISPATCH	265
# define	COND	266
# define	LOOP	267
# define	TYPCASE	268
# define	BLOCK	269
# define	LET	270
# define	PLUS	271
# define	SUB	272
# define	MUL	273
# define	DIVIDE	274
# define	NEG	275
# define	LT	276
# define	EQ	277
# define	LEQ	278
# define	COMP	279
# define	INT	280
# define	STR	281
# define	BOOL	282
# define	NEW	283
# define	ISVOID	284
# define	NO_EXPR	285
# define	OBJECT	286
# define	NO_TYPE	287
# define	STR_CONST	288
# define	INT_CONST	289
# define	ID	290
# define	LINENO	291


extern YYSTYPE ast_yylval;

#endif /* not BISON_AST_TAB_H */
#endif
