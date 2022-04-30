//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include <string>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#include "symtab.h"
#include "operand.h"

using std::string;

class CgenEnvironment;

#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class CgenNode;

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

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

#define Program_EXTRAS                          \
virtual void cgen(ostream&) = 0;		\
virtual void dump_with_types(ostream&, int) = 0; 

#define program_EXTRAS                          \
void cgen(ostream&);     			\
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                   \
virtual Symbol get_name() = 0;  	\
virtual Symbol get_parent() = 0;    	\
virtual Symbol get_filename() = 0;      \
virtual void dump_with_types(ostream&,int) = 0; 


#define class__EXTRAS                                  \
Symbol get_name()   { return name; }		       \
Symbol get_parent() { return parent; }     	       \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);                    


#define Feature_EXTRAS                     		\
virtual void dump_with_types(ostream&,int) = 0; 	\
virtual void layout_feature(CgenNode *cls) = 0;		\
virtual void code(CgenEnvironment *env) = 0;


#define Feature_SHARED_EXTRAS                           \
void dump_with_types(ostream&,int);  			\
void layout_feature(CgenNode *cls);			\
void code(CgenEnvironment *env);


#define method_EXTRAS			\
virtual Symbol get_return_type() { return return_type; }

#define Formal_EXTRAS                              \
virtual Symbol get_type_decl() = 0;                /* ## */ \
virtual Symbol get_name()      = 0;                /* ## */ \
virtual void dump_with_types(ostream&,int) = 0;


#define formal_EXTRAS                           \
Symbol get_type_decl() { return type_decl; }    /* ## */ \
Symbol get_name()      { return name; }         /* ## */ \
void dump_with_types(ostream&,int);


#define Case_EXTRAS                             \
virtual Symbol get_type_decl() = 0; 		\
virtual operand code(operand, operand, const op_type,  \
	CgenEnvironment *) = 0;	\
virtual void dump_with_types(ostream& ,int) = 0;


#define branch_EXTRAS                                   	\
Symbol get_type_decl() { return type_decl; } 			\
Expression get_expr() { return expr; }		\
operand code(operand expr_val, operand tag, 	\
	const op_type join_type, CgenEnvironment *env); 	\
void dump_with_types(ostream& ,int);


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual int no_code() { return 0; }          /* ## */ \
virtual void dump_with_types(ostream&,int) = 0;  \
virtual operand code(CgenEnvironment *)=0;	   \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; }

#define Expression_SHARED_EXTRAS           \
operand code(CgenEnvironment *);	   \
void dump_with_types(ostream&,int); 

#define no_expr_EXTRAS        /* ## */ \
int no_code() { return 1; }   /* ## */

#endif /* COOL_TREE_HANDCODE_H */
