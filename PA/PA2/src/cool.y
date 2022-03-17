/*
 *  cool.y
 *              Parser definition for the COOL language.
 *
 */
%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

/* Add your own C declarations here */
#define ERROR_HANDLER() {\
  \
}

/************************************************************************/
/*                DONT CHANGE ANYTHING IN THIS SECTION                  */

extern int yylex();           /* the entry point to the lexer  */
extern int curr_lineno;
extern char *curr_filename;
Program ast_root;            /* the result of the parse  */
Classes parse_results;       /* for use in semantic analysis */
int omerrs = 0;              /* number of errors in lexing and parsing */

/*
   The parser will always call the yyerror function when it encounters a parse
   error. The given yyerror implementation (see below) justs prints out the
   location in the file where the error was found. You should not change the
   error message of yyerror, since it will be used for grading puproses.
*/
void yyerror(const char *s);

/*
   The VERBOSE_ERRORS flag can be used in order to provide more detailed error
   messages. You can use the flag like this:

     if (VERBOSE_ERRORS)
       fprintf(stderr, "semicolon missing from end of declaration of class\n");

   By default the flag is set to 0. If you want to set it to 1 and see your
   verbose error messages, invoke your parser with the -v flag.

   You should try to provide accurate and detailed error messages. A small part
   of your grade will be for good quality error messages.
*/
extern int VERBOSE_ERRORS;

%}

/* A union of all the types that can be the result of parsing actions. */
%union {
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
  char *error_msg;
}

/* 
   Declare the terminals; a few have types for associated lexemes.
   The token ERROR is never used in the parser; thus, it is a parse
   error when the lexer returns it.

   The integer following token declaration is the numeric constant used
   to represent that token internally.  Typically, Bison generates these
   on its own, but we give explicit numbers to prevent version parity
   problems (bison 1.25 and earlier start at 258, later versions -- at
   257)
*/
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/
 
   /* Complete the nonterminal list below, giving a type for the semantic
      value of each non terminal. (See section 3.6 in the bison 
      documentation for details). */

/* Declare types for the grammar's non-terminals. */
%type <program> program
%type <classes> class_list
%type <class_> class

/* You will want to change the following line. */
%type <feature> feature
%type <features> feature_list

%type <formal> formal
%type <formals> formal_list

%type <expression> expression
%type <expressions> expression_comma_list
%type <expressions> expression_semi_nonempty_list

%type <expression> let_body

%type <case_> case
%type <cases> case_body

/* Precedence declarations go here. */
%right ASSIGN
%left NOT
%nonassoc LE '<' '='
%left '+' '-'
%left '*' '/'
%left ISVOID
%left '~'
%left '@'
%left '.'

%%
/* 
   Save the root of the abstract syntax tree in a global variable.
*/
program : class_list { ast_root = program($1); }
        ;

class_list
        : class            /* single class */
                { $$ = single_Classes($1); }
        | class_list class /* several classes */
                { $$ = append_Classes($1,single_Classes($2)); }
        | error {
                ERROR_HANDLER();
        }
        | class_list error {
                ERROR_HANDLER();
        }
        ;

/* If no parent is specified, the class inherits from the Object class. */
class  : CLASS TYPEID '{' feature_list '}' ';'
                { $$ = class_($2,idtable.add_string("Object"),$4,
                              stringtable.add_string(curr_filename)); }
        | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
                { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
        ;

/* Feature list may be empty, but no empty features in list. */
feature_list
        : /* empty */ {
                $$ = nil_Features();
        }
        | feature[feat] ';' {
                $$ = single_Features($feat);
        }
        | feature_list[feats] feature[feat] ';' {
                $$ = append_Features($feats, single_Features($feat));
        }
        | error ';' {
                ERROR_HANDLER();
        }
        | feature_list error ';' {
                ERROR_HANDLER();
        }
        ;

feature
        : OBJECTID[class] '(' formal_list[params] ')' ':' TYPEID[type] '{' expression[expr] '}' {
                $$ = method($class, $params, $type, $expr);
        }
        | OBJECTID[class] ':' TYPEID[type] ASSIGN[assign] expression[expr] {
                $$ = attr($class, $type, $expr);
        }
        | OBJECTID[class] ':' TYPEID[type] {
                $$ = attr($class, $type, no_expr());
        }
        ;

formal
        : OBJECTID[name] ':' TYPEID[type] {
                $$ = formal($name, $type);
        }

formal_list
        : /* empty */ {
                $$ = nil_Formals();
        }
        | formal[form] {
                $$ = single_Formals($form);
        }
        | formal_list[forms] ',' formal[form] {
                $$ = append_Formals($forms, single_Formals($form));
        }
        ;

expression_comma_list
        : /* empty */ {
                $$ = nil_Expressions();
        }
        | expression[expr] {
                $$ = single_Expressions($expr);
        }
        | expression_comma_list[exprs] ',' expression[expr] {
                $$ = append_Expressions($exprs, single_Expressions($expr));
        }
        ;

expression_semi_nonempty_list
        : expression[expr] ';' {
                $$ = single_Expressions($expr);
        }
        | expression_semi_nonempty_list[exprs] expression[expr] ';' {
                $$ = append_Expressions($exprs, single_Expressions($expr));
        }
        | error ';' {
                ERROR_HANDLER();
        }
        | expression_semi_nonempty_list error ';' {
                ERROR_HANDLER();
        }
        ;

expression
        : OBJECTID[variable] ASSIGN expression[expr] {
                $$ = assign($variable, $expr);
        }
        | expression[expr] '@' TYPEID[type] '.' OBJECTID[method] '(' expression_comma_list[params] ')' {
                $$ = static_dispatch($expr, $type, $method, $params);
        }
        | expression[expr] '.' OBJECTID[method] '(' expression_comma_list[params] ')' {
                $$ = dispatch($expr, $method, $params);
        }
        | OBJECTID[method] '(' expression_comma_list[params] ')' {
                $$ = dispatch(object(idtable.add_string("self")), $method, $params);
        }
        | IF expression[condition] THEN expression[resolve] ELSE expression[reject] FI {
                $$ = cond($condition, $resolve, $reject);
        }
        | WHILE expression[condition] LOOP expression[expr] POOL {
                $$ = loop($condition, $expr);
        }
        | '{' expression_semi_nonempty_list[exprs] '}' {
                $$ = block($exprs);
        }
        | LET let_body[body] {
                $$ = $body;
        }
        | CASE expression[var] OF case_body[body] ESAC {
                $$ = typcase($var, $body);
        }
        | NEW TYPEID[name] {
                $$ = new_($name);
        }
        | ISVOID expression[expr] {
                $$ = isvoid($expr);
        }
        | expression[left] '+' expression[right] {
                $$ = plus($left, $right);
        }
        | expression[left] '-' expression[right] {
                $$ = sub($left, $right);
        }
        | expression[left] '*' expression[right] {
                $$ = mul($left, $right);
        }
        | expression[left] '/' expression[right] {
                $$ = divide($left, $right);
        }
        | '~' expression[expr] {
                $$ = neg($expr);
        }
        | expression[left] '<' expression[right] {
                $$ = lt($left, $right);
        }
        | expression[left] LE expression[right] {
                $$ = leq($left, $right);
        }
        | expression[left] '=' expression[right] {
                $$ = eq($left, $right);
        }
        | NOT expression[expr] {
                $$ = comp($expr);
        }
        | '(' expression[expr] ')' {
                $$ = $expr;
        }
        | OBJECTID[name] {
                $$ = object($name);
        }
        | INT_CONST[int] {
                $$ = int_const($int);
        }
        | STR_CONST[str] {
                $$ = string_const($str);
        }
        | BOOL_CONST[bool] {
                $$ = bool_const($bool);
        }
        ;

let_body
        : OBJECTID[name] ':' TYPEID[type] IN expression[expr] {
                $$ = let($name, $type, no_expr(), $expr);
        }
        | OBJECTID[name] ':' TYPEID[type] ASSIGN expression[value] IN expression[expr] {
                $$ = let($name, $type, $value, $expr);
        }
        | OBJECTID[name] ':' TYPEID[type] ',' let_body[expr] {
                $$ = let($name, $type, no_expr(), $expr);
        }
        | OBJECTID[name] ':' TYPEID[type] ASSIGN expression[value] ',' let_body[expr] {
                $$ = let($name, $type, $value, $expr);
        }
        | error ',' let_body {
                ERROR_HANDLER();
        }
        ;

case
        : OBJECTID[condition] ':' TYPEID[type] DARROW expression[expr] ';' {
                $$ = branch($condition, $type, $expr);
        }

case_body
        : case[default] {
                $$ = single_Cases($default);
        }
        | case_body[cases] case[default] {
                $$ = append_Cases($cases, single_Cases($default));
        }
        ;

/* end of grammar */
%%

/* This function is called automatically when Bison detects a parse error. */
void yyerror(const char *s)
{
  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;

  if(omerrs>20) {
      if (VERBOSE_ERRORS)
         fprintf(stderr, "More than 20 errors\n");
      exit(1);
  }
}

