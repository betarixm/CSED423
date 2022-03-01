/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

%}

%option noyywrap

/*
 * Define names for regular expressions here.
 */

/* def: Synonym (Begin) */

CR (\r)
LF (\n)

TRUE  (t(?i:rue))
FALSE	(f(?i:alse))

WHITESPACE  (" "|"\f"|"\r"|"\t"|"\v")
SYMBOLS		  ("("|")"|"*"|"+"|","|"-"|"."|"/"|":"|";"|"<"|"="|"@"|"{"|"}"|"~")

DIGIT ([0-9])
UPPER ([A-Z])
LOWER ([a-z])

LETTER  ({UPPER}|{LOWER})
ID      ({LETTER}|{DIGIT}|_)
NEWLINE ({CR}{LF}|{CR}|{LF})

/* def: Synonym   (End) */


/* def: Token (Begin) */

CLASS	  	  (?i:class)
ELSE	  	  (?i:else)
FI		      (?i:fi)
IF		      (?i:if)
IN		      (?i:in)
INHERITS    (?i:inherits)
LET		      (?i:let)
LOOP	  	  (?i:loop)
POOL	  	  (?i:pool)
THEN	  	  (?i:then)
WHILE	  	  (?i:while)
CASE	  	  (?i:case)
ESAC	  	  (?i:esac)
OF	    	  (?i:of)
DARROW      ("=>")
NEW	    	  (?i:new)
ISVOID		  (?i:isvoid)
INT_CONST   ({DIGIT}+)
BOOL_CONST  ({TRUE}|{FALSE})
TYPEID      ({UPPER}{ID}*)
OBJECTID    ({LOWER}{ID}*)
ASSIGN		  ("<-")
NOT	    	  (?i:not)
LE		      ("<=")

/* def: Token   (End) */

%%

 /*
  * Define regular expressions for the tokens of COOL here. Make sure, you
  * handle correctly special cases, like:
  *   - Nested comments
  *   - String constants: They use C like systax and can contain escape
  *     sequences. Escape sequence \c is accepted for all characters c. Except
  *     for \n \t \b \f, the result is c.
  *   - Keywords: They are case-insensitive except for the values true and
  *     false, which must begin with a lower-case letter.
  *   - Multiple-character operators (like <-): The scanner should produce a
  *     single token for every such operator.
  *   - Line counting: You should keep the global variable curr_lineno updated
  *     with the correct line number
  */

%%
