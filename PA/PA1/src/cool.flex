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


/* def: Constant (Begin) */

#define TRUE 1
#define FALSE 0

#define RED 31
#define GREEN 32
#define YELLOW 33
#define BLUE 34
#define MAGENTA 35
#define CYAN 36
#define WHITE 37
#define RESET 0

/* def: Constant   (End) */


/* def: Helper (Begin) */

#define SET_ERROR_MSG(X) (cool_yylval.error_msg = X)

#define SET_SYMBOL(X) (cool_yylval.symbol = X)

#define ADD_STRING_1(TABLE, STR) (TABLE.add_string(STR))

#define ADD_STRING_2(TABLE, STR, LEN) (TABLE.add_string(STR, LEN))

#define GET_3D_MACRO(TABLE, STR, LEN, NAME, ...) NAME

#define ADD_STRING__GET_ELEM(...) GET_3D_MACRO(__VA_ARGS__, ADD_STRING_2, ADD_STRING_1)(__VA_ARGS__)

/* def: Helper   (End) */


/* def: String (Begin) */

#define APPEND_STRING_BUF_2(_1, _2) {\
  if (append_string_buf(_1, _2) == -1) {\
    string_buf_overflow_flag = TRUE;\
  }\
}

#define APPEND_STRING_BUF_1(_1) {\
  if (append_string_buf(_1) == -1) {\
    string_buf_overflow_flag = TRUE;\
  }\
}

#define GET_MACRO(_1, _2, NAME, ...) NAME

#define APPEND_STRING_BUF(...) GET_MACRO(__VA_ARGS__, APPEND_STRING_BUF_2, APPEND_STRING_BUF_1)(__VA_ARGS__)

/* def: String   (End) */


/* def: Comment (Begin) */

int comment_depth = 0;

/* def: Comment   (End) */


/* def: String (Begin) */

char string_buf_overflow_flag = FALSE;

void init_string_buf();

int append_string_buf(std::string str);

int append_string_buf(std::string str, int length);

/* def: String   (End) */


/* def: Debug Purpose (Begin) */

// #define VERBOSE

void verbose();

void print(std::string str);

std::string coloring(std::string str, int color);

/* def: Debug Purpose   (End) */

%}

%option noyywrap


/* def: Block (Begin) */

%x COMMENT
%x STRING

/* def: Block   (End) */


/*
 * Define names for regular expressions here.
 */

/* def: Synonym (Begin) */

CR (\r)
LF (\n)

TRUE  (t(?i:rue))
FALSE (f(?i:alse))

WHITESPACE  (" "|"\f"|"\r"|"\t"|"\v")
OPERATORS     ("("|")"|"*"|"+"|","|"-"|"."|"/"|":"|";"|"<"|"="|"@"|"{"|"}"|"~")

DIGIT ([0-9])
UPPER ([A-Z])
LOWER ([a-z])

LETTER  ({UPPER}|{LOWER})
ID      ({LETTER}|{DIGIT}|_)
NEWLINE ({CR}{LF}|{CR}|{LF})

/* def: Synonym   (End) */


/* def: Token (Begin) */

CLASS       (?i:class)
ELSE        (?i:else)
FI          (?i:fi)
IF          (?i:if)
IN          (?i:in)
INHERITS    (?i:inherits)
LET         (?i:let)
LOOP        (?i:loop)
POOL        (?i:pool)
THEN        (?i:then)
WHILE       (?i:while)
CASE        (?i:case)
ESAC        (?i:esac)
OF          (?i:of)
DARROW      ("=>")
NEW         (?i:new)
ISVOID      (?i:isvoid)
INT_CONST   ({DIGIT}+)
BOOL_CONST  ({TRUE}|{FALSE})
TYPEID      ({UPPER}{ID}*)
OBJECTID    ({LOWER}{ID}*)
ASSIGN      ("<-")
NOT         (?i:not)
LE          ("<=")

/* def: Token   (End) */


/* def: Delimiter (Begin) */

COMMENT_BEGIN ("(*")
COMMENT_END   ("*)")

LINE_COMMENT_BEGIN  ("--")

STR_DELIM     ("\"")

/* def: Delimiter   (End) */

SIGMA (.)

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


 /* def: Nested Comments (Begin) */

{COMMENT_BEGIN} {
  /*
   * Nested Comments::Basic Rule
   * Begin Comment
   */
  comment_depth++;
  BEGIN(COMMENT);
}

<COMMENT>{COMMENT_BEGIN} {
  /*
   * Nested Comments::Basic Rule
   * Begin Comment
   */
  comment_depth++;
}

{COMMENT_END} { 
  /*
   * Nested Comments::Basic Rule
   * End Comment
   */
  SET_ERROR_MSG("Unmatched *)");
  return (ERROR);
}

<COMMENT>{COMMENT_END} { 
  /*
   * Nested Comments::Basic Rule
   * End Comment
   */
  comment_depth--;

  if(comment_depth == 0) {
    BEGIN(INITIAL);
  }
}

<COMMENT><<EOF>> {
  /*
   * Nested Comments::Exception
   * EOF
   */
  
  BEGIN(INITIAL); // Don't end lexer with comment block!
  SET_ERROR_MSG("EOF in comment");
  return (ERROR);
}

<COMMENT>{NEWLINE} {
  /*
   * Nested Comments::Exception
   * Line Feed
   */
  curr_lineno++;
}

<COMMENT>{SIGMA} {
  /*
   * Nested Comments::Pumping
   * Pump for any char
   */

  // Pumping rule for comment block.

}

{LINE_COMMENT_BEGIN}{SIGMA}* {
  /*
   * Nested Comments::Exception
   * Line Comment
   */
  
  // Catching rule for line comment.
}

 /* def: Nested Comments   (End) */


 /* def: String constants (Begin) */

{STR_DELIM} { 
  BEGIN(STRING);
  init_string_buf();
}

<STRING>{STR_DELIM}		{
  BEGIN(INITIAL);

  if(string_buf_overflow_flag){
    SET_ERROR_MSG("String constant too long");

    return (ERROR);
  }

  SET_SYMBOL(ADD_STRING__GET_ELEM(stringtable, string_buf));
  return (STR_CONST);
}

<STRING><<EOF>> {
  BEGIN(INITIAL);
  SET_ERROR_MSG("EOF in string constant");

  return (ERROR);
}

<STRING>\0 {
  BEGIN(INITIAL);
  SET_ERROR_MSG("String contains invalid character");

  return (ERROR);
}

<STRING>{NEWLINE}	{
  BEGIN(INITIAL);
  SET_ERROR_MSG("Unterminated string constant");

  return (ERROR);
}

<STRING>\\[^\0\r]	{
  char matched = yytext[1];
  char text = matched;

  if (matched == 'b') {
    text = '\b';
  } else if (matched == 't') {
    text = '\t';
  } else if (matched == 'n') {
    text = '\n';
  } else if (matched == 'f') {
    text = '\f';
  }

  APPEND_STRING_BUF(&text, 1);
}

<STRING>. {
  APPEND_STRING_BUF(yytext);
}

 /* def: String constants   (End) */


 /* def: Keywords (Begin) */

{CLASS} {
  return (CLASS);
}

{ELSE} {
  return (ELSE);
}

{FI} {
  return (FI);
}

{IF} {
  return (IF);
}

{IN} {
  return (IN);
}

{INHERITS} {
  return (INHERITS);
}

{LET} {
  return (LET);
}

{LOOP} {
  return (LOOP);
}

{POOL} {
  return (POOL);
}

{THEN} {
  return (THEN);
}

{WHILE} {
  return (WHILE);
}

{CASE} {
  return (CASE);
}

{ESAC} {
  return (ESAC);
}

{OF} {
  return (OF);
}

{NEW} {
  return (NEW);
}

{ISVOID} {
  return (ISVOID);
}

{TRUE} { 
  cool_yylval.boolean = true;
  return (BOOL_CONST);
}

{FALSE} {
  cool_yylval.boolean = false;
  return (BOOL_CONST);
}

{NOT} {
  return (NOT);
}

 /* def: Keywords   (End) */


 /* def: Control Characters (Begin) */

{NEWLINE} {
  ++curr_lineno;
}

{WHITESPACE} {
   
}

 /* def: Control Characters   (End) */


 /* def: Operators (Begin) */

{OPERATORS} {
  return (yytext[0]);
}

 /* def: Operators   (End) */


 /* def: Multiple-character Operators (Begin) */

{DARROW} {
  return (DARROW);
}

{ASSIGN} {
  return (ASSIGN);
}

{LE} {
  return (LE);
}

 /* def: Multiple-character Operators   (End) */


 /* def: Symbols (Begin) */

{INT_CONST} {
  SET_SYMBOL(ADD_STRING__GET_ELEM(inttable, yytext, yyleng));
  return (INT_CONST);
}

{TYPEID} {
  SET_SYMBOL(ADD_STRING__GET_ELEM(idtable, yytext, yyleng));
  return (TYPEID);
}

{OBJECTID} {
  SET_SYMBOL(ADD_STRING__GET_ELEM(idtable, yytext, yyleng));
  return (OBJECTID);
}

 /* def: Symbols   (End) */


 /* def: Exception (Begin) */

{SIGMA} {
  SET_ERROR_MSG(yytext);
  return (ERROR);    
}

 /* def: Exception   (End) */


%%


/* def: String (Begin) */

void init_string_buf() {
  string_buf[0] = '\0';
  string_buf_ptr = string_buf;
  string_buf_overflow_flag = FALSE;
}

int append_string_buf(std::string str) {
  return append_string_buf(str, str.length());
}

int append_string_buf(std::string str, int length) {
  int result_length = strlen(string_buf) + length;

  if (result_length >= MAX_STR_CONST) {
    return -1;  
  }

  strncat(string_buf, str.c_str(), length);

  print("Current string_buf       : " + std::string(string_buf));
  print("Current string_buf length: " + std::to_string(result_length));

  return result_length;
}

/* def: String   (End) */


/* def: Debug Purpose (Begin) */

void verbose() {
#ifdef VERBOSE
  std::cout << coloring("[*] DEBUG", RED) << std::endl;
  std::cout << coloring("    - Input (yytext):  ", MAGENTA) << yytext << std::endl;
  std::cout << coloring("    - Length (yyleng): ", MAGENTA) << yyleng << std::endl;
  std::cout << coloring("    - Comment Depth:   ", MAGENTA) << comment_depth << std::endl;
#endif
}

void print(std::string str) {
#ifdef VERBOSE
  std::cout << coloring("[*] OUT: ", YELLOW) << str << std::endl;
#endif
}

std::string coloring(std::string str, int color) {
  return "\x1b[" + std::to_string(color) + "m" + str + "\x1b[" + std::to_string(RESET) + "m";
}

/* def: Debug Purpose   (End) */
