/* A Bison parser, made from ast.y
   by GNU bison 1.35.  */

#define YYBISON 1  /* Identify Bison output.  */

#define yyparse ast_yyparse
#define yylex ast_yylex
#define yyerror ast_yyerror
#define yylval ast_yylval
#define yychar ast_yychar
#define yydebug ast_yydebug
#define yynerrs ast_yynerrs
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

#line 6 "ast.y"

#include "cool-io.h"
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

void ast_yyerror(char *);
extern int curr_lineno;
extern int yylex();           /* the entry point to the lexer  */
Program ast_root;             /* the result of the parse  */
Classes parse_results;        /* for use in parsing multiple files */
int omerrs = 0;               /* number of errors in lexing and parsing */

#line 21 "ast.y"
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
#ifndef YYDEBUG
# define YYDEBUG 1
#endif



#define	YYFINAL		121
#define	YYFLAG		-32768
#define	YYNTBASE	41

/* YYTRANSLATE(YYLEX) -- Bison token number corresponding to YYLEX. */
#define YYTRANSLATE(x) ((unsigned)(x) <= 291 ? yytranslate[x] : 56)

/* YYTRANSLATE[YYLEX] -- Bison token number corresponding to YYLEX. */
static const char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      38,    39,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    40,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37
};

#if YYDEBUG
static const short yyprhs[] =
{
       0,     0,     4,     5,     7,    10,    19,    20,    22,    24,
      27,    34,    40,    41,    43,    45,    48,    53,    57,    61,
      66,    73,    79,    85,    90,    94,   101,   106,   110,   114,
     119,   124,   129,   134,   138,   143,   148,   153,   157,   161,
     165,   169,   173,   176,   179,   183,   185,   188,   190,   193
};
static const short yyrhs[] =
{
      37,     3,    42,     0,     0,    43,     0,    42,    43,     0,
      37,     4,    36,    36,    34,    38,    44,    39,     0,     0,
      45,     0,    46,     0,    45,    46,     0,    37,     5,    36,
      47,    36,    50,     0,    37,     6,    36,    36,    50,     0,
       0,    48,     0,    49,     0,    48,    49,     0,    37,     7,
      36,    36,     0,    51,    40,    36,     0,    51,    40,    33,
       0,    37,     9,    36,    50,     0,    37,    10,    50,    36,
      36,    52,     0,    37,    11,    50,    36,    52,     0,    37,
      12,    50,    50,    50,     0,    37,    13,    50,    50,     0,
      37,    15,    53,     0,    37,    16,    36,    36,    50,    50,
       0,    37,    14,    50,    54,     0,    37,    29,    36,     0,
      37,    30,    50,     0,    37,    17,    50,    50,     0,    37,
      18,    50,    50,     0,    37,    19,    50,    50,     0,    37,
      20,    50,    50,     0,    37,    21,    50,     0,    37,    22,
      50,    50,     0,    37,    23,    50,    50,     0,    37,    24,
      50,    50,     0,    37,    25,    50,     0,    37,    26,    35,
       0,    37,    27,    34,     0,    37,    28,    35,     0,    37,
      32,    36,     0,    37,    31,     0,    38,    39,     0,    38,
      53,    39,     0,    50,     0,    53,    50,     0,    55,     0,
      54,    55,     0,    37,     8,    36,    36,    50,     0
};

#endif

#if YYDEBUG
/* YYRLINE[YYN] -- source line where rule number YYN was defined. */
static const short yyrline[] =
{
       0,    68,    70,    75,    78,    83,    89,    91,    96,    98,
     102,   104,   108,   110,   115,   117,   121,   125,   129,   132,
     135,   138,   141,   144,   147,   150,   153,   156,   159,   162,
     165,   168,   171,   174,   177,   180,   183,   186,   189,   192,
     195,   203,   206,   210,   212,   217,   219,   224,   226,   231
};
#endif


#if (YYDEBUG) || defined YYERROR_VERBOSE

/* YYTNAME[TOKEN_NUM] -- String name of the token TOKEN_NUM. */
static const char *const yytname[] =
{
  "$", "error", "$undefined.", "PROGRAM", "CLASS", "METHOD", "ATTR", 
  "FORMAL", "BRANCH", "ASSIGN", "STATIC_DISPATCH", "DISPATCH", "COND", 
  "LOOP", "TYPCASE", "BLOCK", "LET", "PLUS", "SUB", "MUL", "DIVIDE", 
  "NEG", "LT", "EQ", "LEQ", "COMP", "INT", "STR", "BOOL", "NEW", "ISVOID", 
  "NO_EXPR", "OBJECT", "NO_TYPE", "STR_CONST", "INT_CONST", "ID", 
  "LINENO", "'('", "')'", "':'", "program", "class_list", "class", 
  "optional_feature_list", "feature_list", "feature", "formals", 
  "formal_list", "formal", "expr", "expr_aux", "actuals", "expr_list", 
  "case_list", "simple_case", 0
};
#endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives. */
static const short yyr1[] =
{
       0,    41,    41,    42,    42,    43,    44,    44,    45,    45,
      46,    46,    47,    47,    48,    48,    49,    50,    50,    51,
      51,    51,    51,    51,    51,    51,    51,    51,    51,    51,
      51,    51,    51,    51,    51,    51,    51,    51,    51,    51,
      51,    51,    51,    52,    52,    53,    53,    54,    54,    55
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN. */
static const short yyr2[] =
{
       0,     3,     0,     1,     2,     8,     0,     1,     1,     2,
       6,     5,     0,     1,     1,     2,     4,     3,     3,     4,
       6,     5,     5,     4,     3,     6,     4,     3,     3,     4,
       4,     4,     4,     3,     4,     4,     4,     3,     3,     3,
       3,     3,     2,     2,     3,     1,     2,     1,     2,     5
};

/* YYDEFACT[S] -- default rule to reduce with in state S when YYTABLE
   doesn't specify something else to do.  Zero means the default is an
   error. */
static const short yydefact[] =
{
       2,     0,     0,     0,     1,     3,     0,     4,     0,     0,
       0,     6,     0,     0,     7,     8,     0,     0,     5,     9,
      12,     0,     0,     0,    13,    14,     0,     0,     0,    15,
       0,    11,     0,     0,    10,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    42,     0,     0,
      16,     0,     0,     0,     0,     0,     0,    45,    24,     0,
       0,     0,     0,     0,    33,     0,     0,     0,    37,    38,
      39,    40,    27,    28,    41,    18,    17,    19,     0,     0,
       0,    23,     0,    26,    47,    46,     0,    29,    30,    31,
      32,    34,    35,    36,     0,     0,    21,    22,     0,    48,
       0,    20,    43,     0,     0,    25,    44,     0,    49,     0,
       0,     0
};

static const short yydefgoto[] =
{
     119,     4,     5,    13,    14,    15,    23,    24,    25,    67,
      32,   106,    68,    93,    94
};

static const short yypact[] =
{
     -30,     6,   -11,    23,   -11,-32768,    -8,-32768,    -7,    -3,
      -6,    -4,    10,    -5,    -4,-32768,     0,     1,-32768,-32768,
       3,     5,    36,    12,     3,-32768,    15,    17,    15,-32768,
      83,-32768,    14,    19,-32768,    20,    15,    15,    15,    15,
      15,    15,    21,    15,    15,    15,    15,    15,    15,    15,
      15,    15,    24,    26,    27,    22,    15,-32768,    25,   -32,
  -32768,    15,    29,    30,    15,    15,    31,-32768,    15,    33,
      15,    15,    15,    15,-32768,    15,    15,    15,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,    35,    34,
      15,-32768,    55,    31,-32768,-32768,    15,-32768,-32768,-32768,
  -32768,-32768,-32768,-32768,    34,   -34,-32768,-32768,    37,-32768,
      15,-32768,-32768,   -31,    38,-32768,-32768,    15,-32768,    67,
      75,-32768
};

static const short yypgoto[] =
{
  -32768,-32768,    72,-32768,-32768,    63,-32768,-32768,    54,   -26,
  -32768,   -25,   -24,-32768,   -13
};


#define	YYLAST		115


static const short yytable[] =
{
      31,    85,    34,    30,    86,   112,    30,     1,   116,     2,
      62,    63,    64,    65,    66,    16,    17,    70,    71,    72,
      73,    74,    75,    76,    77,    78,     3,     6,     8,     9,
      83,    10,    11,    12,    18,    87,    20,    21,    90,    91,
      22,    26,    95,    27,    97,    98,    99,   100,    28,   101,
     102,   103,    30,    33,    59,    60,    61,    69,    82,    79,
      80,    84,    81,   108,   107,    88,    89,   120,    92,    96,
     110,   104,   105,   114,   117,   121,     7,    19,    29,   111,
     109,   113,     0,     0,   115,     0,     0,    95,     0,     0,
       0,   118,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58
};

static const short yycheck[] =
{
      26,    33,    28,    37,    36,    39,    37,    37,    39,     3,
      36,    37,    38,    39,    40,     5,     6,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    37,     4,    36,    36,
      56,    34,    38,    37,    39,    61,    36,    36,    64,    65,
      37,    36,    68,     7,    70,    71,    72,    73,    36,    75,
      76,    77,    37,    36,    40,    36,    36,    36,    36,    35,
      34,    36,    35,     8,    90,    36,    36,     0,    37,    36,
      96,    36,    38,    36,    36,     0,     4,    14,    24,   104,
      93,   105,    -1,    -1,   110,    -1,    -1,   113,    -1,    -1,
      -1,   117,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison/bison.simple"

/* Skeleton output parser for bison,

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software
   Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser when
   the %semantic_parser declaration is not specified in the grammar.
   It was written by Richard Stallman by simplifying the hairy parser
   used when %semantic_parser is specified.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

#if ! defined (yyoverflow) || defined (YYERROR_VERBOSE)

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || defined (YYERROR_VERBOSE) */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYLTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
# if YYLSP_NEEDED
  YYLTYPE yyls;
# endif
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAX (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# if YYLSP_NEEDED
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE) + sizeof (YYLTYPE))	\
      + 2 * YYSTACK_GAP_MAX)
# else
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAX)
# endif

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAX;	\
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif


#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");			\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).

   When YYLLOC_DEFAULT is run, CURRENT is set the location of the
   first token.  By default, to implement support for ranges, extend
   its range to the last symbol.  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)       	\
   Current.last_line   = Rhs[N].last_line;	\
   Current.last_column = Rhs[N].last_column;
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#if YYPURE
# if YYLSP_NEEDED
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, &yylloc, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval, &yylloc)
#  endif
# else /* !YYLSP_NEEDED */
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval)
#  endif
# endif /* !YYLSP_NEEDED */
#else /* !YYPURE */
# define YYLEX			yylex ()
#endif /* !YYPURE */


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)
/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
#endif /* !YYDEBUG */

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif

#ifdef YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif
#endif

#line 315 "/usr/share/bison/bison.simple"


/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
#  define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL
# else
#  define YYPARSE_PARAM_ARG YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
# endif
#else /* !YYPARSE_PARAM */
# define YYPARSE_PARAM_ARG
# define YYPARSE_PARAM_DECL
#endif /* !YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
# ifdef YYPARSE_PARAM
int yyparse (void *);
# else
int yyparse (void);
# endif
#endif

/* YY_DECL_VARIABLES -- depending whether we use a pure parser,
   variables are global, or local to YYPARSE.  */

#define YY_DECL_NON_LSP_VARIABLES			\
/* The lookahead symbol.  */				\
int yychar;						\
							\
/* The semantic value of the lookahead symbol. */	\
YYSTYPE yylval;						\
							\
/* Number of parse errors so far.  */			\
int yynerrs;

#if YYLSP_NEEDED
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES			\
						\
/* Location data for the lookahead symbol.  */	\
YYLTYPE yylloc;
#else
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES
#endif


/* If nonreentrant, generate the variables here. */

#if !YYPURE
YY_DECL_VARIABLES
#endif  /* !YYPURE */

int
yyparse (YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  /* If reentrant, generate the variables here. */
#if YYPURE
  YY_DECL_VARIABLES
#endif  /* !YYPURE */

  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yychar1 = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack. */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;

#if YYLSP_NEEDED
  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
#endif

#if YYLSP_NEEDED
# define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
# define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  YYSIZE_T yystacksize = YYINITDEPTH;


  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
#if YYLSP_NEEDED
  YYLTYPE yyloc;
#endif

  /* When reducing, the number of symbols on the RHS of the reduced
     rule. */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
#if YYLSP_NEEDED
  yylsp = yyls;
#endif
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  */
# if YYLSP_NEEDED
	YYLTYPE *yyls1 = yyls;
	/* This used to be a conditional around just the two extra args,
	   but that might be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
# else
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);
# endif
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);
# if YYLSP_NEEDED
	YYSTACK_RELOCATE (yyls);
# endif
# undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
#if YYLSP_NEEDED
      yylsp = yyls + yysize - 1;
#endif

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yychar1 = YYTRANSLATE (yychar);

#if YYDEBUG
     /* We have to keep this `#if YYDEBUG', since we use variables
	which are defined only if `YYDEBUG' is set.  */
      if (yydebug)
	{
	  YYFPRINTF (stderr, "Next token is %d (%s",
		     yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise
	     meaning of a token, for further debugging info.  */
# ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
# endif
	  YYFPRINTF (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %d (%s), ",
	      yychar, yytname[yychar1]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to the semantic value of
     the lookahead token.  This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

#if YYLSP_NEEDED
  /* Similarly for the default location.  Let the user run additional
     commands if for instance locations are ranges.  */
  yyloc = yylsp[1-yylen];
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
#endif

#if YYDEBUG
  /* We have to keep this `#if YYDEBUG', since we use variables which
     are defined only if `YYDEBUG' is set.  */
  if (yydebug)
    {
      int yyi;

      YYFPRINTF (stderr, "Reducing via rule %d (line %d), ",
		 yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (yyi = yyprhs[yyn]; yyrhs[yyi] > 0; yyi++)
	YYFPRINTF (stderr, "%s ", yytname[yyrhs[yyi]]);
      YYFPRINTF (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif

  switch (yyn) {

case 1:
#line 69 "ast.y"
{ curr_lineno = yyvsp[-2].lineno; ast_root = program(yyvsp[0].classes); }
    break;
case 2:
#line 71 "ast.y"
{ exit(1); }
    break;
case 3:
#line 76 "ast.y"
{ yyval.classes = single_Classes(yyvsp[0].class_);
                  parse_results = yyval.classes; }
    break;
case 4:
#line 79 "ast.y"
{ yyval.classes = append_Classes(yyvsp[-1].classes,single_Classes(yyvsp[0].class_)); 
                  parse_results = yyval.classes; }
    break;
case 5:
#line 84 "ast.y"
{ curr_lineno = yyvsp[-7].lineno;
		  yyval.class_ = class_(yyvsp[-5].symbol,yyvsp[-4].symbol,yyvsp[-1].features,yyvsp[-3].symbol); }
    break;
case 6:
#line 90 "ast.y"
{  yyval.features = nil_Features(); }
    break;
case 7:
#line 92 "ast.y"
{  yyval.features = yyvsp[0].features; }
    break;
case 8:
#line 97 "ast.y"
{  yyval.features = single_Features(yyvsp[0].feature); }
    break;
case 9:
#line 99 "ast.y"
{  yyval.features = append_Features(yyvsp[-1].features,single_Features(yyvsp[0].feature)); }
    break;
case 10:
#line 103 "ast.y"
{ curr_lineno = yyvsp[-5].lineno; yyval.feature = method(yyvsp[-3].symbol,yyvsp[-2].formals,yyvsp[-1].symbol,yyvsp[0].expression); }
    break;
case 11:
#line 105 "ast.y"
{ curr_lineno = yyvsp[-4].lineno; yyval.feature = attr(yyvsp[-2].symbol,yyvsp[-1].symbol,yyvsp[0].expression); }
    break;
case 12:
#line 109 "ast.y"
{ yyval.formals = nil_Formals(); }
    break;
case 13:
#line 111 "ast.y"
{ yyval.formals = yyvsp[0].formals; }
    break;
case 14:
#line 116 "ast.y"
{  yyval.formals = single_Formals(yyvsp[0].formal); }
    break;
case 15:
#line 118 "ast.y"
{ yyval.formals = append_Formals(yyvsp[-1].formals,single_Formals(yyvsp[0].formal)); }
    break;
case 16:
#line 122 "ast.y"
{  curr_lineno = yyvsp[-3].lineno; yyval.formal = formal(yyvsp[-1].symbol,yyvsp[0].symbol); }
    break;
case 17:
#line 126 "ast.y"
{ yyval.expression = yyvsp[-2].expression;
            yyval.expression->set_type(yyvsp[0].symbol); }
    break;
case 18:
#line 130 "ast.y"
{ yyval.expression = yyvsp[-2].expression; }
    break;
case 19:
#line 133 "ast.y"
{ curr_lineno = yyvsp[-3].lineno; yyval.expression = assign(yyvsp[-1].symbol,yyvsp[0].expression); }
    break;
case 20:
#line 136 "ast.y"
{ curr_lineno = yyvsp[-5].lineno; yyval.expression = static_dispatch(yyvsp[-3].expression,yyvsp[-2].symbol,yyvsp[-1].symbol,yyvsp[0].expressions);}
    break;
case 21:
#line 139 "ast.y"
{ curr_lineno = yyvsp[-4].lineno; yyval.expression = dispatch(yyvsp[-2].expression,yyvsp[-1].symbol,yyvsp[0].expressions); }
    break;
case 22:
#line 142 "ast.y"
{ curr_lineno = yyvsp[-4].lineno; yyval.expression = cond(yyvsp[-2].expression,yyvsp[-1].expression,yyvsp[0].expression); }
    break;
case 23:
#line 145 "ast.y"
{ curr_lineno = yyvsp[-3].lineno; yyval.expression = loop(yyvsp[-1].expression,yyvsp[0].expression); }
    break;
case 24:
#line 148 "ast.y"
{ curr_lineno = yyvsp[-2].lineno; yyval.expression = block(yyvsp[0].expressions); }
    break;
case 25:
#line 151 "ast.y"
{ curr_lineno = yyvsp[-5].lineno; yyval.expression = let(yyvsp[-3].symbol,yyvsp[-2].symbol,yyvsp[-1].expression,yyvsp[0].expression); }
    break;
case 26:
#line 154 "ast.y"
{ curr_lineno = yyvsp[-3].lineno; yyval.expression = typcase(yyvsp[-1].expression,yyvsp[0].cases); }
    break;
case 27:
#line 157 "ast.y"
{ curr_lineno = yyvsp[-2].lineno; yyval.expression = new_(yyvsp[0].symbol); }
    break;
case 28:
#line 160 "ast.y"
{ curr_lineno = yyvsp[-2].lineno; yyval.expression = isvoid(yyvsp[0].expression); }
    break;
case 29:
#line 163 "ast.y"
{ curr_lineno = yyvsp[-3].lineno; yyval.expression = plus(yyvsp[-1].expression,yyvsp[0].expression); }
    break;
case 30:
#line 166 "ast.y"
{ curr_lineno = yyvsp[-3].lineno; yyval.expression = sub(yyvsp[-1].expression,yyvsp[0].expression); }
    break;
case 31:
#line 169 "ast.y"
{ curr_lineno = yyvsp[-3].lineno; yyval.expression = mul(yyvsp[-1].expression,yyvsp[0].expression); }
    break;
case 32:
#line 172 "ast.y"
{ curr_lineno = yyvsp[-3].lineno; yyval.expression = divide(yyvsp[-1].expression,yyvsp[0].expression); }
    break;
case 33:
#line 175 "ast.y"
{ curr_lineno = yyvsp[-2].lineno; yyval.expression = neg(yyvsp[0].expression); }
    break;
case 34:
#line 178 "ast.y"
{ curr_lineno = yyvsp[-3].lineno; yyval.expression = lt(yyvsp[-1].expression,yyvsp[0].expression); }
    break;
case 35:
#line 181 "ast.y"
{ curr_lineno = yyvsp[-3].lineno; yyval.expression = eq(yyvsp[-1].expression,yyvsp[0].expression); }
    break;
case 36:
#line 184 "ast.y"
{ curr_lineno = yyvsp[-3].lineno; yyval.expression = leq(yyvsp[-1].expression,yyvsp[0].expression); }
    break;
case 37:
#line 187 "ast.y"
{ curr_lineno = yyvsp[-2].lineno; yyval.expression = comp(yyvsp[0].expression); }
    break;
case 38:
#line 190 "ast.y"
{ curr_lineno = yyvsp[-2].lineno; yyval.expression = int_const(yyvsp[0].symbol); }
    break;
case 39:
#line 193 "ast.y"
{ curr_lineno = yyvsp[-2].lineno; yyval.expression = string_const(yyvsp[0].symbol); }
    break;
case 40:
#line 196 "ast.y"
{ curr_lineno = yyvsp[-2].lineno; 
            if (*(yyvsp[0].symbol->get_string()) == '1')
	      yyval.expression = bool_const(1); 
	    else
              yyval.expression = bool_const(0);
          }
    break;
case 41:
#line 204 "ast.y"
{ curr_lineno = yyvsp[-2].lineno; yyval.expression = object(yyvsp[0].symbol); }
    break;
case 42:
#line 207 "ast.y"
{ curr_lineno = yyvsp[-1].lineno; yyval.expression = no_expr(); }
    break;
case 43:
#line 211 "ast.y"
{  yyval.expressions = nil_Expressions(); }
    break;
case 44:
#line 213 "ast.y"
{  yyval.expressions = yyvsp[-1].expressions; }
    break;
case 45:
#line 218 "ast.y"
{ yyval.expressions = single_Expressions(yyvsp[0].expression); }
    break;
case 46:
#line 220 "ast.y"
{ yyval.expressions = append_Expressions(yyvsp[-1].expressions,single_Expressions(yyvsp[0].expression)); }
    break;
case 47:
#line 225 "ast.y"
{ yyval.cases = single_Cases(yyvsp[0].case_); }
    break;
case 48:
#line 227 "ast.y"
{ yyval.cases = append_Cases(yyvsp[-1].cases,single_Cases(yyvsp[0].case_)); }
    break;
case 49:
#line 232 "ast.y"
{ curr_lineno = yyvsp[-4].lineno; yyval.case_ = branch(yyvsp[-2].symbol,yyvsp[-1].symbol,yyvsp[0].expression); }
    break;
}

#line 705 "/usr/share/bison/bison.simple"


  yyvsp -= yylen;
  yyssp -= yylen;
#if YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;
#if YYLSP_NEEDED
  *++yylsp = yyloc;
#endif

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("parse error, unexpected ") + 1;
	  yysize += yystrlen (yytname[YYTRANSLATE (yychar)]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "parse error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[YYTRANSLATE (yychar)]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exhausted");
	}
      else
#endif /* defined (YYERROR_VERBOSE) */
	yyerror ("parse error");
    }
  goto yyerrlab1;


/*--------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action |
`--------------------------------------------------*/
yyerrlab1:
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;
      YYDPRINTF ((stderr, "Discarding token %d (%s).\n",
		  yychar, yytname[yychar1]));
      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;


/*-------------------------------------------------------------------.
| yyerrdefault -- current state does not do anything special for the |
| error token.                                                       |
`-------------------------------------------------------------------*/
yyerrdefault:
#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */

  /* If its default is to accept any token, ok.  Otherwise pop it.  */
  yyn = yydefact[yystate];
  if (yyn)
    goto yydefault;
#endif


/*---------------------------------------------------------------.
| yyerrpop -- pop the current state because it cannot handle the |
| error token                                                    |
`---------------------------------------------------------------*/
yyerrpop:
  if (yyssp == yyss)
    YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#if YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "Error: state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

/*--------------.
| yyerrhandle.  |
`--------------*/
yyerrhandle:
  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

/*---------------------------------------------.
| yyoverflowab -- parser overflow comes here.  |
`---------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}
#line 237 "ast.y"


void ast_yyerror(char *)
{
   cerr << "??? unexpected error in ast parsing." << endl;
   exit(1);
}

