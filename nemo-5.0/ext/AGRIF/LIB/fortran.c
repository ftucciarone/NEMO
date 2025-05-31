/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         fortran_parse
#define yylex           fortran_lex
#define yyerror         fortran_error
#define yydebug         fortran_debug
#define yynerrs         fortran_nerrs

#define yylval          fortran_lval
#define yychar          fortran_char

/* Copy the first part of user declarations.  */
#line 36 "fortran.y" /* yacc.c:339  */

#define YYMAXDEPTH 1000
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"

extern int line_num_input;

char c_selectorname[LONG_M];
char ligne[LONG_M];
char truename[LONG_VNAME];
char identcopy[LONG_VNAME];
int c_selectorgiven=0;
listvar *curlistvar;
int in_select_case_stmt=0;
typedim c_selectordim;
listcouple *coupletmp;
int removeline=0;
int token_since_endofstmt = 0;
int increment_nbtokens = 1;
int in_complex_literal = 0;
int close_or_connect = 0;
int in_io_control_spec = 0;
int intent_spec = 0;
long int my_position;
long int my_position_before;
int suborfun = 0;
int indeclaration = 0;
int endoffile = 0;
int in_inquire = 0;
int in_char_selector = 0;
int in_kind_selector =0;
int char_length_toreset = 0;

typedim my_dim;

listvar *test;

char linebuf1[1024];
char linebuf2[1024];

int fortran_error(const char *s)
{
  if (endoffile == 1) 
  {
  endoffile = 0;
  return 0;
  }
    printf("%s line %d, file %s culprit = |%s|\n", s, line_num_input, cur_filename, strcat(linebuf1, linebuf2));
    exit(1);
}


#line 129 "fortran.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int fortran_debug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TOK_EQV = 258,
    TOK_NEQV = 259,
    TOK_OR = 260,
    TOK_XOR = 261,
    TOK_AND = 262,
    TOK_NOT = 263,
    TOK_LT = 264,
    TOK_GT = 265,
    TOK_LE = 266,
    TOK_GE = 267,
    TOK_EQ = 268,
    TOK_NE = 269,
    TOK_DSLASH = 270,
    TOK_SLASH = 271,
    TOK_DASTER = 272,
    TOK_SEMICOLON = 273,
    TOK_PARAMETER = 274,
    TOK_RESULT = 275,
    TOK_ONLY = 276,
    TOK_INCLUDE = 277,
    TOK_SUBROUTINE = 278,
    TOK_PROGRAM = 279,
    TOK_FUNCTION = 280,
    TOK_LABEL_FORMAT = 281,
    TOK_LABEL_CONTINUE = 282,
    TOK_LABEL_END_DO = 283,
    TOK_MAX = 284,
    TOK_TANH = 285,
    TOK_COMMENT = 286,
    TOK_WHERE = 287,
    TOK_ELSEWHEREPAR = 288,
    TOK_ELSEWHERE = 289,
    TOK_ENDWHERE = 290,
    TOK_MAXVAL = 291,
    TOK_TRIM = 292,
    TOK_NULL_PTR = 293,
    TOK_SUM = 294,
    TOK_SQRT = 295,
    TOK_CASE = 296,
    TOK_SELECTCASE = 297,
    TOK_FILE = 298,
    TOK_REC = 299,
    TOK_NAME_EQ = 300,
    TOK_IOLENGTH = 301,
    TOK_ACCESS = 302,
    TOK_ACTION = 303,
    TOK_FORM = 304,
    TOK_RECL = 305,
    TOK_STATUS = 306,
    TOK_UNIT = 307,
    TOK_NEWUNIT = 308,
    TOK_OPENED = 309,
    TOK_FMT = 310,
    TOK_NML = 311,
    TOK_END = 312,
    TOK_EOR = 313,
    TOK_EOF = 314,
    TOK_ERR = 315,
    TOK_POSITION = 316,
    TOK_IOSTAT = 317,
    TOK_IOMSG = 318,
    TOK_EXIST = 319,
    TOK_MIN = 320,
    TOK_FLOAT = 321,
    TOK_EXP = 322,
    TOK_LEN = 323,
    TOK_COS = 324,
    TOK_COSH = 325,
    TOK_ACOS = 326,
    TOK_NINT = 327,
    TOK_CYCLE = 328,
    TOK_SIN = 329,
    TOK_SINH = 330,
    TOK_ASIN = 331,
    TOK_EQUIVALENCE = 332,
    TOK_BACKSPACE = 333,
    TOK_LOG = 334,
    TOK_TAN = 335,
    TOK_ATAN = 336,
    TOK_RECURSIVE = 337,
    TOK_PURE = 338,
    TOK_IMPURE = 339,
    TOK_ELEMENTAL = 340,
    TOK_ABS = 341,
    TOK_MOD = 342,
    TOK_SIGN = 343,
    TOK_MINLOC = 344,
    TOK_MAXLOC = 345,
    TOK_EXIT = 346,
    TOK_KIND = 347,
    TOK_MOLD = 348,
    TOK_SOURCE = 349,
    TOK_ERRMSG = 350,
    TOK_MINVAL = 351,
    TOK_PUBLIC = 352,
    TOK_PRIVATE = 353,
    TOK_ALLOCATABLE = 354,
    TOK_CONTIGUOUS = 355,
    TOK_RETURN = 356,
    TOK_THEN = 357,
    TOK_ELSEIF = 358,
    TOK_ELSE = 359,
    TOK_ENDIF = 360,
    TOK_PRINT = 361,
    TOK_PLAINGOTO = 362,
    TOK_LOGICALIF = 363,
    TOK_LOGICALIF_PAR = 364,
    TOK_PLAINDO = 365,
    TOK_CONTAINS = 366,
    TOK_ENDDO = 367,
    TOK_MODULE = 368,
    TOK_ENDMODULE = 369,
    TOK_WHILE = 370,
    TOK_CONCURRENT = 371,
    TOK_ALLOCATE = 372,
    TOK_OPEN = 373,
    TOK_CLOSE = 374,
    TOK_INQUIRE = 375,
    TOK_WRITE_PAR = 376,
    TOK_WRITE = 377,
    TOK_FLUSH = 378,
    TOK_READ_PAR = 379,
    TOK_READ = 380,
    TOK_REWIND = 381,
    TOK_DEALLOCATE = 382,
    TOK_NULLIFY = 383,
    TOK_DIMENSION = 384,
    TOK_ENDSELECT = 385,
    TOK_EXTERNAL = 386,
    TOK_INTENT = 387,
    TOK_INTRINSIC = 388,
    TOK_NAMELIST = 389,
    TOK_DEFAULT = 390,
    TOK_OPTIONAL = 391,
    TOK_POINTER = 392,
    TOK_CONTINUE = 393,
    TOK_SAVE = 394,
    TOK_TARGET = 395,
    TOK_IMPLICIT = 396,
    TOK_NONE = 397,
    TOK_CALL = 398,
    TOK_STAT = 399,
    TOK_POINT_TO = 400,
    TOK_COMMON = 401,
    TOK_GLOBAL = 402,
    TOK_LEFTAB = 403,
    TOK_RIGHTAB = 404,
    TOK_PAUSE = 405,
    TOK_PROCEDURE = 406,
    TOK_STOP = 407,
    TOK_FOURDOTS = 408,
    TOK_HEXA = 409,
    TOK_ASSIGNTYPE = 410,
    TOK_OUT = 411,
    TOK_INOUT = 412,
    TOK_IN = 413,
    TOK_USE = 414,
    TOK_EQUALEQUAL = 415,
    TOK_SLASHEQUAL = 416,
    TOK_INFEQUAL = 417,
    TOK_SUPEQUAL = 418,
    TOK_TRUE = 419,
    TOK_FALSE = 420,
    TOK_LABEL = 421,
    TOK_LABEL_DJVIEW = 422,
    TOK_PLAINDO_LABEL_DJVIEW = 423,
    TOK_PLAINDO_LABEL = 424,
    TOK_TYPE = 425,
    TOK_TYPEPAR = 426,
    TOK_ENDTYPE = 427,
    TOK_COMMACOMPLEX = 428,
    TOK_REAL = 429,
    TOK_INTEGER = 430,
    TOK_LOGICAL = 431,
    TOK_DOUBLEPRECISION = 432,
    TOK_ENDSUBROUTINE = 433,
    TOK_ENDFUNCTION = 434,
    TOK_ENDPROGRAM = 435,
    TOK_ENDUNIT = 436,
    TOK_CHARACTER = 437,
    TOK_CHAR_CONSTANT = 438,
    TOK_CHAR_CUT = 439,
    TOK_DATA = 440,
    TOK_CHAR_MESSAGE = 441,
    TOK_CSTREAL = 442,
    TOK_COMPLEX = 443,
    TOK_DOUBLECOMPLEX = 444,
    TOK_NAME = 445,
    TOK_CSTINT = 446
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 91 "fortran.y" /* yacc.c:355  */

    char        na[LONG_M];
    listdim     *d;
    listvar     *l;
    listcouple  *lc;
    listname    *lnn;
    typedim     dim1;
    variable    *v;

#line 368 "fortran.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE fortran_lval;

int fortran_parse (void);



/* Copy the second part of user declarations.  */

#line 385 "fortran.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   4595

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  208
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  524
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1080
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1749

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   446

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     202,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,   204,     2,     2,
     198,   199,    21,    19,     3,    20,     2,   203,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     4,     2,
     200,     5,   201,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   206,     2,   207,     2,   205,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     1,     2,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   196,   197
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   519,   519,   520,   522,   523,   524,   526,   528,   529,
     530,   531,   534,   535,   536,   538,   539,   547,   565,   569,
     570,   571,   575,   576,   601,   869,   870,  1123,  1124,  1125,
    1126,  1127,  1129,  1130,  1134,  1135,  1136,  1137,  1138,  1139,
    1140,  1141,  1142,  1143,  1144,  1145,  1146,  1147,  1148,  1149,
    1150,  1151,  1152,  1153,  1154,  1156,  1157,  1158,  1159,  1162,
    1163,  1166,  1167,  1168,  1172,  1183,  1184,  1185,  1185,  1186,
    1186,  1188,  1189,  1189,  1198,  1210,  1211,  1214,  1215,  1218,
    1219,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1230,  1277,
    1278,  1279,  1280,  1281,  1282,  1283,  1285,  1288,  1289,  1290,
    1291,  1293,  1294,  1304,  1305,  1357,  1360,  1361,  1386,  1387,
    1391,  1392,  1405,  1406,  1407,  1408,  1409,  1410,  1411,  1412,
    1413,  1414,  1415,  1416,  1417,  1420,  1421,  1425,  1428,  1429,
    1433,  1434,  1438,  1439,  1442,  1443,  1447,  1451,  1452,  1455,
    1456,  1460,  1461,  1465,  1466,  1467,  1468,  1469,  1470,  1471,
    1472,  1473,  1478,  1479,  1480,  1481,  1482,  1490,  1491,  1492,
    1493,  1494,  1495,  1496,  1497,  1498,  1499,  1500,  1501,  1502,
    1524,  1525,  1526,  1527,  1528,  1529,  1530,  1531,  1532,  1533,
    1534,  1535,  1539,  1542,  1547,  1548,  1552,  1553,  1554,  1555,
    1557,  1561,  1580,  1581,  1585,  1586,  1590,  1591,  1595,  1599,
    1600,  1601,  1612,  1612,  1614,  1615,  1620,  1620,  1622,  1622,
    1624,  1624,  1626,  1626,  1628,  1628,  1630,  1630,  1635,  1636,
    1642,  1644,  1646,  1653,  1654,  1659,  1660,  1665,  1666,  1682,
    1683,  1688,  1689,  1696,  1702,  1703,  1704,  1708,  1709,  1710,
    1713,  1714,  1719,  1720,  1725,  1726,  1727,  1728,  1729,  1733,
    1735,  1737,  1738,  1742,  1744,  1749,  1750,  1751,  1755,  1756,
    1760,  1760,  1765,  1766,  1769,  1770,  1773,  1774,  1777,  1778,
    1782,  1785,  1786,  1789,  1793,  1794,  1797,  1798,  1802,  1803,
    1807,  1811,  1814,  1815,  1816,  1819,  1820,  1824,  1825,  1826,
    1827,  1827,  1828,  1831,  1832,  1836,  1860,  1861,  1865,  1866,
    1869,  1870,  1874,  1875,  1876,  1880,  1885,  1887,  1890,  1891,
    1895,  1896,  1900,  1901,  1904,  1905,  1909,  1910,  1914,  1915,
    1916,  1920,  1922,  1937,  1941,  1945,  1949,  1950,  1955,  1956,
    1960,  1965,  1967,  1972,  1976,  1977,  1976,  2045,  2046,  2049,
    2050,  2054,  2055,  2059,  2060,  2062,  2064,  2064,  2066,  2068,
    2068,  2070,  2071,  2073,  2075,  2077,  2079,  2084,  2086,  2091,
    2125,  2128,  2131,  2132,  2136,  2142,  2148,  2157,  2161,  2163,
    2168,  2169,  2169,  2174,  2176,  2178,  2180,  2182,  2186,  2192,
    2201,  2203,  2208,  2213,  2217,  2223,  2232,  2234,  2239,  2245,
    2254,  2259,  2282,  2283,  2302,  2303,  2307,  2308,  2312,  2316,
    2318,  2320,  2326,  2325,  2344,  2345,  2349,  2351,  2356,  2357,
    2362,  2361,  2376,  2377,  2380,  2381,  2385,  2395,  2397,  2403,
    2405,  2410,  2411,  2415,  2421,  2428,  2430,  2435,  2436,  2440,
    2444,  2449,  2451,  2453,  2455,  2456,  2457,  2458,  2459,  2463,
    2464,  2480,  2481,  2482,  2483,  2484,  2485,  2486,  2492,  2500,
    2505,  2507,  2505,  2553,  2553,  2562,  2562,  2575,  2576,  2575,
    2595,  2597,  2602,  2619,  2620,  2619,  2627,  2628,  2631,  2632,
    2635,  2636,  2640,  2642,  2643,  2647,  2651,  2655,  2657,  2656,
    2668,  2669,  2673,  2676,  2677,  2681,  2682,  2686,  2689,  2690,
    2692,  2693,  2697,  2701,  2704,  2705,  2709,  2709,  2712,  2713,
    2717,  2718,  2719,  2724,  2725,  2724,  2734,  2735,  2743,  2749,
    2757,  2758,  2761,  2763,  2762,  2772,  2774,  2782,  2788,  2788,
    2797,  2798,  2799,  2800,  2809,  2812,  2825,  2828,  2832,  2836,
    2839,  2843,  2846,  2849,  2853,  2854,  2856,  2871,  2876,  2881,
    2882,  2887,  2889,  2889,  2901,  2905,  2910,  2915,  2917,  2924,
    2925,  2927,  2949,  2951,  2953,  2955,  2957,  2959,  2961,  2962,
    2964,  2966,  2970,  2972,  2974,  2976,  2978,  2981,  2995,  2999,
    3000,  2999,  3008,  3009,  3013,  3014,  3018,  3019,  3023,  3027,
    3031,  3032,  3036,  3040,  3041,  3044,  3045,  3049,  3050,  3054,
    3057,  3058,  3062,  3066,  3070,  3071,  3070,  3076,  3077,  3080,
    3081,  3085,  3086,  3090,  3091,  3100,  3110,  3111,  3112,  3113,
    3118,  3123,  3124,  3128,  3129,  3136,  3137,  3139,  3141,  3142,
    3147,  3151,  3153,  3157,  3159,  3164,  3165,  3170,  3173,  3174,
    3179,  3180,  3181,  3182,  3183,  3184,  3185,  3186,  3187,  3189,
    3190,  3192,  3197,  3198,  3204,  3205,  3211,  3212,  3217,  3218,
    3223,  3227,  3231,  3235,  3236,  3240,  3243,  3247,  3251,  3255,
    3256,  3259,  3263,  3270,  3274,  3278,  3281,  3285,  3291,  3292,
    3304,  3305,  3306,  3314,  3315,  3319,  3320,  3324,  3325,  3329,
    3333,  3337,  3340,  3349,  3353,  3354,  3355,  3359,  3363,  3366,
    3367,  3370,  3371,  3374,  3375,  3379,  3383,  3384,  3385,  3389,
    3393,  3397,  3398,  3402,  3403,  3408,  3409,  3413,  3417,  3420,
    3421,  3426,  3427,  3431,  3436,  3437,  3448,  3449,  3450,  3451,
    3454,  3455,  3456,  3457,  3461,  3462,  3463,  3464,  3469,  3470,
    3471,  3472,  3476,  3480,  3489,  3490,  3494,  3495,  3506,  3507,
    3513,  3523,  3528,  3529,  3530,  3531,  3532,  3533,  3534,  3535,
    3536,  3537,  3538,  3539,  3540,  3541,  3542,  3543,  3544,  3554,
    3555,  3558,  3559,  3570,  3575,  3578,  3579,  3583,  3587,  3590,
    3591,  3592,  3595,  3598,  3599,  3600,  3603,  3607,  3608,  3609,
    3613,  3614,  3618,  3619,  3623,  3624,  3628,  3632,  3635,  3636,
    3637,  3640,  3644,  3644,  3645,  3645,  3649,  3650,  3654,  3654,
    3655,  3655,  3660,  3660,  3661,  3665,  3666,  3671,  3672,  3673,
    3674,  3678,  3682,  3683,  3687,  3691,  3695,  3699,  3700,  3704,
    3705,  3709,  3710,  3711,  3715,  3719,  3723,  3723,  3723,  3726,
    3727,  3731,  3732,  3733,  3734,  3735,  3736,  3737,  3738,  3739,
    3740,  3741,  3742,  3743,  3747,  3751,  3755,  3755,  3759,  3760,
    3764,  3765,  3766,  3767,  3768,  3769,  3774,  3773,  3779,  3778,
    3783,  3784,  3789,  3788,  3794,  3793,  3801,  3802,  3804,  3805,
    3808,  3812,  3813,  3814,  3815,  3816,  3817,  3818,  3819,  3820,
    3821,  3822,  3823,  3827,  3828,  3829,  3832,  3833,  3836,  3837,
    3841,  3842,  3846,  3847,  3851,  3854,  3855,  3865,  3869,  3870,
    3874,  3875,  3879,  3880,  3884,  3885,  3886,  3887,  3888,  3892,
    3893,  3897,  3898,  3902,  3903,  3904,  3905,  3906,  3912,  3911,
    3915,  3914,  3919,  3923,  3924,  3928,  3929,  3930,  3931,  3932,
    3933,  3934,  3935,  3936,  3937,  3938,  3939,  3943,  3947,  3947,
    3950,  3951,  3956,  3955,  3976,  3975,  4000,  4001,  4004,  4005,
    4008,  4011,  4012,  4015,  4016,  4019,  4020,  4023,  4024,  4028,
    4033,  4032,  4071,  4070,  4122,  4123,  4124,  4128,  4129,  4134,
    4137,  4138,  4141,  4142,  4147,  4146,  4160,  4161,  4160,  4172,
    4173,  4175,  4176,  4179,  4183,  4186,  4192,  4196,  4205,  4215,
    4217,  4226,  4234,  4242,  4250,  4254,  4258,  4259,  4262,  4263,
    4266,  4270,  4274,  4275,  4278,  4282,  4283,  4283,  4290,  4289,
    4303,  4302,  4315,  4316,  4315,  4330,  4330,  4354,  4355,  4356,
    4360,  4361,  4366,  4374,  4385,  4386,  4396,  4399,  4400,  4404,
    4405,  4409,  4411,  4413,  4415,  4417,  4419,  4424,  4429,  4430,
    4428,  4454,  4479,  4484,  4485,  4489,  4506,  4505,  4510,  4511,
    4515,  4520,  4519,  4534,  4551,  4556,  4600,  4601,  4605,  4606,
    4606,  4611,  4612,  4617,  4629,  4643,  4645,  4650,  4651,  4656,
    4655,  4691,  4692,  4799,  4800,  4801,  4802,  4803,  4820,  4913,
    4914
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "','", "':'", "'='", "TOK_EQV",
  "TOK_NEQV", "TOK_OR", "TOK_XOR", "TOK_AND", "TOK_NOT", "TOK_LT",
  "TOK_GT", "TOK_LE", "TOK_GE", "TOK_EQ", "TOK_NE", "TOK_DSLASH", "'+'",
  "'-'", "'*'", "TOK_SLASH", "TOK_DASTER", "TOK_SEMICOLON",
  "TOK_PARAMETER", "TOK_RESULT", "TOK_ONLY", "TOK_INCLUDE",
  "TOK_SUBROUTINE", "TOK_PROGRAM", "TOK_FUNCTION", "TOK_LABEL_FORMAT",
  "TOK_LABEL_CONTINUE", "TOK_LABEL_END_DO", "TOK_MAX", "TOK_TANH",
  "TOK_COMMENT", "TOK_WHERE", "TOK_ELSEWHEREPAR", "TOK_ELSEWHERE",
  "TOK_ENDWHERE", "TOK_MAXVAL", "TOK_TRIM", "TOK_NULL_PTR", "TOK_SUM",
  "TOK_SQRT", "TOK_CASE", "TOK_SELECTCASE", "TOK_FILE", "TOK_REC",
  "TOK_NAME_EQ", "TOK_IOLENGTH", "TOK_ACCESS", "TOK_ACTION", "TOK_FORM",
  "TOK_RECL", "TOK_STATUS", "TOK_UNIT", "TOK_NEWUNIT", "TOK_OPENED",
  "TOK_FMT", "TOK_NML", "TOK_END", "TOK_EOR", "TOK_EOF", "TOK_ERR",
  "TOK_POSITION", "TOK_IOSTAT", "TOK_IOMSG", "TOK_EXIST", "TOK_MIN",
  "TOK_FLOAT", "TOK_EXP", "TOK_LEN", "TOK_COS", "TOK_COSH", "TOK_ACOS",
  "TOK_NINT", "TOK_CYCLE", "TOK_SIN", "TOK_SINH", "TOK_ASIN",
  "TOK_EQUIVALENCE", "TOK_BACKSPACE", "TOK_LOG", "TOK_TAN", "TOK_ATAN",
  "TOK_RECURSIVE", "TOK_PURE", "TOK_IMPURE", "TOK_ELEMENTAL", "TOK_ABS",
  "TOK_MOD", "TOK_SIGN", "TOK_MINLOC", "TOK_MAXLOC", "TOK_EXIT",
  "TOK_KIND", "TOK_MOLD", "TOK_SOURCE", "TOK_ERRMSG", "TOK_MINVAL",
  "TOK_PUBLIC", "TOK_PRIVATE", "TOK_ALLOCATABLE", "TOK_CONTIGUOUS",
  "TOK_RETURN", "TOK_THEN", "TOK_ELSEIF", "TOK_ELSE", "TOK_ENDIF",
  "TOK_PRINT", "TOK_PLAINGOTO", "TOK_LOGICALIF", "TOK_LOGICALIF_PAR",
  "TOK_PLAINDO", "TOK_CONTAINS", "TOK_ENDDO", "TOK_MODULE",
  "TOK_ENDMODULE", "TOK_WHILE", "TOK_CONCURRENT", "TOK_ALLOCATE",
  "TOK_OPEN", "TOK_CLOSE", "TOK_INQUIRE", "TOK_WRITE_PAR", "TOK_WRITE",
  "TOK_FLUSH", "TOK_READ_PAR", "TOK_READ", "TOK_REWIND", "TOK_DEALLOCATE",
  "TOK_NULLIFY", "TOK_DIMENSION", "TOK_ENDSELECT", "TOK_EXTERNAL",
  "TOK_INTENT", "TOK_INTRINSIC", "TOK_NAMELIST", "TOK_DEFAULT",
  "TOK_OPTIONAL", "TOK_POINTER", "TOK_CONTINUE", "TOK_SAVE", "TOK_TARGET",
  "TOK_IMPLICIT", "TOK_NONE", "TOK_CALL", "TOK_STAT", "TOK_POINT_TO",
  "TOK_COMMON", "TOK_GLOBAL", "TOK_LEFTAB", "TOK_RIGHTAB", "TOK_PAUSE",
  "TOK_PROCEDURE", "TOK_STOP", "TOK_FOURDOTS", "TOK_HEXA",
  "TOK_ASSIGNTYPE", "TOK_OUT", "TOK_INOUT", "TOK_IN", "TOK_USE",
  "TOK_EQUALEQUAL", "TOK_SLASHEQUAL", "TOK_INFEQUAL", "TOK_SUPEQUAL",
  "TOK_TRUE", "TOK_FALSE", "TOK_LABEL", "TOK_LABEL_DJVIEW",
  "TOK_PLAINDO_LABEL_DJVIEW", "TOK_PLAINDO_LABEL", "TOK_TYPE",
  "TOK_TYPEPAR", "TOK_ENDTYPE", "TOK_COMMACOMPLEX", "TOK_REAL",
  "TOK_INTEGER", "TOK_LOGICAL", "TOK_DOUBLEPRECISION", "TOK_ENDSUBROUTINE",
  "TOK_ENDFUNCTION", "TOK_ENDPROGRAM", "TOK_ENDUNIT", "TOK_CHARACTER",
  "TOK_CHAR_CONSTANT", "TOK_CHAR_CUT", "TOK_DATA", "TOK_CHAR_MESSAGE",
  "TOK_CSTREAL", "TOK_COMPLEX", "TOK_DOUBLECOMPLEX", "TOK_NAME",
  "TOK_CSTINT", "'('", "')'", "'<'", "'>'", "'\\n'", "'/'", "'%'", "'_'",
  "'['", "']'", "$accept", "input", "line", "line-break",
  "suite_line_list", "suite_line", "fin_line", "program-unit",
  "external-subprogram", "filename", "opt_comma", "uexpr", "signe",
  "operation", "after_slash", "after_equal", "lhs", "beforefunctionuse",
  "array_ele_substring_func_ref", "$@4", "$@5", "begin_array", "$@6",
  "structure_component", "funarglist", "funargs", "funarg", "triplet",
  "ident", "simple_const", "string_constant", "opt_substring", "opt_expr",
  "specification-part", "opt-use-stmt-list",
  "opt-declaration-construct-list", "declaration-construct-list",
  "declaration-construct", "opt-execution-part", "execution-part",
  "opt-execution-part-construct-list", "execution-part-construct-list",
  "execution-part-construct", "opt-internal-subprogram-part",
  "internal-subprogram-part", "opt-internal-subprogram",
  "internal-subprogram-list", "internal-subprogram",
  "other-specification-stmt", "executable-construct", "action-stmt",
  "keyword", "scalar-constant", "constant", "literal-constant",
  "named-constant", "opt-label", "label", "opt-label-djview",
  "label-djview", "type-param-value", "declaration-type-spec", "$@7",
  "intrinsic-type-spec", "$@8", "$@9", "$@10", "$@11", "$@12", "$@13",
  "opt-kind-selector", "kind-selector", "signed-int-literal-constant",
  "int-literal-constant", "kind-param", "signed-real-literal-constant",
  "real-literal-constant", "complex-literal-constant", "real-part",
  "imag-part", "opt-char_length-star", "opt-char-selector",
  "char-selector", "length-selector", "char-length",
  "char-literal-constant", "logical-literal-constant", "derived-type-def",
  "$@14", "derived-type-stmt", "opt-type-attr-spec-list-comma-fourdots",
  "opt-type-attr-spec-list-comma", "type-attr-spec-list", "type-attr-spec",
  "type-param-name-list", "type-param-name", "end-type-stmt",
  "opt-component-part", "component-part", "component-def-stmt",
  "data-component-def-stmt", "opt-component-attr-spec-list-comma-2points",
  "component-attr-spec-list", "component-attr-spec", "$@15",
  "component-decl-list", "component-decl", "opt-component-array-spec",
  "component-array-spec", "opt-component-initialization",
  "component-initialization", "initial-data-target", "derived-type-spec",
  "type-param-spec-list", "type-param-spec", "structure-constructor",
  "component-spec-list", "component-spec", "component-data-source",
  "array-constructor", "ac-spec", "lbracket", "rbracket", "ac-value-list",
  "ac-value", "ac-implied-do", "ac-implied-do-control", "ac-do-variable",
  "type-declaration-stmt", "$@16", "$@17", "opt-attr-spec-construct",
  "opt-attr-spec-comma-list", "attr-spec-comma-list", "attr-spec", "$@18",
  "$@19", "entity-decl-list", "entity-decl", "object-name",
  "object-name-noident", "opt-initialization", "initialization",
  "null-init", "access-spec", "opt-array-spec-par", "$@20", "array-spec",
  "explicit-shape-spec-list", "explicit-shape-spec", "lower-bound",
  "upper-bound", "assumed-shape-spec-list", "assumed-shape-spec",
  "deferred-shape-spec-list", "deferred-shape-spec", "assumed-size-spec",
  "opt-explicit-shape-spec-list-comma", "opt-lower-bound-2points",
  "implied-shape-spec-list", "implied-shape-spec", "intent-spec",
  "access-stmt", "$@21", "opt-access-id-list", "access-id-list",
  "access-id", "data-stmt", "$@22", "opt-data-stmt-set-nlist",
  "data-stmt-set-nlist", "data-stmt-set", "data-stmt-object-list",
  "data-stmt-value-list", "data-stmt-object", "data-implied-do",
  "data-i-do-object-list", "data-i-do-object", "data-i-do-variable",
  "data-stmt-value", "opt-data-stmt-star", "data-stmt-constant",
  "scalar-constant-subobject", "constant-subobject", "dimension-stmt",
  "$@23", "$@24", "array-name-spec-list", "$@25", "$@26", "parameter-stmt",
  "$@27", "$@28", "named-constant-def-list", "named-constant-def",
  "save-stmt", "$@29", "$@30", "opt-TOK_FOURDOTS", "opt-saved-entity-list",
  "saved-entity-list", "saved-entity", "proc-pointer-name",
  "get_my_position", "implicit-stmt", "$@31", "implicit-spec-list",
  "implicit-spec", "letter-spec-list", "letter-spec", "namelist-stmt",
  "opt-namelist-other", "namelist-group-object-list",
  "namelist-group-object", "equivalence-stmt", "equivalence-set-list",
  "equivalence-set", "$@32", "equivalence-object-list",
  "equivalence-object", "common-stmt", "$@33", "$@34",
  "opt-common-block-name", "common-block-name", "opt-comma",
  "opt-common-block-list", "$@35", "common-block-object-list",
  "common-block-object", "$@36", "designator", "scalar-variable",
  "variable", "variable-name", "scalar-logical-variable",
  "logical-variable", "char-variable", "scalar-default-char-variable",
  "default-char-variable", "scalar-int-variable", "int-variable",
  "substring", "substring-range", "data-ref", "opt-part-ref", "part-ref",
  "$@37", "scalar-structure-component", "structure-component",
  "array-element", "array-section", "section-subscript-list",
  "section-subscript", "section_subscript_ambiguous", "vector-subscript",
  "allocate-stmt", "$@38", "$@39", "opt-alloc-opt-list-comma",
  "alloc-opt-list", "alloc-opt", "stat-variable", "errmsg-variable",
  "allocation-list", "allocation", "allocate-object",
  "opt-allocate-shape-spec-list-par", "allocate-shape-spec-list",
  "allocate-shape-spec", "opt-lower-bound-expr", "lower-bound-expr",
  "upper-bound-expr", "deallocate-stmt", "$@40", "$@41",
  "allocate-object-list", "opt-dealloc-opt-list-comma", "dealloc-opt-list",
  "dealloc-opt", "primary", "level-1-expr", "mult-operand", "add-operand",
  "level-2-expr", "power-op", "mult-op", "add-op", "level-3-expr",
  "concat-op", "level-4-expr", "rel-op", "and-operand", "or-operand",
  "equiv-operand", "level-5-expr", "not-op", "and-op", "or-op", "equiv-op",
  "expr", "scalar-default-char-expr", "default-char-expr", "int-expr",
  "opt-scalar-int-expr", "scalar-int-expr", "specification-expr",
  "constant-expr", "scalar-default-char-constant-expr",
  "default-char-constant-expr", "scalar-int-constant-expr",
  "int-constant-expr", "assignment-stmt", "pointer-assignment-stmt",
  "opt-bounds-spec-list-par", "bounds-spec-list", "bounds-remapping-list",
  "bounds-spec", "bounds-remapping", "data-target",
  "procedure-component-name", "proc-component-ref", "proc-target",
  "where-stmt", "where-construct", "opt-where-body-construct",
  "opt-masked-elsewhere-construct", "opt-elsewhere-construct",
  "where-construct-stmt", "where-body-construct", "where-assignment-stmt",
  "mask-expr", "masked-elsewhere-stmt", "elsewhere-stmt", "end-where-stmt",
  "forall-header", "block", "opt-execution-part-construct", "do-construct",
  "block-do-construct", "label-do-stmt", "label-do-stmt-djview",
  "nonlabel-do-stmt", "loop-control", "do-variable", "do-block", "end-do",
  "end-do-stmt", "nonblock-do-construct", "action-term-do-construct",
  "do-term-action-stmt", "do-term-action-stmt-special",
  "outer-shared-do-construct", "label-do-stmt-djview-do-block-list",
  "inner-shared-do-construct", "do-term-shared-stmt",
  "opt-do-construct-name", "cycle-stmt", "if-construct",
  "opt-else-if-stmt-block", "else-if-stmt-block", "opt-else-stmt-block",
  "else-stmt-block", "if-then-stmt", "else-if-stmt", "else-stmt",
  "end-if-stmt", "if-stmt", "case-construct", "opt_case-stmt-block",
  "case-stmt-block", "select-case-stmt", "$@42", "$@43", "case-stmt",
  "end-select-stmt", "$@44", "$@45", "case-selector", "$@46",
  "case-value-range-list", "case-value-range", "case-value", "exit-stmt",
  "goto-stmt", "arithmetic-if-stmt", "continue-stmt", "stop-stmt",
  "stop-code", "io-unit", "file-unit-number", "internal-file-variable",
  "open-stmt", "$@47", "$@48", "connect-spec-list", "connect-spec",
  "file-name-expr", "iomsg-variable", "close-stmt", "$@49",
  "close-spec-list", "close-spec", "read-stmt", "$@50", "$@51",
  "write-stmt", "$@52", "$@53", "print-stmt", "io-control-spec-list",
  "namelist-group-name", "io-control-spec", "format", "input-item-list",
  "input-item", "output-item-list", "output-item", "io-implied-do",
  "io-implied-do-object-list", "io-implied-do-object",
  "io-implied-do-control", "rewind-stmt", "position-spec-list",
  "position-spec", "flush-stmt", "flush-spec-list", "flush-spec",
  "inquire-stmt", "$@54", "$@55", "set_in_inquire", "inquire-spec-list",
  "inquire-spec", "format-stmt", "module", "$@56",
  "opt-module-subprogram-part", "module-stmt", "$@57", "end-module-stmt",
  "$@58", "opt-tok-module", "opt-ident", "module-subprogram-part",
  "opt-module-subprogram-list", "module-subprogram-list",
  "module-subprogram", "use-stmt-list", "save_olduse", "use-stmt", "$@59",
  "$@60", "opt-module-nature-2points", "opt-only-list", "main-program",
  "opt-specification-part", "program-stmt", "$@61", "end-program-stmt",
  "$@62", "$@63", "opt-tok-program", "opt-tok-name", "module-nature",
  "opt-rename-list", "rename-list", "rename", "only-list", "only",
  "only-use-name", "generic-spec", "external-stmt", "external-name-list",
  "external-name", "intrinsic-stmt", "intrinsic-procedure-name-list",
  "intrinsic-procedure-name", "function-reference", "$@64", "call-stmt",
  "$@65", "$@66", "$@67", "$@68", "before-call-stmt", "$@69",
  "procedure-designator", "actual-arg-spec-list", "actual-arg-spec",
  "actual-arg", "opt-prefix", "prefix", "prefix-spec",
  "function-subprogram", "function-stmt", "$@70", "$@71", "function-name",
  "dummy-arg-name", "opt-suffix", "suffix", "end-function-stmt", "$@72",
  "opt-tok-function", "subroutine-subprogram", "subroutine-stmt", "$@73",
  "subroutine-name", "end-subroutine-stmt", "close_subroutine",
  "opt-tok-subroutine", "opt-dummy-arg-list-par", "$@74",
  "opt-dummy-arg-list", "dummy-arg-list", "dummy-arg", "return-stmt",
  "contains-stmt", "$@75", "opt_name", "after_rewind",
  "declare_after_percent", "pointer_name_list", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,    44,    58,    61,   258,   259,   260,   261,
     262,   263,   264,   265,   266,   267,   268,   269,   270,    43,
      45,    42,   271,   272,   273,   274,   275,   276,   277,   278,
     279,   280,   281,   282,   283,   284,   285,   286,   287,   288,
     289,   290,   291,   292,   293,   294,   295,   296,   297,   298,
     299,   300,   301,   302,   303,   304,   305,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   315,   316,   317,   318,
     319,   320,   321,   322,   323,   324,   325,   326,   327,   328,
     329,   330,   331,   332,   333,   334,   335,   336,   337,   338,
     339,   340,   341,   342,   343,   344,   345,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362,   363,   364,   365,   366,   367,   368,
     369,   370,   371,   372,   373,   374,   375,   376,   377,   378,
     379,   380,   381,   382,   383,   384,   385,   386,   387,   388,
     389,   390,   391,   392,   393,   394,   395,   396,   397,   398,
     399,   400,   401,   402,   403,   404,   405,   406,   407,   408,
     409,   410,   411,   412,   413,   414,   415,   416,   417,   418,
     419,   420,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,   431,   432,   433,   434,   435,   436,   437,   438,
     439,   440,   441,   442,   443,   444,   445,   446,    40,    41,
      60,    62,    10,    47,    37,    95,    91,    93
};
# endif

#define YYPACT_NINF -1325

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-1325)))

#define YYTABLE_NINF -1029

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -1325,  1536, -1325, -1325, -1325,   -66,   -62, -1325, -1325, -1325,
   -1325, -1325, -1325,   -34,   814, -1325, -1325,    74,   197, -1325,
   -1325, -1325, -1325,   963, -1325,   115, -1325,   115,   806,   647,
   -1325, -1325,   115, -1325,   115, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325, -1325,   218,   234,   309, -1325,
   -1325, -1325,  1272, -1325, -1325,  3984,   259,   115, -1325,   497,
    4292,   245,   303, -1325, -1325,  4292,  4292, -1325,   374,   374,
     174,   174,   174,   174,   244,   174,  1067, -1325, -1325, -1325,
   -1325, -1325, -1325,   374,   361, -1325, -1325,    92,    61,   418,
     547, -1325, -1325,    92,   114, -1325, -1325,   650, -1325,   614,
   -1325,   415, -1325,  3984, -1325, -1325,   526,   794,   437, -1325,
   -1325, -1325,   448,   444, -1325, -1325, -1325,   511, -1325, -1325,
     513,   587, -1325, -1325, -1325, -1325,   232,   737, -1325,   552,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325,   642, -1325, -1325, -1325,   880,   566,   579,
    2542,   225,   366,   520,   622,   625, -1325,  3699,  3761,   629,
     645,  2823,   798,   688, -1325,  4177, -1325,   849, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
     815, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325,   682, -1325, -1325,   712, -1325,
     714,   688,   688,    74,    74,   717,  2852, -1325, -1325, -1325,
   -1325, -1325,   592,  1008, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325,  3790, -1325, -1325, -1325,   722,   729,
    3801, -1325,    91,   935, -1325, -1325, -1325,   764, -1325, -1325,
     437, -1325,    87, -1325, -1325,  3790, -1325, -1325,   928, -1325,
     641,   250,  1286,   498, -1325, -1325,   943,   957,   807,  1422,
   -1325, -1325, -1325, -1325,   769,   777,    74, -1325,   106, -1325,
   -1325,    74,   487,   374,   782, -1325,   111, -1325, -1325,   791,
     792,   740,    74,   374,   783,   793,   451,   271,   117,   686,
   -1325, -1325, -1325, -1325,    40, -1325, -1325,  2823,  3235,  3801,
     374,   987,   998,  3801,   718,   119, -1325,   796,   418,   418,
     289,  3848,  3801,   869,  3801,  3801,   818, -1325,  4090,   574,
     851,   916,   129, -1325, -1325, -1325,  1092, -1325, -1325, -1325,
    3801,  3801,   410,   415, -1325, -1325,   374,   374,    74,   374,
   -1325, -1325, -1325, -1325, -1325,   832,  3533, -1325,   374,  3578,
     374, -1325,   837,    74, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325,   374,   537,   374, -1325, -1325, -1325,  4205, -1325, -1325,
   -1325,  3801,   840,  2510,  2510,  3235, -1325,   485,   -28,   116,
   -1325, -1325,   844,   374, -1325, -1325, -1325, -1325, -1325, -1325,
    1039,   846,  1067, -1325, -1325,  1043,  1046,   103,  3790,   898,
    1052, -1325, -1325, -1325,   705,   705,   350,   882, -1325,   883,
     884,  1286,   860,  1067,  1067, -1325,   857, -1325,  1286, -1325,
   -1325,  1286, -1325, -1325,  1286,   886,   641, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
    1422,  1422, -1325,  3801, -1325,  3801, -1325, -1325,  3801, -1325,
     867,   876,   817,   361,    74,   875, -1325, -1325,  1069,    74,
     111,   782,    74, -1325,   118, -1325,  1053, -1325,   888,   889,
   -1325,    74,  1073, -1325, -1325,   374, -1325,   879, -1325,  1077,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325,   121,   650,   650,
     604,  3801,    92,    92,  1388,    74,   374, -1325,    94, -1325,
   -1325, -1325,   122,   890,    74,   976,  3801,   894,  1090, -1325,
     216,   917,   727, -1325, -1325,  1888,   900,   939,  1099,   374,
   -1325,  1101, -1325, -1325,   911,   240, -1325,   910,   126, -1325,
   -1325,   101,   907, -1325, -1325, -1325, -1325,   374,  1109, -1325,
     104,   107, -1325, -1325,   817,   374,   915,   837, -1325, -1325,
      92,  1113,  1007,  2348, -1325, -1325, -1325, -1325,   -47, -1325,
     433, -1325,   930,   808, -1325, -1325,   991, -1325,   937,   374,
     949, -1325, -1325, -1325,   927,   940,    74,    74,    74,   837,
    3118,  3317,  3801,   366,   817,   817,   860, -1325,   123, -1325,
      74,  3801,   366,   817,   817, -1325,   140, -1325,    74,   837,
   -1325,   145,    74,   942,   630, -1325,   955, -1325,   950, -1325,
   -1325,  1142,  3607,  3235,   952,   366,   366,   366,   817,   817,
   -1325, -1325, -1325, -1325, -1325, -1325,   148, -1325, -1325, -1325,
     162,   128,   492,   817, -1325, -1325, -1325,  1114, -1325, -1325,
   -1325, -1325,   236,   960, -1325, -1325, -1325, -1325,  3801,    74,
      79,   374,    79,   977, -1325,   979, -1325,  3801, -1325,   964,
    1067,  3801,  3801, -1325,  1164,   860, -1325,  3790, -1325, -1325,
   -1325, -1325,   221,   990, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325,   641,   250,  1152, -1325,   943,   957, -1325,  3801,
    1166,   170, -1325,   242,  1169, -1325, -1325,   978, -1325, -1325,
    3801, -1325,  3801,    74, -1325,   791,    74,   837,  1155,   986,
    1171, -1325,   783,    74,   993,   271,   374,   650, -1325, -1325,
     989, -1325,  1170, -1325, -1325,   246, -1325,   992, -1325, -1325,
     693, -1325,  1170, -1325,  1172,   602, -1325,   995,    74,   374,
      74,   374,  1551,   366,  3801,   131,   171, -1325, -1325,   247,
   -1325,    74,  3863,    74,  1084,  3801,   374, -1325,  3801,   640,
   -1325,   837,   292, -1325, -1325, -1325, -1325, -1325,   997, -1325,
    1001, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,  1199,
   -1325,  1005, -1325,  1888,    74,   796,  1009,  1205, -1325, -1325,
   -1325,  1206, -1325, -1325, -1325,   374,  1014,   448,    74,  1016,
      74,  3801,  3801, -1325,  3801,  1062, -1325,   374,    74, -1325,
   -1325,    74,   374,  1041,   358,  1019,  3893,  1020,  1118,   817,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325,   851, -1325, -1325,
    1075,  3801,   391, -1325,   746, -1325, -1325, -1325, -1325,  1061,
    1219,    74,  1105,   430, -1325, -1325, -1325, -1325,  1224, -1325,
    1031,  3801,  3801,  3801,  3801,  3801,  1234,  3801,   817,   366,
    3801,   817,   817, -1325,   172, -1325,  3801,  1235,   817,   817,
     817,   817,  3801,   817,   366,   817,   817,   817, -1325,   175,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
    3533,   374, -1325, -1325, -1325, -1325,  3578,   374, -1325,  1239,
     837, -1325,  3801, -1325,  1515, -1325, -1325, -1325,  1216,  4320,
    3379,  3801, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325,  2510,  3863,   701,   701,    74, -1325, -1325,  3801,
     881, -1325,  1647,   374,    74, -1325,   374,   374,    91,  1245,
   -1325, -1325,   177, -1325, -1325, -1325, -1325, -1325,  1054,  1248,
   -1325,    74,  1072,  1221,  1232,  1076, -1325,   178,   186,  1078,
    3790, -1325, -1325, -1325, -1325, -1325,  1079,   187,  3801,   876,
   -1325,   817,  3801,  1081,  1263, -1325, -1325,  1271, -1325, -1325,
   -1325, -1325,   889,   707, -1325, -1325,   189, -1325,   139, -1325,
    1267, -1325,    74, -1325,  1067,   749, -1325, -1325,  3567,   604,
   -1325, -1325, -1325, -1325,  1168,    74,    74,  3801,  1277, -1325,
   -1325,  3654,  1388, -1325,  1687,  3801, -1325,  3863, -1325,   134,
   -1325, -1325,   374,  1085,    74, -1325, -1325, -1325,  1087, -1325,
     222, -1325, -1325,  1088,   136, -1325,   374,    74, -1325, -1325,
     900,   374, -1325,  1262, -1325, -1325, -1325,  1091,   374,   374,
     240,    74,  1268,   191, -1325, -1325, -1325, -1325, -1325, -1325,
    1287, -1325,  1288, -1325,   817,    74,    74,    92,   374,    74,
    3801,  2893,  2799,  3089, -1325,   837,  3801,  4398, -1325,   851,
    1100,   374,    74,   405, -1325, -1325, -1325, -1325,    88, -1325,
   -1325,  1094,    74, -1325,   374,   135,  1106,  3801, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325, -1325,  3801, -1325, -1325, -1325,
   -1325, -1325, -1325,  3118, -1325, -1325,   817,  1108, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,  3471,
   -1325, -1325,    74, -1325,    74,   438,  1112, -1325,  1115, -1325,
   -1325,  1102, -1325,   985,   230,  1299,  3801,   366,   817,   817,
   -1325,   201, -1325, -1325, -1325,   374,  1310,  3863, -1325,   374,
    1312, -1325, -1325,   185,  1119,   440,   441, -1325, -1325,   485,
    3801, -1325,   203, -1325,  1313,    74,   374,    74,    74,  3801,
    3801, -1325, -1325,    79,  1294, -1325,  1094, -1325,  1094, -1325,
    1225, -1325,  1251, -1325, -1325,   139,  1123,  1321, -1325, -1325,
   -1325, -1325, -1325, -1325,   374,   205, -1325,  1129, -1325,  3801,
     837,   199,  1760, -1325,   374,   740,   993,   374,  3801,   221,
     519, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325,  1325,   209, -1325, -1325,  1127, -1325, -1325,
   -1325, -1325,   374, -1325,  3801,   366, -1325, -1325,  3801,  1328,
   -1325,   860, -1325,  1333, -1325,  3863,    74,    74,  1229, -1325,
     640, -1325,  1899,  1262,   837,    74,    74,  1760,   609, -1325,
      74,  1760,   592,   195,  1760,  1140,    74,    74, -1325,  1144,
    1014, -1325, -1325,  3801,   374,    74,   374,    74,  1143,  3801,
   -1325, -1325, -1325, -1325, -1325,   496,   506,   589,   736,   969,
     607,   623,  1145,  3801,  1139, -1325,   817,  1149,   528,  1153,
     850,  1776,   210,  1156, -1325,  1244,    74,   374,    74,  1350,
    1207,  1354, -1325,   374, -1325, -1325,    74,   817,  1360,  1356,
   -1325, -1325, -1325,   213, -1325,  3801,  1363, -1325, -1325,   374,
   -1325,  3863, -1325,   374,   817,  1364, -1325,  1367, -1325, -1325,
   -1325, -1325, -1325,  3801,   366,  3801, -1325, -1325, -1325, -1325,
    3379,   374,    74,   374,    74,   701,   374,    74,   477,   374,
      74,   374,    74,   485, -1325,  1647, -1325,  3801,    74,   415,
   -1325, -1325,   374, -1325,  1176, -1325, -1325, -1325, -1325,  1370,
    1373, -1325,  3801,    74,   817, -1325, -1325,  1377, -1325,    74,
    1359, -1325,  1183,  1380, -1325,  1381, -1325,  1383, -1325,  1384,
   -1325, -1325,  3801,  1368,  1385, -1325, -1325,  1386,    74, -1325,
   -1325,    74,  1390, -1325, -1325,  3848,  3848, -1325,    74, -1325,
   -1325, -1325,  3801,  3863, -1325,   374,  1899, -1325, -1325,  1185,
    1392,  1394,  1384,   217, -1325,  1197, -1325, -1325, -1325,  1202,
    1203, -1325,  3801,   571, -1325, -1325,  1204, -1325, -1325, -1325,
      74,    74,   459, -1325, -1325, -1325, -1325, -1325, -1325,   978,
   -1325,   386, -1325, -1325,  1208, -1325, -1325,  1923,  3801,  3801,
    3801,  3801,  3801,  3801,  3801,  3801,  3801,  3801,  3801,  3801,
    3801,  3801,  1849,  3801,  2238,  2434, -1325, -1325,  4398,   580,
      74,  1209,  1214,  1217,    74,   374, -1325, -1325,   817,    27,
     374,  3801, -1325, -1325, -1325,    74,  1310,    74, -1325,   817,
      46,   374,   374,   374,  1213,  1401, -1325, -1325,    74,    74,
   -1325,    74,   374,    74,    74,    74, -1325, -1325,    74,  1218,
     374, -1325,   374,  3801,  1067,  1413, -1325,  3801,  1223, -1325,
    3801,  3665,  2106,  1416,  1417,  1396, -1325, -1325,  3801,   889,
    3801, -1325, -1325, -1325,  1418, -1325,  1226,    74,  1227, -1325,
    3801,  3801,  3801,   571, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325,  1760,   157, -1325, -1325, -1325,  3801,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325, -1325,  3801,  3801, -1325, -1325,
   -1325,  3801, -1325,  3801, -1325,   374,    74,  1207, -1325, -1325,
    1421, -1325, -1325, -1325, -1325, -1325,    74, -1325, -1325, -1325,
      74, -1325,   374, -1325, -1325,    74,    74,    74,  4398,   366,
      74,  1228,    74,   374,    74,  1231,  1236,  3801, -1325,  1409,
   -1325, -1325, -1325, -1325,  1430, -1325, -1325, -1325, -1325, -1325,
    1171,   214,  3801, -1325, -1325, -1325, -1325, -1325,  1238,  1139,
    1240,  2172,  1247,  1249,  1253, -1325, -1325, -1325, -1325, -1325,
      74,   374,  1209,    74,   374, -1325,    74, -1325, -1325,  1440,
     837, -1325,  3801, -1325,  1444, -1325, -1325,  2201,  1452, -1325,
   -1325,  1454, -1325,   817, -1325,    74, -1325,    74,  3801, -1325,
    1260,  3801,  3801,  1457,  2172,  3801, -1325, -1325, -1325,  1459,
   -1325,  3801, -1325,  1460,  3801, -1325,  3801, -1325, -1325
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     1,     6,     8,     0,     0,    17,     9,  1033,
    1034,  1035,  1036,  1032,     0,    18,     3,     4,     5,    12,
      15,    20,  1031,     0,    21,   106,    19,   106,     0,   202,
    1029,    22,   106,    23,   106,    24,    18,   974,   942,   208,
     206,   216,   210,   214,   212,    88,   306,     0,     0,     7,
      11,    18,   202,   203,   971,   108,     0,   107,   957,   192,
     192,     0,     0,  1032,  1030,   192,   192,    16,     0,     0,
     218,   218,   218,   218,   242,   218,     0,   204,   205,    10,
      13,    14,   457,     0,     0,   368,   369,    25,     0,   466,
       0,   503,   194,    25,   264,   255,   257,     0,   256,    88,
     195,   541,   105,   109,   110,   116,     0,   193,     0,   112,
     260,   117,   202,   404,   143,   145,   146,     0,   113,   151,
       0,     0,   115,   150,   147,   144,   525,     0,   523,   534,
     539,   522,   520,   521,   118,   119,   120,   711,   709,   709,
     712,   738,   739,   121,   709,   122,   124,   114,   148,   149,
     123,   959,   958,     0,   193,   938,   941,   202,     0,     0,
     103,     0,     0,     0,     0,     0,   922,     0,     0,     0,
       0,     0,    88,   134,   126,   192,   152,     0,   157,   163,
     158,   173,   179,   156,   689,   153,   162,   155,   170,   154,
     788,   165,   164,   181,   161,   178,   172,   160,   175,   180,
     174,   177,   166,   171,   159,  1008,   176,  1053,  1058,  1041,
       0,   134,   134,   975,   943,     0,     0,   209,   219,   207,
     217,   211,     0,     0,   215,   243,   244,   213,   201,   650,
     623,   624,   200,  1018,     0,   258,   259,  1019,   231,   225,
       0,   324,   541,     0,   606,   310,   618,   186,   187,   189,
     190,   188,     0,   308,   607,     0,   605,   610,   611,   613,
     615,   625,     0,   628,   642,   644,   646,   648,   655,     0,
     658,   661,   199,   608,     0,     0,   937,   496,     0,   494,
      26,   725,     0,     0,     0,  1000,     0,   998,   467,     0,
       0,   506,   717,     0,     0,     0,     0,     0,   510,     0,
     417,   422,   525,   421,     0,   542,   111,     0,     0,     0,
       0,    88,     0,   659,   202,   337,   402,     0,   466,   466,
     202,     0,     0,     0,     0,   659,   538,   733,   192,   196,
     196,   769,   964,  1069,   476,   950,   202,   953,   955,   956,
       0,     0,    88,   541,   167,   104,     0,     0,   812,     0,
    1072,  1071,   169,   569,   826,     0,     0,   824,     0,     0,
       0,   594,     0,   817,   657,   665,   667,   819,   664,   820,
     666,     0,     0,     0,   976,   135,   127,   192,   130,   132,
     133,     0,     0,     0,     0,     0,  1015,   691,     0,     0,
     789,   709,  1012,     0,  1059,  1051,  1038,   476,   476,   222,
       0,     0,     0,   254,   251,     0,     0,     0,     0,     0,
     323,   326,   329,   328,     0,     0,   541,   618,   235,   187,
       0,     0,     0,     0,     0,   307,     0,   620,     0,   621,
     622,     0,   619,   223,     0,   186,   616,   632,   634,   633,
     635,   630,   631,   627,   636,   637,   639,   641,   638,   640,
       0,     0,   651,     0,   652,     0,   653,   654,     0,   643,
    1006,     0,     0,     0,   493,     0,   707,   732,     0,   727,
       0,     0,   996,  1004,     0,  1002,     0,   508,     0,     0,
     507,   719,   267,   268,   270,     0,   265,     0,   429,     0,
     425,   545,   428,   544,   427,   511,   410,   510,     0,     0,
       0,     0,    25,    25,   549,  1067,     0,   885,   225,   884,
     657,   883,     0,     0,   816,     0,     0,     0,     0,   660,
     282,     0,   202,   278,   280,     0,     0,     0,   340,     0,
     408,   405,   406,   409,     0,   468,   478,     0,     0,   480,
      88,   605,     0,   524,   684,   685,   686,     0,     0,   592,
       0,     0,   675,   677,     0,     0,     0,     0,   710,   198,
      25,     0,     0,   192,   709,   714,   734,   740,     0,   760,
     192,   715,     0,   773,   770,   709,     0,   965,     0,     0,
       0,   939,   954,   700,     0,     0,   767,   813,   814,     0,
       0,     0,     0,     0,     0,     0,   658,   913,     0,   911,
     909,     0,     0,     0,     0,   904,     0,   902,   900,     0,
    1079,     0,   818,     0,   202,   969,     0,   131,     0,   846,
     822,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      88,   529,   825,   871,   821,   823,     0,   874,   868,   873,
       0,     0,     0,     0,   699,   697,   698,   693,   690,   696,
     804,   802,     0,   798,   790,   787,   791,  1010,     0,  1009,
    1061,     0,  1061,     0,  1037,     0,  1050,     0,   220,     0,
       0,     0,     0,   249,     0,   328,   321,     0,   228,   227,
     232,   226,     0,   187,   609,   311,   309,   325,   322,   186,
     612,   614,   617,   626,   629,   645,   647,   649,  1005,     0,
       0,     0,   460,   526,     0,   500,   502,   534,   501,   495,
       0,   731,     0,   997,   999,     0,  1001,     0,     0,   517,
     512,   515,     0,   262,     0,     0,     0,     0,   414,   418,
     541,   434,   223,   435,   229,   439,   437,     0,   438,   436,
       0,   419,   439,   448,   305,     0,   367,     0,   724,     0,
     716,     0,   553,     0,     0,   541,     0,   550,   558,   567,
     568,  1068,     0,   866,     0,     0,     0,   536,   659,     0,
     283,     0,     0,   261,   279,   353,   344,   345,     0,   348,
       0,   351,   352,   354,   355,   356,   341,   343,   361,   335,
     357,   370,   338,     0,   403,     0,     0,   451,   360,   472,
     464,   469,   470,   473,   474,     0,     0,   202,   477,     0,
     672,   679,     0,   674,     0,     0,   681,     0,   668,   535,
     540,   721,     0,     0,     0,     0,     0,     0,     0,   193,
     742,   746,   743,   757,   741,   751,   748,   735,   753,   745,
     755,   758,   754,   756,   747,   752,   744,   761,   709,   759,
       0,     0,     0,   771,     0,   774,   709,   772,   983,     0,
     984,  1070,   946,     0,   794,   583,   545,   584,   572,   580,
     585,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   831,     0,   829,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   925,     0,
     923,   914,   917,   533,   915,   532,   531,   845,   530,   916,
       0,     0,   905,   908,   907,   906,     0,     0,   597,   599,
       0,   168,     0,   136,   202,   139,   141,   142,   979,   192,
       0,     0,   822,   872,   876,   870,   875,   877,   878,   879,
     881,   880,     0,   862,   856,     0,   860,  1017,  1016,     0,
       0,   689,     0,     0,   796,   800,     0,     0,   541,     0,
    1025,  1024,     0,  1020,  1022,  1066,  1042,  1065,     0,  1062,
    1063,  1052,     0,  1048,  1056,     0,   253,     0,     0,     0,
       0,   327,   191,   239,   237,   238,     0,     0,     0,     0,
     458,     0,   659,     0,     0,  1003,   526,   488,   490,   492,
     509,   518,     0,   504,   269,   273,     0,   271,   541,   426,
       0,   430,   411,   415,   542,     0,   432,   433,     0,     0,
     416,   431,   224,   230,     0,   726,   718,     0,   554,   561,
     557,     0,     0,   543,   562,     0,   552,     0,   892,     0,
     890,   893,     0,     0,   669,   537,   288,   289,     0,   292,
       0,   285,   287,   296,     0,   293,     0,   274,   346,   349,
       0,     0,   371,   240,   342,   407,   453,     0,     0,     0,
       0,   479,   485,     0,   483,   481,   682,   683,   680,   593,
       0,   676,     0,   678,     0,   670,   723,    25,     0,   736,
       0,  1077,  1075,     0,   749,     0,     0,   192,   763,   762,
       0,     0,   782,     0,   775,   768,   776,   966,     0,   960,
     947,   948,   695,   687,     0,     0,     0,     0,   582,   844,
     656,   836,   833,   834,   837,   842,     0,   832,   840,   835,
     841,   839,   838,     0,   827,   927,     0,     0,   928,   929,
     936,   926,   528,   935,   527,   930,   933,   932,   931,     0,
     918,   912,   910,   903,   901,     0,     0,  1080,     0,   140,
     980,   981,   786,     0,   193,     0,     0,     0,     0,     0,
     850,     0,   848,   882,   869,     0,   864,     0,   888,     0,
     858,   886,   889,     0,     0,     0,     0,   689,   688,   692,
       0,   811,     0,   805,   807,   797,     0,   799,  1011,     0,
       0,  1013,  1060,     0,  1043,  1049,   948,  1057,   948,   221,
       0,   250,     0,   247,   246,   541,     0,     0,   333,   233,
    1007,   663,   462,   461,     0,     0,   498,     0,   730,     0,
       0,   510,   392,   516,     0,     0,     0,     0,     0,     0,
     191,   441,   183,   184,   185,   443,   444,   446,   447,   445,
     440,   442,   312,     0,     0,   314,   316,   681,   318,   319,
     320,   420,     0,   555,     0,     0,   559,   551,     0,   563,
     566,   892,   897,     0,   895,     0,   867,   779,     0,   290,
       0,   284,     0,   240,     0,   281,   275,   392,     0,   358,
     336,   392,     0,   362,   392,     0,   452,   465,   471,     0,
       0,   482,   679,     0,     0,   720,     0,   737,     0,     0,
      32,    33,    91,    71,    94,   258,   259,   255,   257,   256,
     231,   225,     0,     0,    27,    63,    65,    62,   541,    28,
     101,   658,     0,     0,   764,     0,   783,     0,   784,     0,
       0,   985,   986,     0,   949,   944,   795,     0,     0,   573,
     574,   581,   570,     0,   587,     0,     0,   843,   830,     0,
     934,     0,   924,     0,     0,     0,   598,   600,   601,   595,
     792,   982,   977,     0,     0,     0,   851,   854,   853,   852,
       0,     0,   863,     0,   857,     0,     0,   861,     0,     0,
     703,     0,   705,   694,   809,     0,   803,   808,   801,   541,
    1023,  1021,     0,  1064,     0,  1039,  1044,  1055,  1055,     0,
       0,   330,     0,   459,     0,   497,   535,   728,   491,   487,
       0,   386,     0,   373,   378,     0,   381,   374,   384,   375,
     388,   376,   394,     0,   377,   396,   662,   383,   505,   513,
     272,   263,     0,   236,   234,     0,     0,   313,   777,   556,
     560,   564,     0,     0,   891,     0,     0,   286,   390,     0,
     298,     0,   299,   300,   294,     0,   400,   401,   399,     0,
       0,   241,     0,     0,   359,   363,     0,   455,   486,   484,
     671,   722,     0,    31,  1074,  1076,    30,  1078,    66,   534,
      67,    72,  1073,    95,    98,    96,   102,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    55,     0,     0,     0,    29,   750,   192,     0,
     785,   967,     0,     0,   961,     0,   579,   576,     0,     0,
       0,     0,   586,   589,   591,   828,   920,   919,   603,     0,
       0,     0,     0,     0,     0,     0,   855,   849,   847,   865,
     887,   859,     0,   701,   704,   706,   806,   810,  1014,     0,
       0,  1046,     0,     0,     0,     0,   499,     0,     0,   519,
     393,   387,     0,     0,     0,     0,   382,   398,   394,     0,
       0,   317,   315,   565,     0,   896,     0,   778,     0,   297,
       0,     0,     0,     0,   295,   301,   347,   350,   372,   364,
     366,   365,   305,   454,   392,     0,    64,    64,    64,     0,
      54,    60,    39,    49,    51,    50,    52,    45,    40,    47,
      46,    38,    48,    34,    35,    36,     0,     0,    53,    56,
      37,     0,    42,     0,    41,     0,   780,   994,   962,   993,
     968,   989,   992,   991,   988,   987,   945,   578,   577,   575,
     571,   588,     0,   604,   602,   596,   793,   978,   192,     0,
     702,     0,  1040,     0,  1054,     0,     0,     0,   729,     0,
     379,   380,   383,   386,     0,   385,   389,   395,   391,   397,
     514,     0,     0,   894,   291,   302,   304,   303,     0,    74,
      61,    75,     0,     0,     0,    59,    57,    58,    44,    43,
     781,     0,     0,   921,     0,  1045,  1047,   245,   248,   331,
       0,   387,     0,   423,     0,   456,    72,    87,    76,    77,
      80,    79,    68,     0,    73,   963,   990,   815,     0,   489,
       0,     0,     0,    85,     0,    86,    70,   332,   424,   898,
      84,     0,    78,    81,     0,    83,     0,   899,    82
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1325, -1325, -1325,   948, -1325,  1411,   501, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325, -1325,  -140, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325,  -684, -1325,  -267, -1325,   -14, -1325,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325,  1366,   871, -1325,
   -1325, -1325,   100,   748, -1325, -1325, -1325,   546, -1325,   -73,
    -918,  -638, -1325, -1325,   456,   470,   -45,   -29, -1325,   633,
    -218,   -77, -1325,  1464, -1325, -1325, -1325, -1325, -1325, -1325,
     877, -1325,  -149,  -189,  1065,  -436,  -190, -1325, -1325, -1325,
     207, -1325, -1325, -1325,   196,   -37, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325,   771, -1325,   258, -1325, -1325, -1325,   974,
   -1325, -1325, -1325,   219, -1325, -1325,   223, -1325,    41, -1325,
   -1325,  -977,  1484, -1325,  1082,   486, -1325,    63,    65, -1325,
    1261, -1325, -1325,  1107,  -596, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325,   725, -1325, -1325, -1325,   460, -1325,
   -1325, -1325, -1325,  -976,  -269, -1325, -1325, -1207, -1174,  -910,
   -1179,  -900, -1325,   -51, -1115,   -50, -1325, -1325,    90, -1325,
     -52, -1325, -1325, -1325, -1325, -1325,   733, -1325, -1325, -1325,
   -1325,  -410, -1325, -1325,  1025,  -252, -1325,   805, -1325,   510,
    -508, -1325,   518, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325,   545, -1325, -1325, -1325,   -21, -1325,
   -1325,   468, -1325,    -2, -1325, -1325, -1325,   728, -1325,   239,
   -1325, -1325,  -170,   311, -1325, -1325,  1086, -1325, -1325,  -943,
   -1325, -1325, -1325, -1325,  -278,  -475, -1325, -1325,   -31,   548,
   -1325,  1269, -1325,  2066,  -456,   655, -1325, -1325,  -811, -1325,
    -529, -1325,  -441,  -288,  -294, -1325,   996, -1325, -1325,  -282,
    -290, -1325, -1325,   522, -1325, -1325,   994, -1325, -1325, -1325,
   -1325,    32,    24,   208, -1325,   454,  -573, -1325, -1325,    43,
   -1325,  -228,   220,  1024, -1325, -1325, -1325, -1325, -1325,    39,
   -1325, -1325,   300,    98,  1132, -1325, -1325,  -121,  1137, -1325,
    1314, -1325,  1146,  1141,  1158, -1325, -1325, -1325, -1325, -1325,
    1847,  -805,  -155,  -169,   827,   -59,  -885, -1258, -1325, -1325,
    -212, -1325,   -54,   169, -1325, -1325, -1325,   786,   803,  -510,
     801, -1325,  1301,  -378,  -379,  -869, -1325, -1325, -1325, -1325,
    -832,  -544, -1325, -1325, -1325, -1325,  -117, -1325,   348, -1325,
   -1325,  1063, -1325,   -53,  -699,  -107,  1302, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325,  1068, -1325, -1325, -1325,   419, -1325,
    -487, -1325, -1325, -1325, -1325, -1325, -1325,  1044, -1325, -1325,
    1246, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1325,
   -1325,   238, -1105, -1325,  1074, -1325,     8, -1325, -1325,  1017,
    -112, -1325,  1080, -1325, -1325, -1325,   505,   754,  -570,  1096,
   -1325, -1325,   261,  1097, -1325, -1325,  1098, -1325, -1325,     2,
    1258,  1021,   706,  -231,   702,   264,  -889,  -966,  -902, -1325,
     204, -1325,  1110, -1325,   734,  1116, -1325,   744,  1120, -1325,
   -1325, -1325, -1325,   514,   427, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325,  -337, -1325, -1325, -1325,  1326, -1325, -1325,
    1607, -1325, -1325, -1325, -1325, -1325,   804, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325, -1325, -1325, -1032, -1325,   -32,
   -1325, -1324, -1325,  1387,  1194, -1325, -1325,   953,  -488, -1325,
    1121, -1325, -1325, -1325, -1325, -1325, -1325,  1033,   982,   476,
     478, -1325, -1325,  1649,  -138, -1325, -1325, -1325, -1325, -1325,
   -1325, -1325, -1325, -1325, -1325,  -133, -1325, -1325, -1325, -1325,
     274, -1325, -1325, -1325,  1026, -1325,   484,   465, -1325, -1325,
   -1325, -1325, -1325,   594
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    16,    17,    18,    19,    49,    20,    21,    36,
     282,  1322,  1323,  1516,  1628,  1610,  1324,  1691,  1325,  1606,
    1607,  1326,  1608,  1327,  1692,  1718,  1719,  1720,   343,  1329,
    1330,  1495,   344,    54,    55,   102,   103,   104,   173,   174,
     376,   377,   378,   374,   375,   923,   924,   925,   105,   175,
     176,   243,  1241,  1242,   244,   983,   177,   107,   562,  1097,
     245,    22,    23,    47,    71,    70,    73,    75,    74,    72,
     217,   218,   246,   247,   680,   418,   248,   249,   420,   986,
    1293,   224,   225,   226,   404,   250,   251,   109,   314,   110,
     295,   296,   482,   483,  1006,  1007,   773,   521,   522,   523,
     524,   771,  1050,  1051,  1456,  1054,  1055,  1283,  1459,  1594,
    1595,   736,   737,   252,   253,   738,  1254,  1255,  1256,   254,
     409,   255,   688,   410,   411,   412,  1216,  1217,   111,   112,
    1061,   526,   527,   528,   786,  1287,  1288,   789,   790,   799,
     791,  1474,  1475,   739,   113,  1063,  1291,  1422,  1423,  1424,
    1425,  1426,  1427,  1428,  1429,  1430,  1431,  1432,  1433,  1434,
    1435,  1469,   114,   529,   316,   531,   532,   115,   726,   496,
     497,   298,   299,   740,   300,   301,   489,   490,  1010,   741,
    1016,  1250,   742,   743,   116,   117,  1068,   797,  1294,  1604,
     118,   275,  1224,   701,   702,   119,   120,  1069,   289,   800,
     801,   802,   803,    56,   122,   805,   538,   539,  1073,  1074,
     123,  1231,   997,   998,   124,   278,   279,   462,  1225,   704,
     125,   291,  1234,   479,   804,   498,  1003,  1579,   720,   721,
    1232,   256,   542,   127,   865,  1143,  1144,   632,   907,   908,
    1647,   905,   128,   517,   129,   326,   130,   504,   492,   131,
     132,   133,   756,   757,  1036,   758,   178,   589,  1530,  1116,
    1349,  1350,  1648,  1527,   868,   869,   870,  1118,  1353,  1354,
    1355,  1356,  1078,   179,   609,  1541,   919,  1156,  1367,  1368,
     257,   258,   259,   260,   261,   428,   431,   262,   263,   450,
     264,   451,   265,   266,   267,   268,   269,   453,   455,   458,
     270,  1119,  1120,   271,   518,   357,  1437,  1222,   367,   368,
     369,   370,   180,   181,   323,   550,   551,   552,   553,  1259,
     545,   546,  1260,   182,   183,   387,   647,   950,   184,   648,
     649,   584,   951,  1187,  1188,   711,   327,   328,   185,   137,
     138,   564,   139,   283,   468,   329,   565,   566,   140,   141,
     567,   834,   142,   568,   569,  1098,   346,   186,   187,   573,
     574,   854,   855,   144,   575,   856,  1105,   188,   189,   389,
     390,   190,  1542,  1114,   391,   655,   956,  1196,   652,   952,
    1192,  1193,  1194,   191,   192,   193,   194,   195,   371,   633,
     634,   635,   196,   590,  1359,   884,   885,  1121,   909,   197,
     930,  1171,  1172,   198,  1179,  1386,   199,  1175,  1383,   200,
     636,   637,   638,   639,  1180,  1181,  1039,  1040,  1041,  1273,
    1274,  1586,   201,   606,   607,   202,   598,   599,   203,  1363,
    1652,   355,   899,   900,   380,    24,   334,   155,    25,    69,
     581,  1525,  1111,  1345,   156,   335,   336,   337,    57,   332,
      58,  1343,  1701,   578,  1638,    26,    59,    27,    68,   615,
     616,  1543,  1161,  1372,   859,  1109,  1341,  1639,  1640,  1641,
    1642,   533,   148,   286,   287,   149,   474,   475,   273,   699,
     204,   393,   957,   658,  1402,   205,   642,   274,   962,   963,
     964,    28,    29,    30,    31,    32,   662,  1560,   210,   967,
    1405,  1406,   664,  1663,  1206,    33,    34,   661,   208,   666,
    1561,  1208,   395,   660,   968,   969,   970,   206,   157,   579,
     352,  1094,  1605,   611
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      46,   134,   366,   491,   401,   407,   705,   494,   646,   645,
     106,  1162,   746,   480,   153,   493,   365,   272,   108,   338,
     959,   706,   727,   108,   339,   484,  1011,   331,   108,   108,
     154,  1113,   330,   403,   915,   315,   918,   556,  1247,  1249,
     293,   101,  1182,  1182,   817,   488,   101,   366,  1226,   134,
     419,   101,   101,   121,  1176,   358,   360,   146,   106,   941,
     108,   959,   242,   145,   733,   904,   108,  1122,  1123,  1124,
     108,  1272,   433,   435,   914,  1130,  1342,   512,  1138,  1139,
    1465,   981,  1189,   101,  1470,  1394,   853,  1476,   728,   101,
     424,   417,   317,   101,   548,   280,  -182,  -195,    50,   940,
     965,   121,   379,  1461,  -525,   146,   672,   812,  1460,   463,
     814,   145,   432,   650,   471,  1339,     4,   294,  -195,   421,
     495,   715,   525,    35,   495,   762,   910,   560,  1347,   807,
       4,   945,   576,   349,    37,     4,  1031,  1275,   108,  1284,
     434,  -412,     4,   916,  -732,  -413,     4,  1364,   920,   561,
       4,   942,     4,   511,   641,   501,   502,     8,     4,  -195,
       4,   101,    38,   388,   272,   942,   419,  1462,   707,   419,
     651,     8,   708,   989,  1032,  1133,     8,  1348,  1149,  1334,
    1200,  1210,  -412,     8,   669,   833,  -413,     8,  1385,  1212,
    1200,     8,  1236,     8,  1300,   215,  1365,  1643,   338,     8,
    1472,     8,   495,   339,  1380,   685,  1395,   417,  1414,     4,
     417,  1272,  1446,   920,  1599,   503,  1531,  1712,   419,   769,
     284,    52,  1592,     4,   135,  1280,   416,  1017,   511,   511,
     511,   683,   435,  1374,  1021,   421,  1347,   520,   421,   689,
     230,   231,   689,   537,   597,   435,   985,   605,   506,     4,
       8,  1034,   653,  1574,   519,   379,   787,   285,   477,   417,
       4,   999,   478,   549,     8,   222,   519,  1015,   467,   230,
     231,  1313,   135,  -266,   656,   966,    51,  1461,  -339,   509,
    -476,  1218,  1460,   101,  1340,  1348,   425,   421,   577,   305,
       8,   108,  1557,  -195,    15,   866,  -195,   534,   535,   415,
    -525,     8,   673,   813,   379,  -525,   815,   867,    15,  1454,
     734,   732,  1132,    15,   101,   866,     4,  1013,  1393,  -412,
      15,  1357,   911,  -413,    15,  1360,  1147,   867,    15,   305,
      15,    45,   580,   644,  1685,   760,    15,   305,    15,   917,
     108,  1462,   416,   272,   921,   416,  1473,   943,   610,  1128,
     108,   731,  1131,    45,   509,   509,   509,     8,   643,  1137,
     436,   944,  1140,   101,   272,   272,  1146,  1104,  1593,   990,
    1033,  1134,   216,   101,  1150,   770,  1201,  1211,  1643,   745,
    1253,  1281,     4,   321,   981,  1213,  1220,    15,  1237,  -542,
    1301,  1461,   934,  1674,   416,   663,   665,  1688,     4,  1574,
    1381,    15,  1396,   136,  1415,  1184,  1035,  -542,  1447,  1517,
     242,  1461,  1532,  1713,   238,     4,    76,    45,   239,    95,
      96,   347,    98,     8,   151,   108,    45,    15,   558,     4,
     322,   491,   953,    77,  -766,   494,    45,   536,    15,     8,
     305,   207,   223,   493,   313,   520,  -541,   239,   703,   749,
     751,   136,   977,   484,     4,   975,     8,   847,   857,   978,
     979,   108,   108,   735,     4,     4,    14,    45,   511,   297,
       8,  1566,  1536,   488,   143,  -766,   926,   617,   883,   898,
     901,   927,   147,  1182,   101,   101,   730,  1272,  1056,   912,
     755,  1645,   734,   433,    15,     8,  1600,  1601,   366,   209,
    1052,     4,   366,   366,   433,     8,     8,   822,    78,   832,
     437,   438,   439,   440,   441,   442,   443,   108,   828,   436,
     150,   798,   143,   158,   787,   850,   108,   746,  1235,  -236,
     147,   746,   692,   984,   829,   705,  1526,    67,    92,  1364,
     101,   154,     8,   101,   432,  -542,  1308,  1576,   305,   101,
     706,  -197,    79,  1526,  1088,  1023,  1022,   108,   108,   277,
      15,   745,   959,   100,   902,   842,   108,   108,   150,   290,
    1546,   837,   434,   913,  1304,   703,    15,   288,   837,  1246,
     101,   101,  1366,   733,  1080,   613,  1082,  1101,  1365,   101,
     101,   108,   108,    15,   509,   703,   937,   938,   939,  1379,
    1162,  1337,    92,   288,     4,    92,   108,    15,   465,   466,
     486,   272,  -766,   305,   101,   101,  1686,  1687,   304,    95,
      96,   233,    98,   230,   231,    14,    45,   100,   947,   101,
     100,    92,    15,   307,    45,   313,  1389,  1391,   308,  1378,
    -466,   309,    15,    15,   958,     8,   318,   487,    95,    96,
    1113,    98,   501,   502,    50,    45,   100,    92,   319, -1027,
    1670, -1027,   429,   430,   444,   445,   446,   447,   982,    92,
     310,  1671,   237,  1552,    95,    96, -1028,    98, -1028,    15,
    1670,    45,   100,    45,  -940,   958,  1672,  1576,    45,   499,
     108,  1671,   -89,  1576,   100,   -89,  1019,   707,   448,   449,
     233,   708,   -90,   996,  1227,   -90,  1672,  -236,   500,   519,
     495,  1008,   503,   101,   360,  1020,   350,  1014,     9,    10,
      11,    12,   351,  -541,  1029,  -510,  1491,  1492,   690,  -510,
     537,   691,   -61,   233,   320,     9,    10,    11,    12,  1106,
    1162,  1099,   324,    85,    86,  1046,  1047,   559,   560,    63,
     325,   237,  1079,   549,  1584,   549,  1420,  1053,   477,   333,
      95,    96,   478,    98,   340,  1127,    63,    45,   230,   231,
     561,  1466,  1467,  1468,   999,  1048,  1635,   341,   -97,   -97,
    1141,   -97,    15,  1049,   237,   -97,   926,   511,   -97,   239,
     402,   927,   108,    95,    96,   238,    98,   238,   597,   239,
      45,   239,   372,   -93,   605,   373,   -93,    14,  1253,   644,
     646,   645,   414,   456,   457,   101,  1125,  -137,  1170,   -92,
     353,   866,  1484,   354,    14,   419,   108,   361,   415,   734,
     732,    60,    51,   867,   643,    61,    65,    62,    66,    95,
      96,   108,    98,   362,   108,   108,    45,   419,   297,   101,
    1129,   108,   108,   108,   108,   572,   108,  1103,   108,   108,
     108,   866,   388,   760,   101,  1145,  1245,   101,   101,  1407,
     731,  1408,  1173,   867,   101,   101,   101,   101,   233,   101,
     392,   101,   101,   101,  1163,  1488,    85,    86,   417,  1496,
      95,    96,   108,    98,   745,    14,  -276,    45,   745,  1177,
    1164,   678,   679,   419,    14,  -277,  1157,   108,   108, -1027,
     394, -1027,   396,   509,   399,   101,   421,   572,   852,   235,
     236,  1185,  1186,  1693,  1694,  -100,  -100,   414,  -100,   237,
     101,   101,  -100,   519,   415,  -100,   211,   212,    95,    96,
     423,    98,   238,  -223,   417,    45,   239,  1239,   219,   220,
     221,   427,   227,   452,   108,   272,   307,  1439,   705,   397,
     398,   308,   563,   570,   381,   454,  1215,   460,     9,    10,
      11,    12,   421,   706,   382,   461,   383,   703,   285,   384,
     385,   605,   735,    95,    96,   746,    98,   473,   476,   485,
     311,   515,   530,   310,    39,    40,    41,    42,   386,    63,
     242,  1240,    43,   516,   242,   730,    95,    96,    44,    98,
      45,  1052,   228,    45,   646,   645,   213,   214,   755,   229,
     554,   883,   557,   416,   559,   572,  1442,   230,   231,   232,
     591,   276,  1489,    45,  1306,   281,  1489,   898,   619,  1494,
      96,   292,    98,   657,   667,   668,    45,   108,   670,   734,
     433,   671,  1163,   676,  1376,   677,   798,    14,   549,   684,
     108,  -234,  -229,   682,   687,  -224,   698,  -951,   829,   366,
     101,   228,   700,   710,   712,   717,   722,   724,   229,  1328,
     725,   610,   405,   101,   718,   719,   230,   231,   232,   764,
    1444,   765,   307,   767,   768,   772,   788,   308,   792,   108,
    1373,   703,   793,   403,   795,   746,   406,   796,   806,   348,
     382,   809,   383,   811,   819,   384,   385,   823,   745,   363,
     707, -1027,   101, -1027,   708,   824,   863,   108,   851,   310,
     858,   108,   108,   860,   386,   644,   862,   233,  1377,   864,
     922,   703,   928,    39,    40,    41,    42,   931,   935,   929,
     101,    43,   108,   949,   101,   101,   955,    44,   -99,   -99,
     643,   -99,   234,   976,   973,   -99,   974,   980,   -99,  -230,
     443,   988,   991,  1436,  1002,   101,   992,  1000,   235,   236,
       9,    10,    11,    12,  1001,  1399,   958,  1014,   237,  1005,
    1018,  1015,  1042,  -449,  1024,  1058,   233,    95,    96,  1059,
      98,   238,  1060,  1062,    45,   239,   240,  1066,  1067,  1070,
    1072,    63,  1076,  1084,   241,  1087,   996,  1090,  1095,   310,
    1107,   234,  1108,  1436,  1110,  1443,   464,  1115,  1436,  1117,
     308,   469,  1436,  1096,   472,  1436,  1450,   235,   236,  1126,
    1136,   481,  1155,   382,  1079,   383,  1160,   237,   384,   385,
    1199,  1203,  1205,  1202,   999,   505,    95,    96,   514,    98,
     238,  1207,   310,    45,   239,   240,  1229,   386,  1170,    14,
    1053,  1204,  1238,   241,  1230,  1209,  1262,  1214,  1219,  -952,
    1228,  1264,  1736,  1292,  1278,  1279,  1282,  1295,  1299,   108,
    1344,  1302,  1303,   108,   586,   587,  1079,   588,  1371,  1335,
       5, -1027,     6, -1027,  1375,  1352,   600,  1361,   608,     7,
     108,  1369,   101,  1275,  1370,  1385,   101,  1397,  1388,   612,
    1404,   614,  1411,  1409,   126,  1410,  1412,   108,  1416,   126,
    1445,  -524,  1452,   101,   126,   126,  1453,  1455,  1477,   644,
    1478,   659,  1482,  1487,  1485,  1545,  1666,  1490,   108,  1493,
     101,  1665,  1519,  1565,  1521,  1518,   108,  1523,  1522,  1529,
       9,    10,    11,    12,   643,  1528,   302,  1534,  1681,  1539,
    1540,   101,   126,  1436,  1559,  1563,   302,   108,  1564,   101,
    1567,  1568,  1569,  1570,  1589,  1571,  1572,  1573,  1578,  1577,
    -382,    13,   752,  1580,   366,  1590,  1596,  1436,  1591,   229,
     703,  1597,  1598,  1603,  1659,  1637,  -255,   230,   231,   753,
    1644,   366,  1658,  1340,  1661,   233,  1667,  1678,   713,  1669,
    1458,  1677,   716,  1682,  1702,  1683,  1684,  1705,   644,  1489,
    1707,  1710,   242,   723,  1711,  1708,   108,  1715,  1716,  1215,
     234,   230,   231,  1728,   126,   108,  1722,  1731,  1723,    14,
     748,   750,  1724,   643,   761,  1734,   235,   236,  1735,  1738,
     763,  1741,  1744,    81,  1746,  1689,   237,  1742,   101,   306,
    1159,  1243,   549,  1163,    80,    95,    96,   794,    98,   238,
     681,   108,    45,   239,   240,  1244,   808,    53,  1471,   829,
    1463,   108,   241,  1004,  1440,   810,   774,  1588,    48,  1457,
    1730,  1248,   108,   818,   101,   272,   686,  1464,   821,  1582,
    1581,  1436,  1436,  1436,   101,   674,   426,   233,  1064,  1436,
    1289,  1675,  1575,  1676,   729,   101,  1679,   861,  1065,  1261,
    1009,  1436,  1436,  1251,  1223,  1075,     2,     3,  1298,  1479,
    1729,  1418,   234,   366, -1027,  1436, -1027,   754,  1680,   709,
    1233,   233,  1148,   820,  1267,  1027,   108,   830,   235,   236,
       4,  1649,   229,  1653,     5, -1027,     6, -1027,   237,  1351,
     230,   231,  1538,     7,  1651,  1533,   234,    95,    96,  1654,
      98,   238,   693,   459,    45,   239,   240,   831,   694,   946,
     541,  1690,   235,   236,   241,  1045,   696,   126,  1081,   695,
     954,     8,   237,     9,    10,    11,    12,   835,  1709,   971,
    1077,    95,    96,  1163,    98,   238,   697,  1083,    45,   239,
     240,   108,   547,  1714,     9,    10,    11,    12,   241,  1164,
    1704,   848,   571,  1556,    63,   654,   849,   836,  1358,   933,
    1135,  1547,   640,   838,   101,   936,   126,  1183,  1174,  1550,
    1153,  1190,   541,   541,  1151,    13,   302,  1585,   229,   839,
     840,   841,   582,  1362,   152,   714,   230,   231,   995,  1737,
    1726,   470,  1739,   843,  1012,   948,  1401,  1400,    64,   844,
     233,   987,  1562,   845,   846,  1747,   108,  1403,   972,  1332,
       0,  1268,    14,     0,     0,     0,   996,  1025,   229,  1026,
       0,     0,  -138,     0,     0,   234,   230,   231,     0,   101,
       0,     0,     0,    14,  1044,     0,  -202,  -202,  -202,  -202,
    1057,   235,   236,     0,  -202,     0,     0,     0,     0,     0,
    -202,   237,     0,     0,     0,     0,     0,     0,    15,     0,
      95,    96,     0,    98,   238,     0,     0,    45,   239,   240,
       0,     0,     0,  1071,     0,     0,     0,   241,     0,     0,
       0,     0,     0,     0,  1421,  1085,     0,   302,   302,   744,
    1086,   229,  1089,     0,     0,     0,   233,     0,     0,   230,
     231,  1497,  1498,  1499,  1500,  1501,  1502,     0,  1503,  1504,
    1505,  1506,  1507,  1508,     0,  1509,  1510,  1511,  1512,  1513,
    1102,   234,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1112,     0,     0,     0,     0,   233,   235,   236,     0,
       0,     0,     0,   302,     0,     0,     0,   237,     0,     0,
       0,     0,   302,     0,     0,     0,    95,    96,     0,    98,
     238,   234,     0,    45,   239,   240,     0,     0,     0,     0,
       0,     0,     0,   241,  1626,     0,     0,   235,   236,  1152,
     229,     0,     0,   302,   302,  1154,     0,   237,   230,   231,
       0,  1627,   302,   302,     0,     0,    95,    96,     0,    98,
     238,     0,     0,    45,   239,   240,     0,     0,     0,   233,
       0,   541,     0,   241,     0,     0,     0,   302,   302,     0,
       0,  1195,     0,  1458,  1197,  1198,     0,     0,     0,     0,
     229,     0,   302,   775,   234,     0,     0,     0,   230,   231,
       0,     0,     0,     0,     0,     0,     0,   541,  1609,     0,
     235,   236,     0,     0,   229,     0,     0,     0,     0,     0,
     237,     0,   230,   231,     0,     0,     0,     0,     0,    95,
      96,     0,    98,   238,     0,     0,    45,   239,   240,     0,
       0,     0,     0,     0,     0,     0,   241,     0,   541,     0,
       0,     0,     0,     0,     0,   684,  1514,  1515,   233,     0,
       0,     0,     0,     0,     0,     0,     0,  1276,     0,     0,
    1277,    85,    86,   776,   777,     0,   302,     0,     0,     0,
       0,     0,  1285,   234,  1286,     0,     0,   345,     0,  1290,
       0,     0,     0,     0,     0,     0,  1296,  1297,   364,   235,
     236,     0,     0,   778,     0,   779,   780,   781,   233,   237,
     782,   783,     0,   784,   785,  1305,  1307,     0,    95,    96,
       0,    98,   238,     0,     0,    45,   239,   240,     0,  1336,
       0,  1338,   233,   234,     0,   241,     0,     0,     0,     0,
       0,     0,  1346,     0,     0,     0,     0,     0,     0,   235,
     236,     0,     0,     0,     0,     0,     0,   234,     0,   237,
       0,   413,     0,     0,     0,     0,     0,   422,    95,    96,
       0,    98,   238,   235,   236,    45,   239,   240,   302,     0,
       0,     0,   413,   237,     0,   241,     0,     0,     0,     0,
    1673,     0,    95,    96,     0,    98,   238,   229,     0,    45,
     239,   240,     0,  1382,     0,   230,   231,  1384,     0,   241,
       0,  1387,   302,  1390,  1392,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1398,     0,     0,   302,     0,     0,
     302,   302,     0,     0,     0,   510,   513,   302,   302,   302,
     302,     0,   302,   303,   302,   302,   302,     0,   544,     0,
       0,   555,  1413,   312,     0,     0,  1717,     0,     0,  1419,
       0,     0,  1438,   229,     0,  1441,     0,   583,   585,     0,
       0,   230,   231,     0,     0,     0,     0,     0,   126,     0,
       0,     0,     0,   596,     0,  1732,   596,     0,     0,     0,
    1448,   541,   229,   302,   302,     0,     0,     0,     0,     0,
     230,   231,     0,     0,     0,     0,     0,     0,   618,     0,
     364,   364,   510,     0,     0,   233,     0,     0,     0,     0,
       0,     0,     0,  1631,     0,     0,     0,     0,     0,   229,
       0,     0,  1480,     0,  1481,   675,     0,   230,   231,     0,
     234,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   235,   236,     0,     0,
       0,     0,     0,     0,   744,  1520,   237,   541,   744,     0,
       0,  1524,     0,     0,     0,    95,    96,     0,    98,   238,
       0,   233,    45,   239,   240,     0,     0,  1535,     0,     0,
       0,  1537,   241,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   234,     0,     0,  1548,
     233,  1549,     0,     0,  1551,     0,  1553,  1554,     0,  1555,
       0,     0,   235,   236,     0,     0,     0,     0,   747,     0,
    1558,   759,   237,   302,     0,   234,     0,     0,     0,     0,
       0,    95,    96,   766,    98,   238,   126,   233,    45,   239,
     240,   235,   236,     0,     0,     0,     0,     0,   241,     0,
       0,   237,     0,     0,     0,     0,   825,   543,     0,     0,
      95,    96,   234,    98,   238,     0,     0,    45,   239,   240,
       0,     0,     0,  1587,     0,   302,     0,   241,   235,   236,
       0,     0,     0,     0,     0,     0,     0,     0,   237,     0,
       0,     0,     0,     0,     0,     0,     0,    95,    96,     0,
      98,   238,     0,   302,    45,   239,   240,   302,   302,  1633,
       0,     0,     0,     0,   241,   229,     0,     0,     0,   631,
     631,     0,     0,   230,   231,     0,     0,     0,   302,     0,
       0,   162,     0,     0,     0,     0,  -197,  1636,   541,   541,
     510,   164,   165,  1646,   166,     0,     0,   167,  1650,     0,
     826,   169,   827,     0,     0,     0,     0,     0,     0,  1655,
    1656,  1657,     0,     0,     0,     0,     0,     0,     0,     0,
    1660,     0,     0,     0,     0,   961,     0,     0,  1662,     0,
    1664,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      92,   229,     0,     0,   413,     0,     0,     0,     0,   230,
     231,   620,     0,     0,     0,     0,     0,    95,    96,     0,
      98,     0,     0,     0,    45,   100,   961,     0,     0,     0,
       0,     0,     0,   229,     0,     0,     0,   993,     0,   994,
     621,   230,   231,   233,   303,   303,  -765,     0,   622,     0,
       0,   623,   624,   625,   626,     0,   627,     0,   628,   629,
       0,     0,     0,  1700,     0,     0,     0,     0,   234,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1028,
    1703,  1030,     0,     0,   235,   236,     0,  -765,     0,  1038,
       0,  1706,  1043,     0,   237,     0,   302,     0,     0,     0,
     816,     0,     0,    95,    96,     0,    98,   238,     0,     0,
      45,   239,   240,   302,     0,     0,     0,     0,     0,   233,
     241,     0,     0,     0,     0,     0,     0,     0,     0,  1725,
       0,     0,  1727,     0,   302,     0,     0,     0,     0,     0,
     903,   906,   302,     0,   234,     0,     0,     0,     0,   903,
     906,   233,     0,     0,     0,     0,     0,     0,     0,     0,
     235,   236,    92,     0,     0,     0,     0,     0,   631,     0,
     237,     0,     0,     0,   903,   906,   234,     0,  1100,    95,
      96,     0,    98,   238,     0,     0,   630,   508,   240,   312,
       0,     0,   235,   236,   541,   541,   241,     0,   510,   510,
     510,   510,   237,     0,   960,     0,     0,   510,     0,     0,
       0,    95,    96,   510,    98,   238,     0,     0,   342,   239,
     240,     0,  1602,     0,  -765,     0,     0,     0,   241,     0,
       0,   302,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   960,     0,     0,     0,  1158,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   126,     0,   364,
    1038,     0,     0,   303,     0,     0,   583,   302,     0,  1191,
       0,     0,     0,     0,     0,  -225,  -225,  -225,   302,  -225,
       0,  -225,  -225,  -225,  -225,  -225,  -225,  -225,  -225,  -225,
    -225,  -225,  -225,  -225,     0,     0,     0,   413,     0,     0,
       0,     0,     0,     0,   229,  1221,     0,     0,     0,     0,
       0,     0,   230,   231,     0,     0,     0,     4,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1602,   229,  -225,  1258,     0,     0,     0,     0,
       0,   230,   231,     0,  1263,     0,     0,     0,  1266,   759,
       0,  1269,  1270,     0,  1271,     0,     0,     0,     8,     0,
       0,     0,     0,     0,     0,   312,     0,     0,     0,   -88,
     -88,   -88,     0,   -88,     0,   -88,   -88,   -88,   -88,   -88,
     -88,   -88,   -88,   -88,   -88,   -88,   -88,   -88,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   126,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   583,     0,     0,
    1331,     0,     0,  1333,   903,     0,     0,   903,   906,     0,
     400,     0,   233,     0,   903,   906,   906,   903,   -88,  1142,
       0,   903,   906,  1142,     0,  -225,  -225,  -225,  -225,     0,
       0,     0,     0,   510,     0,     0,     0,   234,     0,     0,
       0,   233,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   235,   236,     0,  -225,     0,     0,  -225,
    -225,  -225,     0,   237,   415,     0,   234,     0,   631,     0,
    1178,  1178,    95,    96,     0,    98,   238,     0,     0,    45,
     239,   240,   235,   236,  1038,    15,     0,     0,     0,   241,
       0,     0,   237,     0,     0,     0,     0,  1191,     0,     0,
       0,    95,    96,     0,    98,   238,   961,   961,    45,   239,
     240,     0,     0,     0,     0,     0,     0,     0,   241,   -88,
     -88,   -88,   -88,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1417,     0,     0,     0,
       0,     0,     0,     0,  1257,     0,     0,     0,     0,     0,
     -88,   -88,     0,   -88,   -88,   -88,     0,   -88,     0,     0,
    1309,     0,     0,     0,     0,     0,     0,     0,  1310,  1311,
       0,  1449,     0,     0,     0,  1451,     0,     0,     0,     0,
       0,     0,  1038,     0,     0,     0,     0,     0,     0,   229,
       0,     0,     0,  1312,     0,     0,     0,   230,   231,     0,
       0,     0,     0,     0,     0,     0,     0,   601,     0,     0,
     816,     0,     0,     0,     0,   602,  1483,   603,   604,     0,
       0,     0,     0,     0,     0,     0,     0,   871,     0,     0,
    1486,   872,   873,   874,   875,   876,   877,   878,     0,     0,
       0,     0,     0,     0,   879,   880,   881,   882,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   906,  1313,     0,     0,     0,     0,  1038,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   233,     0,
    1544,     0,   510,     0,     0,     0,     0,     0,     0,     0,
     312,     0,     0,     0,   903,   906,     0,     0,     0,     0,
       0,     0,  1191,   234,  1191,     0,   229,   233,     0,  1314,
       0,     0,     0,     0,   230,   231,   507,     0,     0,  1315,
    1316,     0,     0,     0,     0,   960,   960,     0,     0,   237,
       0,     0,   234,     0,     0,     0,     0,     0,  1317,  1318,
       0,  1319,  1320,     0,     0,    45,  1321,   240,   235,   236,
       0,     0,  1258,  1258,     0,   241,     0,     0,   237,  1583,
    1038,     0,     0,     0,     0,     0,     0,    95,    96,     0,
      98,   238,     0,     0,    45,   239,   240,     0,     0,  1221,
       0,     0,     0,     0,   241,     0,     0,     0,   229,     0,
       0,     0,     0,     0,     0,     0,   230,   231,     0,     0,
       0,     0,     0,     0,  1611,  1612,  1613,  1614,  1615,  1616,
    1617,  1618,  1619,  1620,  1621,  1622,  1623,  1624,  1625,  1629,
    1630,  1632,  1634,     0,   233,     0,   886,     0,   887,   888,
     889,   890,     0,   891,     0,   892,     0,   893,     0,     0,
       0,     0,     0,   894,     0,   895,   896,   897,     0,   234,
     229,     0,     0,     0,     0,     0,     0,     0,   230,   231,
       0,     0,     0,     0,     0,   235,   236,    92,     0,     0,
       0,     0,     0,   906,  1668,   237,     0,     0,     0,     0,
       0,     0,     0,     0,    95,    96,     0,    98,   238,     0,
     906,    45,   508,   240,     0,     0,  1165,  1166,     0,  1221,
       0,   241,     0,     0,     0,  1167,   233,  1168,  1169,     0,
       0,  1178,     0,     0,     0,     0,  1695,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   234,     0,  1696,  1697,     0,     0,     0,  1698,     0,
    1699,     0,   229,     0,     0,     0,     0,   235,   236,     0,
     230,   231,     0,     0,     0,     0,     0,   237,     0,     0,
       0,     0,     0,     0,     0,     0,    95,    96,   233,    98,
     238,  1257,  1257,    45,   239,   240,     0,     0,     0,     0,
     886,     0,   887,   241,   889,   890,     0,   891,     0,   892,
       0,   893,     0,   234,     0,     0,     0,   894,  1721,   895,
     896,   897,     0,     0,   229,     0,     0,     0,     0,   235,
     236,     0,   230,   231,     0,     0,     0,     0,     0,   237,
       0,     0,     0,     0,  1733,     0,     0,     0,    95,    96,
       0,    98,   238,     0,     0,    45,   239,   240,   229,  1740,
       0,  1721,  1743,     0,     0,   241,   230,   231,  1745,   229,
       0,   592,     0,  1748,   903,     0,     0,   230,   231,   593,
     233,   594,   595,     0,     0,   903,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   229,     0,
       0,     0,     0,     0,     0,   234,   230,   231,   932,     0,
       0,     0,     0,     0,     0,     0,   601,     0,     0,     0,
       0,   235,   236,     0,   602,     0,   603,   604,     0,     0,
       0,   237,     0,     0,     0,     0,     0,     0,     0,     0,
      95,    96,   233,    98,   238,   229,     0,    45,   239,   240,
       0,     0,     0,   230,   231,  1265,   229,   241,     0,     0,
       0,     0,     0,     0,   230,   231,  -395,   234,     0,     0,
       0,     0,     0,     0,     0,     0,   233,     0,     0,     0,
       0,     0,     0,   235,   236,     0,     0,   233,     0,     0,
     229,     0,     0,   237,     0,     0,     0,     0,   230,   231,
       0,   234,    95,    96,     0,    98,   238,     0,     0,    45,
     239,   240,   234,     0,     0,     0,   233,   235,   236,   241,
       0,     0,     0,     0,     0,     0,     0,   237,   235,   236,
       0,     0,     0,     0,     0,     0,    95,    96,   237,    98,
     238,   234,     0,   540,   239,   240,  1252,    95,    96,     0,
      98,   238,   229,   241,    45,   239,   240,   235,   236,     0,
     230,   231,     0,   233,   241,     0,     0,   237,     0,     0,
       0,     0,     0,     0,   233,     0,    95,    96,     0,    98,
     238,   229,     0,    45,   239,   240,     0,     0,   234,   230,
     231,     0,   229,   241,     0,     0,     0,     0,     0,   234,
     230,   231,     0,     0,   235,   236,     0,     0,   233,     0,
       0,     0,     0,     0,   237,   235,   236,     0,     0,     0,
       0,     0,     0,    95,    96,   237,    98,   238,     0,     0,
      45,   239,   240,   234,    95,    96,     0,    98,   238,   229,
     241,    45,   239,   240,     0,     0,     0,   230,   231,   235,
     236,   241,     0,     0,   229,     0,     0,     0,     0,   237,
       0,     0,   230,   231,     0,     0,     0,     0,    95,    96,
     233,    98,   238,     0,     0,    45,   239,   356,     0,     0,
       0,     0,     0,     0,   229,   241,     0,     0,     0,     0,
       0,     0,   230,   231,     0,   234,     0,     0,     0,   233,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     233,   235,   236,     0,     0,     0,     0,     0,     0,     0,
       0,   237,     0,     0,   234,     0,     0,     0,     0,     0,
      95,    96,     0,    98,   238,   234,     0,    45,   239,   359,
     235,   236,     0,     0,     0,     0,     0,   241,     0,     0,
     237,   235,   236,     0,     0,     0,     0,   233,     0,    95,
      96,   237,    98,   238,     0,     0,    45,   239,   408,     0,
      95,    96,   233,    98,   238,     0,   241,    45,   239,   240,
       0,     0,   234,     0,     0,     0,     0,   241,     0,    82,
       0,     0,     0,     0,     0,     0,    83,   234,   235,   236,
       0,     0,   233,     0,     0,     0,     0,     0,   237,     0,
       0,     0,     0,   235,   236,     0,     0,    95,    96,     0,
      98,   238,     0,   237,   540,   239,   240,   234,     0,     0,
       0,     0,    95,    96,   241,    98,   238,     0,     0,    45,
     239,  1037,     0,   235,   236,     0,     0,    84,     0,   241,
       0,     0,     0,   237,     0,     0,     0,     0,     0,     0,
       0,     0,    95,    96,     0,    98,   238,    85,    86,  1091,
    1092,  1093,     0,     0,     0,     0,     0,     0,     0,   241,
      87,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -450,
       0,    88,    83,    89,    90,     0,     0,     0,   158,  -463,
       0,  -476,     0,     0,     0,     0,    91,  -708,   159,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    92,     0,     0,    93,
      94,  -334,     0,     0,  -334,  -334,  -334,  -334,     0,   160,
       0,     0,  -334,    95,    96,    97,    98,     0,  -334,     0,
      99,   100,     0,     0,     0,     0,     0,   161,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -708,
    -708,  -708,     0,   162,     0,     0,    87,     0,  -708,    83,
     163,     0,     0,   164,   165,   158,   166,     0,     0,   167,
       0,     0,   168,   169,   170,   159,  -708,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    83,     0,     0,
       0,     0,     0,   158,     0,     0,     0,     0,   171,     0,
       0,     0,     0,   159,     0,     0,   160,     0,     0,     0,
       0,     0,    92,  -708,  -708,    93,     0,     0,     0,     0,
       0,     0,     0,     0,   161,     0,     0,     0,     0,    95,
      96,     0,    98,     0,   160,     0,   172,   100,     0,     0,
     162,     0,     0,    87,  -128,     0,     0,   163,     0,     0,
     164,   165,   161,   166,     0,     0,   167,     0,     0,   168,
     169,   170,     0,     0,     0,     0,     0,     0,   162,     0,
       0,    87,  -129,     0,     0,   163,     0,     0,   164,   165,
     158,   166,     0,     0,   167,   171,     0,   168,   169,   170,
     159,     0,     0,     0,     0,     0,     0,     0,     0,    92,
       0,     0,    93,     0,     0,     0,     0,     0,   825,     0,
       0,     0,     0,   171,  -128,     0,    95,    96,     0,    98,
       0,   160,     0,   172,   100,     0,     0,    92,     0,     0,
      93,     0,     0,     0,     0,     0,     0,     0,     0,   161,
       0,     0,  -129,     0,    95,    96,     0,    98,     0,   160,
       0,   172,   100,     0,     0,   162,     0,     0,    87,  -125,
       0,     0,   163,     0,     0,   164,   165,   161,   166,     0,
       0,   167,     0,     0,   168,   169,   170,     0,  1042,     0,
       0,     0,     0,   162,     0,     0,   825,     0,     0,     0,
     163,     0,     0,   164,   165,     0,   166,     0,     0,   167,
     171,     0,   168,   169,   170,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    92,     0,     0,    93,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   160,   171,  -125,
       0,    95,    96,     0,    98,     0,     0,     0,   172,   100,
       0,     0,    92,     0,     0,   161,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    95,
      96,   162,    98,     0,     0,     0,    45,   100,   163,     0,
       0,   164,   165,     0,   166,     0,     0,   167,     0,     0,
     168,   169,   170,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   171,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      92,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    95,    96,     0,
      98,     0,     0,     0,    45,   100
};

static const yytype_int16 yycheck[] =
{
      14,    55,   171,   297,   216,   223,   462,   297,   387,   387,
      55,   929,   500,   291,    59,   297,   171,    76,    55,   157,
     658,   462,   497,    60,   157,   294,   725,   144,    65,    66,
      59,   863,   139,   222,   604,   112,   609,   325,  1015,  1015,
      93,    55,   944,   945,   554,   297,    60,   216,   991,   103,
     240,    65,    66,    55,   943,   167,   168,    55,   103,   629,
      97,   699,    76,    55,   500,   594,   103,   872,   873,   874,
     107,  1037,   261,   262,   603,   880,  1108,   308,   889,   890,
    1287,   677,   951,    97,  1291,  1190,   573,  1294,   498,   103,
       3,   240,   113,   107,   322,     3,     5,     3,    24,   628,
      21,   103,   175,  1282,     3,   103,     3,     3,  1282,     3,
       3,   103,   261,   141,     3,    27,    24,     3,    24,   240,
       3,     3,     3,   189,     3,     3,     3,   174,   101,     3,
      24,     3,     3,   162,   196,    24,     5,     3,   175,     3,
     261,    24,    24,     3,     5,    24,    24,   101,     3,   196,
      24,     3,    24,   308,   385,   115,   116,    65,    24,    65,
      24,   175,   196,    47,   223,     3,   356,  1282,   462,   359,
     198,    65,   462,     3,     3,     3,    65,   150,     3,  1097,
       3,     3,    65,    65,   402,   563,    65,    65,     3,     3,
       3,    65,     3,    65,     3,    21,   150,  1521,   336,    65,
       5,    65,     3,   336,     3,   423,     3,   356,     3,    24,
     359,  1177,     3,     3,  1472,   175,     3,     3,   408,     3,
     159,    24,     5,    24,    55,     3,   240,   735,   383,   384,
     385,   421,   421,     3,   742,   356,   101,   314,   359,   428,
      19,    20,   431,   320,   356,   434,   682,   359,   307,    24,
      65,     4,   136,  1432,   313,   328,   525,   196,    18,   408,
      24,   717,    22,   322,    65,    21,   325,    21,   282,    19,
      20,   114,   103,   159,   391,   196,   202,  1456,   159,   308,
     165,   980,  1456,   297,   196,   150,   199,   408,   159,   198,
      65,   328,  1397,   199,   202,   589,   202,   318,   319,   205,
     199,    65,   199,   199,   377,   204,   199,   589,   202,  1275,
     500,   500,   882,   202,   328,   609,    24,   727,  1187,   202,
     202,  1126,   199,   202,   202,  1136,   896,   609,   202,   198,
     202,   196,   334,   387,  1592,   504,   202,   198,   202,   199,
     377,  1456,   356,   402,   199,   359,   151,   199,   362,   878,
     387,   500,   881,   196,   383,   384,   385,    65,   387,   888,
     262,   199,   891,   377,   423,   424,   895,   854,   151,   199,
     199,   199,   198,   387,   199,   159,   199,   199,  1702,   500,
    1018,   159,    24,   151,   980,   199,   199,   202,   199,     3,
     199,  1570,   623,  1572,   408,   397,   398,  1604,    24,  1578,
     199,   202,   199,    55,   199,   949,   159,    21,   199,   199,
     424,  1590,   199,   199,   193,    24,   198,   196,   197,   189,
     190,   196,   192,    65,   165,   462,   196,   202,   328,    24,
     198,   725,   196,   199,    24,   725,   196,   148,   202,    65,
     198,   196,   198,   725,   198,   522,   204,   197,   462,   502,
     503,   103,   670,   722,    24,   667,    65,   564,   575,   671,
     672,   498,   499,   500,    24,    24,   177,   196,   623,   198,
      65,  1414,  1361,   725,    55,    65,   614,   377,   590,   591,
     592,   614,    55,  1385,   498,   499,   500,  1453,   196,   601,
     504,  1523,   682,   682,   202,    65,  1473,  1473,   667,   196,
     769,    24,   671,   672,   693,    65,    65,   560,   199,   563,
      12,    13,    14,    15,    16,    17,    18,   554,   563,   421,
      55,   535,   103,    38,   793,   570,   563,  1015,  1003,   179,
     103,  1019,   434,   682,   563,   991,  1347,    36,   172,   101,
     554,   570,    65,   557,   693,   159,  1090,  1432,   198,   563,
     991,   118,    51,  1364,   196,   745,   745,   594,   595,   198,
     202,   682,  1200,   197,   593,   563,   603,   604,   103,    22,
    1375,   563,   693,   602,  1084,   589,   202,   159,   570,  1015,
     594,   595,  1155,  1019,   812,    48,   814,   196,   150,   603,
     604,   628,   629,   202,   623,   609,   625,   626,   627,  1169,
    1518,   196,   172,   159,    24,   172,   643,   202,   121,   122,
     159,   670,   202,   198,   628,   629,  1593,  1593,     4,   189,
     190,   129,   192,    19,    20,   177,   196,   197,   642,   643,
     197,   172,   202,   107,   196,   198,   196,   196,   112,  1168,
     196,   115,   202,   202,   658,    65,   135,   196,   189,   190,
    1482,   192,   115,   116,    24,   196,   197,   172,   145,    29,
    1570,    31,    21,    22,   166,   167,   168,   169,   682,   172,
     144,  1571,   180,   196,   189,   190,    29,   192,    31,   202,
    1590,   196,   197,   196,   187,   699,  1571,  1572,   196,     3,
     727,  1591,   196,  1578,   197,   199,     3,   991,   200,   201,
     129,   991,   196,   717,   992,   199,  1591,   179,    22,   768,
       3,   725,   175,   727,   826,    22,   196,   198,    88,    89,
      90,    91,   202,   204,   753,    18,   198,   199,   428,    22,
     807,   431,   204,   129,   147,    88,    89,    90,    91,   856,
    1658,   848,     5,   103,   104,   105,   106,   173,   174,   119,
     198,   180,   811,   812,  1453,   814,  1231,   771,    18,   117,
     189,   190,    22,   192,   198,   877,   119,   196,    19,    20,
     196,   162,   163,   164,  1230,   135,   196,   198,   189,   190,
     892,   192,   202,   143,   180,   196,   924,   942,   199,   197,
     198,   924,   829,   189,   190,   193,   192,   193,   910,   197,
     196,   197,     4,   196,   916,   117,   199,   177,  1446,   863,
    1189,  1189,   205,     6,     7,   829,   875,   187,   930,   196,
     198,  1115,   199,   198,   177,  1015,   863,   198,   205,  1019,
    1019,    27,   202,  1115,   863,    29,    32,    31,    34,   189,
     190,   878,   192,   198,   881,   882,   196,  1037,   198,   863,
     879,   888,   889,   890,   891,   109,   893,   111,   895,   896,
     897,  1155,    47,  1032,   878,   894,  1015,   881,   882,  1206,
    1019,  1208,   931,  1155,   888,   889,   890,   891,   129,   893,
     198,   895,   896,   897,   929,  1326,   103,   104,  1037,  1330,
     189,   190,   929,   192,  1015,   177,   178,   196,  1019,   198,
     929,   196,   197,  1093,   177,   178,   920,   944,   945,    29,
     198,    31,   198,   942,   197,   929,  1037,   109,   110,   170,
     171,    40,    41,  1607,  1608,   189,   190,   205,   192,   180,
     944,   945,   196,   992,   205,   199,    65,    66,   189,   190,
       5,   192,   193,   179,  1093,   196,   197,   198,    71,    72,
      73,    23,    75,    10,   991,  1014,   107,  1235,  1414,   211,
     212,   112,   329,   330,   115,     8,   980,   198,    88,    89,
      90,    91,  1093,  1414,   125,   198,   127,   991,   196,   130,
     131,  1093,  1019,   189,   190,  1473,   192,   196,   196,   196,
     196,     4,   196,   144,   180,   181,   182,   183,   149,   119,
    1014,  1015,   188,     5,  1018,  1019,   189,   190,   194,   192,
     196,  1280,     4,   196,  1393,  1393,    68,    69,  1032,    11,
     151,  1133,   204,  1037,   173,   109,  1238,    19,    20,    21,
     198,    83,  1326,   196,  1087,    87,  1330,  1149,   198,   189,
     190,    93,   192,   199,     5,   199,   196,  1084,     5,  1239,
    1239,     5,  1097,   155,  1166,     3,  1070,   177,  1117,   199,
    1097,   179,   179,   179,   207,   179,   199,   187,  1097,  1238,
    1084,     4,   196,   198,     5,    22,     3,   198,    11,  1093,
       3,  1095,    74,  1097,   196,   196,    19,    20,    21,   199,
    1239,   115,   107,   199,     4,   178,   196,   112,   159,  1136,
     115,  1115,     3,  1292,     3,  1593,    98,   196,   198,   161,
     125,   204,   127,     4,   199,   130,   131,     4,  1239,   171,
    1414,    29,  1136,    31,  1414,   118,   199,  1164,   198,   144,
     139,  1168,  1169,   196,   149,  1189,   187,   129,  1167,   199,
     198,  1155,   187,   180,   181,   182,   183,     5,   196,   199,
    1164,   188,  1189,    39,  1168,  1169,   196,   194,   189,   190,
    1189,   192,   154,   199,   187,   196,   187,     3,   199,   179,
      18,     5,     3,  1232,     3,  1189,   198,    22,   170,   171,
      88,    89,    90,    91,   198,  1199,  1200,   198,   180,   196,
     198,    21,   108,    21,   199,   198,   129,   189,   190,   198,
     192,   193,     3,   198,   196,   197,   198,   198,     3,     3,
     196,   119,   196,   151,   206,   174,  1230,   198,   198,   144,
     159,   154,     3,  1282,   119,  1239,   278,     3,  1287,   198,
     112,   283,  1291,   115,   286,  1294,  1265,   170,   171,     5,
       5,   293,     3,   125,  1303,   127,    30,   180,   130,   131,
       5,     3,    31,   199,  1710,   307,   189,   190,   310,   192,
     193,    29,   144,   196,   197,   198,     3,   149,  1380,   177,
    1284,   199,     5,   206,     3,   199,   108,   199,   199,   187,
     199,     4,  1723,    21,   199,   198,   198,   196,    20,  1326,
     196,     4,     4,  1330,   346,   347,  1355,   349,   196,   199,
      28,    29,    30,    31,     5,   199,   358,   199,   360,    37,
    1347,   199,  1326,     3,   199,     3,  1330,     4,   199,   371,
      26,   373,   199,    98,    55,    74,     5,  1364,   199,    60,
       5,   204,     4,  1347,    65,    66,     3,   108,   198,  1393,
     196,   393,   199,   204,   199,  1374,  1564,   198,  1385,   196,
    1364,  1563,   108,  1412,     4,   199,  1393,     3,   151,     3,
      88,    89,    90,    91,  1393,     5,    97,     4,  1580,     5,
       3,  1385,   103,  1432,   198,     5,   107,  1414,     5,  1393,
       3,    22,   199,     3,   199,     4,     3,     3,     3,    21,
       4,   119,     4,     3,  1563,     3,   199,  1456,     4,    11,
    1414,   199,   199,   199,     3,   196,   198,    19,    20,    21,
     196,  1580,   199,   196,   196,   129,     3,    21,   470,   196,
       4,     4,   474,     5,     3,   199,   199,   199,  1482,  1723,
     199,    22,  1446,   485,     4,   199,  1473,   199,   198,  1453,
     154,    19,    20,     3,   175,  1482,   199,     3,   199,   177,
     502,   503,   199,  1482,   506,     3,   170,   171,     4,   199,
     512,     4,     3,    52,     4,  1605,   180,  1734,  1482,   103,
     924,  1015,  1531,  1518,   202,   189,   190,   529,   192,   193,
     415,  1518,   196,   197,   198,  1015,   538,    23,  1292,  1518,
    1283,  1528,   206,   722,  1236,   547,   522,  1456,    14,  1280,
    1712,  1015,  1539,   555,  1518,  1564,   424,  1284,   560,  1446,
    1445,  1570,  1571,  1572,  1528,   408,   255,   129,   793,  1578,
    1060,  1572,  1432,  1573,   499,  1539,  1578,   579,   795,  1019,
     725,  1590,  1591,  1015,   989,   807,     0,     1,  1070,  1300,
    1710,  1230,   154,  1712,    29,  1604,    31,   159,  1579,   463,
    1002,   129,   897,   557,  1032,     4,  1593,   563,   170,   171,
      24,  1529,    11,  1539,    28,    29,    30,    31,   180,  1115,
      19,    20,  1364,    37,  1531,  1355,   154,   189,   190,  1540,
     192,   193,   450,   269,   196,   197,   198,   563,   451,   641,
     321,  1605,   170,   171,   206,   768,   455,   328,   812,   453,
     652,    65,   180,    88,    89,    90,    91,   563,  1667,   661,
     809,   189,   190,  1658,   192,   193,   458,   814,   196,   197,
     198,  1658,   321,  1682,    88,    89,    90,    91,   206,  1658,
    1659,   568,   330,  1395,   119,   389,   568,   563,  1133,   622,
     886,  1380,   384,   563,  1658,   624,   377,   945,   942,  1385,
     916,     4,   383,   384,   910,   119,   387,  1453,    11,   563,
     563,   563,   336,  1149,    57,   471,    19,    20,   715,  1728,
    1702,   284,  1731,   563,   726,   642,  1200,  1199,    29,   563,
     129,   699,  1408,   563,   563,  1744,  1723,  1203,   662,  1095,
      -1,     4,   177,    -1,    -1,    -1,  1710,   749,    11,   751,
      -1,    -1,   187,    -1,    -1,   154,    19,    20,    -1,  1723,
      -1,    -1,    -1,   177,   766,    -1,   180,   181,   182,   183,
     772,   170,   171,    -1,   188,    -1,    -1,    -1,    -1,    -1,
     194,   180,    -1,    -1,    -1,    -1,    -1,    -1,   202,    -1,
     189,   190,    -1,   192,   193,    -1,    -1,   196,   197,   198,
      -1,    -1,    -1,   805,    -1,    -1,    -1,   206,    -1,    -1,
      -1,    -1,    -1,    -1,     4,   817,    -1,   498,   499,   500,
     822,    11,   824,    -1,    -1,    -1,   129,    -1,    -1,    19,
      20,     5,     6,     7,     8,     9,    10,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    20,    21,    22,    23,
     852,   154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   863,    -1,    -1,    -1,    -1,   129,   170,   171,    -1,
      -1,    -1,    -1,   554,    -1,    -1,    -1,   180,    -1,    -1,
      -1,    -1,   563,    -1,    -1,    -1,   189,   190,    -1,   192,
     193,   154,    -1,   196,   197,   198,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   206,     5,    -1,    -1,   170,   171,   911,
      11,    -1,    -1,   594,   595,   917,    -1,   180,    19,    20,
      -1,    22,   603,   604,    -1,    -1,   189,   190,    -1,   192,
     193,    -1,    -1,   196,   197,   198,    -1,    -1,    -1,   129,
      -1,   622,    -1,   206,    -1,    -1,    -1,   628,   629,    -1,
      -1,   953,    -1,     4,   956,   957,    -1,    -1,    -1,    -1,
      11,    -1,   643,    25,   154,    -1,    -1,    -1,    19,    20,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   658,     5,    -1,
     170,   171,    -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,
     180,    -1,    19,    20,    -1,    -1,    -1,    -1,    -1,   189,
     190,    -1,   192,   193,    -1,    -1,   196,   197,   198,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   206,    -1,   699,    -1,
      -1,    -1,    -1,    -1,    -1,   199,   200,   201,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1039,    -1,    -1,
    1042,   103,   104,   105,   106,    -1,   727,    -1,    -1,    -1,
      -1,    -1,  1054,   154,  1056,    -1,    -1,   160,    -1,  1061,
      -1,    -1,    -1,    -1,    -1,    -1,  1068,  1069,   171,   170,
     171,    -1,    -1,   135,    -1,   137,   138,   139,   129,   180,
     142,   143,    -1,   145,   146,  1087,  1088,    -1,   189,   190,
      -1,   192,   193,    -1,    -1,   196,   197,   198,    -1,  1101,
      -1,  1103,   129,   154,    -1,   206,    -1,    -1,    -1,    -1,
      -1,    -1,  1114,    -1,    -1,    -1,    -1,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,   180,
      -1,   234,    -1,    -1,    -1,    -1,    -1,   240,   189,   190,
      -1,   192,   193,   170,   171,   196,   197,   198,   829,    -1,
      -1,    -1,   255,   180,    -1,   206,    -1,    -1,    -1,    -1,
       4,    -1,   189,   190,    -1,   192,   193,    11,    -1,   196,
     197,   198,    -1,  1175,    -1,    19,    20,  1179,    -1,   206,
      -1,  1183,   863,  1185,  1186,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1196,    -1,    -1,   878,    -1,    -1,
     881,   882,    -1,    -1,    -1,   308,   309,   888,   889,   890,
     891,    -1,   893,    97,   895,   896,   897,    -1,   321,    -1,
      -1,   324,  1224,   107,    -1,    -1,     4,    -1,    -1,  1231,
      -1,    -1,  1234,    11,    -1,  1237,    -1,   340,   341,    -1,
      -1,    19,    20,    -1,    -1,    -1,    -1,    -1,   929,    -1,
      -1,    -1,    -1,   356,    -1,     4,   359,    -1,    -1,    -1,
    1262,   942,    11,   944,   945,    -1,    -1,    -1,    -1,    -1,
      19,    20,    -1,    -1,    -1,    -1,    -1,    -1,   381,    -1,
     383,   384,   385,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     5,    -1,    -1,    -1,    -1,    -1,    11,
      -1,    -1,  1304,    -1,  1306,   408,    -1,    19,    20,    -1,
     154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,  1015,  1337,   180,  1018,  1019,    -1,
      -1,  1343,    -1,    -1,    -1,   189,   190,    -1,   192,   193,
      -1,   129,   196,   197,   198,    -1,    -1,  1359,    -1,    -1,
      -1,  1363,   206,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,  1381,
     129,  1383,    -1,    -1,  1386,    -1,  1388,  1389,    -1,  1391,
      -1,    -1,   170,   171,    -1,    -1,    -1,    -1,   501,    -1,
    1402,   504,   180,  1084,    -1,   154,    -1,    -1,    -1,    -1,
      -1,   189,   190,   516,   192,   193,  1097,   129,   196,   197,
     198,   170,   171,    -1,    -1,    -1,    -1,    -1,   206,    -1,
      -1,   180,    -1,    -1,    -1,    -1,    38,   321,    -1,    -1,
     189,   190,   154,   192,   193,    -1,    -1,   196,   197,   198,
      -1,    -1,    -1,  1455,    -1,  1136,    -1,   206,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   189,   190,    -1,
     192,   193,    -1,  1164,   196,   197,   198,  1168,  1169,     5,
      -1,    -1,    -1,    -1,   206,    11,    -1,    -1,    -1,   383,
     384,    -1,    -1,    19,    20,    -1,    -1,    -1,  1189,    -1,
      -1,   113,    -1,    -1,    -1,    -1,   118,  1519,  1199,  1200,
     623,   123,   124,  1525,   126,    -1,    -1,   129,  1530,    -1,
     132,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,  1541,
    1542,  1543,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1552,    -1,    -1,    -1,    -1,   658,    -1,    -1,  1560,    -1,
    1562,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     172,    11,    -1,    -1,   677,    -1,    -1,    -1,    -1,    19,
      20,    21,    -1,    -1,    -1,    -1,    -1,   189,   190,    -1,
     192,    -1,    -1,    -1,   196,   197,   699,    -1,    -1,    -1,
      -1,    -1,    -1,    11,    -1,    -1,    -1,   710,    -1,   712,
      50,    19,    20,   129,   498,   499,    24,    -1,    58,    -1,
      -1,    61,    62,    63,    64,    -1,    66,    -1,    68,    69,
      -1,    -1,    -1,  1635,    -1,    -1,    -1,    -1,   154,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   752,
    1652,   754,    -1,    -1,   170,   171,    -1,    65,    -1,   762,
      -1,  1663,   765,    -1,   180,    -1,  1347,    -1,    -1,    -1,
     554,    -1,    -1,   189,   190,    -1,   192,   193,    -1,    -1,
     196,   197,   198,  1364,    -1,    -1,    -1,    -1,    -1,   129,
     206,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1701,
      -1,    -1,  1704,    -1,  1385,    -1,    -1,    -1,    -1,    -1,
     594,   595,  1393,    -1,   154,    -1,    -1,    -1,    -1,   603,
     604,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     170,   171,   172,    -1,    -1,    -1,    -1,    -1,   622,    -1,
     180,    -1,    -1,    -1,   628,   629,   154,    -1,   851,   189,
     190,    -1,   192,   193,    -1,    -1,   196,   197,   198,   643,
      -1,    -1,   170,   171,  1445,  1446,   206,    -1,   871,   872,
     873,   874,   180,    -1,   658,    -1,    -1,   880,    -1,    -1,
      -1,   189,   190,   886,   192,   193,    -1,    -1,   196,   197,
     198,    -1,  1473,    -1,   202,    -1,    -1,    -1,   206,    -1,
      -1,  1482,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   699,    -1,    -1,    -1,   922,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1518,    -1,   942,
     943,    -1,    -1,   727,    -1,    -1,   949,  1528,    -1,   952,
      -1,    -1,    -1,    -1,    -1,     6,     7,     8,  1539,    10,
      -1,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,   980,    -1,    -1,
      -1,    -1,    -1,    -1,    11,   988,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1593,    11,    65,  1018,    -1,    -1,    -1,    -1,
      -1,    19,    20,    -1,  1027,    -1,    -1,    -1,  1031,  1032,
      -1,  1034,  1035,    -1,  1037,    -1,    -1,    -1,    65,    -1,
      -1,    -1,    -1,    -1,    -1,   829,    -1,    -1,    -1,     6,
       7,     8,    -1,    10,    -1,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1658,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1090,    -1,    -1,
    1093,    -1,    -1,  1096,   878,    -1,    -1,   881,   882,    -1,
      98,    -1,   129,    -1,   888,   889,   890,   891,    65,   893,
      -1,   895,   896,   897,    -1,   166,   167,   168,   169,    -1,
      -1,    -1,    -1,  1126,    -1,    -1,    -1,   154,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   170,   171,    -1,   197,    -1,    -1,   200,
     201,   202,    -1,   180,   205,    -1,   154,    -1,   942,    -1,
     944,   945,   189,   190,    -1,   192,   193,    -1,    -1,   196,
     197,   198,   170,   171,  1177,   202,    -1,    -1,    -1,   206,
      -1,    -1,   180,    -1,    -1,    -1,    -1,  1190,    -1,    -1,
      -1,   189,   190,    -1,   192,   193,  1199,  1200,   196,   197,
     198,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   206,   166,
     167,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1229,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1018,    -1,    -1,    -1,    -1,    -1,
     197,   198,    -1,   200,   201,   202,    -1,   204,    -1,    -1,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    20,
      -1,  1264,    -1,    -1,    -1,  1268,    -1,    -1,    -1,    -1,
      -1,    -1,  1275,    -1,    -1,    -1,    -1,    -1,    -1,    11,
      -1,    -1,    -1,    44,    -1,    -1,    -1,    19,    20,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    58,    -1,    -1,
    1084,    -1,    -1,    -1,    -1,    66,  1309,    68,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
    1323,    53,    54,    55,    56,    57,    58,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    66,    67,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1136,   114,    -1,    -1,    -1,    -1,  1361,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
    1373,    -1,  1375,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1164,    -1,    -1,    -1,  1168,  1169,    -1,    -1,    -1,    -1,
      -1,    -1,  1395,   154,  1397,    -1,    11,   129,    -1,   160,
      -1,    -1,    -1,    -1,    19,    20,    21,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,  1199,  1200,    -1,    -1,   180,
      -1,    -1,   154,    -1,    -1,    -1,    -1,    -1,   189,   190,
      -1,   192,   193,    -1,    -1,   196,   197,   198,   170,   171,
      -1,    -1,  1445,  1446,    -1,   206,    -1,    -1,   180,  1452,
    1453,    -1,    -1,    -1,    -1,    -1,    -1,   189,   190,    -1,
     192,   193,    -1,    -1,   196,   197,   198,    -1,    -1,  1472,
      -1,    -1,    -1,    -1,   206,    -1,    -1,    -1,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    20,    -1,    -1,
      -1,    -1,    -1,    -1,  1497,  1498,  1499,  1500,  1501,  1502,
    1503,  1504,  1505,  1506,  1507,  1508,  1509,  1510,  1511,  1512,
    1513,  1514,  1515,    -1,   129,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    -1,    58,    -1,    60,    -1,    -1,
      -1,    -1,    -1,    66,    -1,    68,    69,    70,    -1,   154,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    20,
      -1,    -1,    -1,    -1,    -1,   170,   171,   172,    -1,    -1,
      -1,    -1,    -1,  1347,  1567,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   189,   190,    -1,   192,   193,    -1,
    1364,   196,   197,   198,    -1,    -1,    57,    58,    -1,  1592,
      -1,   206,    -1,    -1,    -1,    66,   129,    68,    69,    -1,
      -1,  1385,    -1,    -1,    -1,    -1,  1609,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,    -1,  1626,  1627,    -1,    -1,    -1,  1631,    -1,
    1633,    -1,    11,    -1,    -1,    -1,    -1,   170,   171,    -1,
      19,    20,    -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   189,   190,   129,   192,
     193,  1445,  1446,   196,   197,   198,    -1,    -1,    -1,    -1,
      49,    -1,    51,   206,    53,    54,    -1,    56,    -1,    58,
      -1,    60,    -1,   154,    -1,    -1,    -1,    66,  1691,    68,
      69,    70,    -1,    -1,    11,    -1,    -1,    -1,    -1,   170,
     171,    -1,    19,    20,    -1,    -1,    -1,    -1,    -1,   180,
      -1,    -1,    -1,    -1,  1717,    -1,    -1,    -1,   189,   190,
      -1,   192,   193,    -1,    -1,   196,   197,   198,    11,  1732,
      -1,  1734,  1735,    -1,    -1,   206,    19,    20,  1741,    11,
      -1,    58,    -1,  1746,  1528,    -1,    -1,    19,    20,    66,
     129,    68,    69,    -1,    -1,  1539,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,
      -1,    -1,    -1,    -1,    -1,   154,    19,    20,    21,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    58,    -1,    -1,    -1,
      -1,   170,   171,    -1,    66,    -1,    68,    69,    -1,    -1,
      -1,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     189,   190,   129,   192,   193,    11,    -1,   196,   197,   198,
      -1,    -1,    -1,    19,    20,    21,    11,   206,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    20,    21,   154,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,   170,   171,    -1,    -1,   129,    -1,    -1,
      11,    -1,    -1,   180,    -1,    -1,    -1,    -1,    19,    20,
      -1,   154,   189,   190,    -1,   192,   193,    -1,    -1,   196,
     197,   198,   154,    -1,    -1,    -1,   129,   170,   171,   206,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,   189,   190,   180,   192,
     193,   154,    -1,   196,   197,   198,   199,   189,   190,    -1,
     192,   193,    11,   206,   196,   197,   198,   170,   171,    -1,
      19,    20,    -1,   129,   206,    -1,    -1,   180,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,   189,   190,    -1,   192,
     193,    11,    -1,   196,   197,   198,    -1,    -1,   154,    19,
      20,    -1,    11,   206,    -1,    -1,    -1,    -1,    -1,   154,
      19,    20,    -1,    -1,   170,   171,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,   180,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,   189,   190,   180,   192,   193,    -1,    -1,
     196,   197,   198,   154,   189,   190,    -1,   192,   193,    11,
     206,   196,   197,   198,    -1,    -1,    -1,    19,    20,   170,
     171,   206,    -1,    -1,    11,    -1,    -1,    -1,    -1,   180,
      -1,    -1,    19,    20,    -1,    -1,    -1,    -1,   189,   190,
     129,   192,   193,    -1,    -1,   196,   197,   198,    -1,    -1,
      -1,    -1,    -1,    -1,    11,   206,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    20,    -1,   154,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   180,    -1,    -1,   154,    -1,    -1,    -1,    -1,    -1,
     189,   190,    -1,   192,   193,   154,    -1,   196,   197,   198,
     170,   171,    -1,    -1,    -1,    -1,    -1,   206,    -1,    -1,
     180,   170,   171,    -1,    -1,    -1,    -1,   129,    -1,   189,
     190,   180,   192,   193,    -1,    -1,   196,   197,   198,    -1,
     189,   190,   129,   192,   193,    -1,   206,   196,   197,   198,
      -1,    -1,   154,    -1,    -1,    -1,    -1,   206,    -1,    25,
      -1,    -1,    -1,    -1,    -1,    -1,    32,   154,   170,   171,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,   180,    -1,
      -1,    -1,    -1,   170,   171,    -1,    -1,   189,   190,    -1,
     192,   193,    -1,   180,   196,   197,   198,   154,    -1,    -1,
      -1,    -1,   189,   190,   206,   192,   193,    -1,    -1,   196,
     197,   198,    -1,   170,   171,    -1,    -1,    83,    -1,   206,
      -1,    -1,    -1,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   189,   190,    -1,   192,   193,   103,   104,   196,
     197,   198,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   206,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    32,   139,   140,    -1,    -1,    -1,    38,   145,
      -1,   147,    -1,    -1,    -1,    -1,   152,    47,    48,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   172,    -1,    -1,   175,
     176,   177,    -1,    -1,   180,   181,   182,   183,    -1,    79,
      -1,    -1,   188,   189,   190,   191,   192,    -1,   194,    -1,
     196,   197,    -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,   111,    -1,   113,    -1,    -1,   116,    -1,   118,    32,
     120,    -1,    -1,   123,   124,    38,   126,    -1,    -1,   129,
      -1,    -1,   132,   133,   134,    48,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    32,    -1,    -1,
      -1,    -1,    -1,    38,    -1,    -1,    -1,    -1,   158,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,   172,   173,   174,   175,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,   189,
     190,    -1,   192,    -1,    79,    -1,   196,   197,    -1,    -1,
     113,    -1,    -1,   116,   117,    -1,    -1,   120,    -1,    -1,
     123,   124,    97,   126,    -1,    -1,   129,    -1,    -1,   132,
     133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
      -1,   116,   117,    -1,    -1,   120,    -1,    -1,   123,   124,
      38,   126,    -1,    -1,   129,   158,    -1,   132,   133,   134,
      48,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   172,
      -1,    -1,   175,    -1,    -1,    -1,    -1,    -1,    38,    -1,
      -1,    -1,    -1,   158,   187,    -1,   189,   190,    -1,   192,
      -1,    79,    -1,   196,   197,    -1,    -1,   172,    -1,    -1,
     175,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    97,
      -1,    -1,   187,    -1,   189,   190,    -1,   192,    -1,    79,
      -1,   196,   197,    -1,    -1,   113,    -1,    -1,   116,   117,
      -1,    -1,   120,    -1,    -1,   123,   124,    97,   126,    -1,
      -1,   129,    -1,    -1,   132,   133,   134,    -1,   108,    -1,
      -1,    -1,    -1,   113,    -1,    -1,    38,    -1,    -1,    -1,
     120,    -1,    -1,   123,   124,    -1,   126,    -1,    -1,   129,
     158,    -1,   132,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   172,    -1,    -1,   175,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,   158,   187,
      -1,   189,   190,    -1,   192,    -1,    -1,    -1,   196,   197,
      -1,    -1,   172,    -1,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   189,
     190,   113,   192,    -1,    -1,    -1,   196,   197,   120,    -1,
      -1,   123,   124,    -1,   126,    -1,    -1,   129,    -1,    -1,
     132,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   189,   190,    -1,
     192,    -1,    -1,    -1,   196,   197
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   209,     0,     1,    24,    28,    30,    37,    65,    88,
      89,    90,    91,   119,   177,   202,   210,   211,   212,   213,
     215,   216,   269,   270,   643,   646,   663,   665,   699,   700,
     701,   702,   703,   713,   714,   189,   217,   196,   196,   180,
     181,   182,   183,   188,   194,   196,   236,   271,   320,   214,
      24,   202,    24,   271,   241,   242,   411,   656,   658,   664,
     664,    29,    31,   119,   701,   664,   664,   214,   666,   647,
     273,   272,   277,   274,   276,   275,   198,   199,   199,   214,
     202,   213,    25,    32,    83,   103,   104,   116,   137,   139,
     140,   152,   172,   175,   176,   189,   190,   191,   192,   196,
     197,   236,   243,   244,   245,   256,   264,   265,   293,   295,
     297,   336,   337,   352,   370,   375,   392,   393,   398,   403,
     404,   411,   412,   418,   422,   428,   439,   441,   450,   452,
     454,   457,   458,   459,   520,   521,   546,   547,   548,   550,
     556,   557,   560,   566,   571,   594,   617,   642,   680,   683,
     725,   165,   658,   264,   265,   645,   652,   726,    38,    48,
      79,    97,   113,   120,   123,   124,   126,   129,   132,   133,
     134,   158,   196,   246,   247,   257,   258,   264,   464,   481,
     520,   521,   531,   532,   536,   546,   565,   566,   575,   576,
     579,   591,   592,   593,   594,   595,   600,   607,   611,   614,
     617,   630,   633,   636,   688,   693,   725,   196,   716,   196,
     706,   246,   246,   211,   211,    21,   198,   278,   279,   278,
     278,   278,    21,   198,   289,   290,   291,   278,     4,    11,
      19,    20,    21,   129,   154,   170,   171,   180,   193,   197,
     198,   206,   236,   259,   262,   268,   280,   281,   284,   285,
     293,   294,   321,   322,   327,   329,   439,   488,   489,   490,
     491,   492,   495,   496,   498,   500,   501,   502,   503,   504,
     508,   511,   513,   686,   695,   399,   211,   198,   423,   424,
       3,   211,   218,   551,   159,   196,   681,   682,   159,   406,
      22,   429,   211,   551,     3,   298,   299,   198,   379,   380,
     382,   383,   439,   441,     4,   198,   245,   107,   112,   115,
     144,   196,   441,   198,   296,   269,   372,   406,   135,   145,
     147,   151,   198,   522,     5,   198,   453,   544,   545,   553,
     553,   544,   657,   117,   644,   653,   654,   655,   702,   713,
     198,   198,   196,   236,   240,   508,   564,   196,   211,   265,
     196,   202,   728,   198,   198,   639,   198,   513,   598,   198,
     598,   198,   198,   211,   508,   510,   511,   516,   517,   518,
     519,   596,     4,   117,   251,   252,   248,   249,   250,   257,
     642,   115,   125,   127,   130,   131,   149,   533,    47,   577,
     578,   582,   198,   689,   198,   720,   198,   251,   251,   197,
      98,   518,   198,   281,   292,    74,    98,   268,   198,   328,
     331,   332,   333,   508,   205,   205,   236,   280,   283,   284,
     286,   495,   508,     5,     3,   199,   328,    23,   493,    21,
      22,   494,   280,   281,   495,   281,   491,    12,    13,    14,
      15,    16,    17,    18,   166,   167,   168,   169,   200,   201,
     497,   499,    10,   505,     8,   506,     6,     7,   507,   498,
     198,   198,   425,     3,   211,   121,   122,   236,   552,   211,
     681,     3,   211,   196,   684,   685,   196,    18,    22,   431,
     432,   211,   300,   301,   352,   196,   159,   196,   383,   384,
     385,   452,   456,   457,   458,     3,   377,   378,   433,     3,
      22,   115,   116,   175,   455,   211,   513,    21,   197,   265,
     508,   510,   621,   508,   211,     4,     5,   451,   512,   513,
     269,   305,   306,   307,   308,     3,   339,   340,   341,   371,
     196,   373,   374,   679,   406,   406,   148,   269,   414,   415,
     196,   439,   440,   441,   508,   528,   529,   530,   479,   513,
     523,   524,   525,   526,   151,   508,   451,   204,   250,   173,
     174,   196,   266,   267,   549,   554,   555,   558,   561,   562,
     267,   554,   109,   567,   568,   572,     3,   159,   661,   727,
     411,   648,   655,   508,   539,   508,   211,   211,   211,   465,
     601,   198,    58,    66,    68,    69,   508,   598,   634,   635,
     211,    58,    66,    68,    69,   598,   631,   632,   211,   482,
     236,   731,   211,    48,   211,   667,   668,   250,   508,   198,
      21,    50,    58,    61,    62,    63,    64,    66,    68,    69,
     196,   441,   445,   597,   598,   599,   618,   619,   620,   621,
     618,   621,   694,   265,   520,   531,   532,   534,   537,   538,
     141,   198,   586,   136,   578,   583,   544,   199,   691,   211,
     721,   715,   704,   411,   710,   411,   717,     5,   199,   268,
       5,     5,     3,   199,   331,   508,   155,     3,   196,   197,
     282,   282,   179,   284,   199,   268,   322,   207,   330,   281,
     490,   490,   491,   492,   496,   500,   501,   502,   199,   687,
     196,   401,   402,   236,   427,   442,   450,   452,   458,   424,
     198,   543,     5,   211,   682,     3,   211,    22,   196,   196,
     436,   437,     3,   211,   198,     3,   376,   433,   379,   382,
     236,   280,   281,   283,   284,   293,   319,   320,   323,   351,
     381,   387,   390,   391,   439,   495,   686,   508,   211,   551,
     211,   551,     4,    21,   159,   236,   460,   461,   463,   508,
     511,   211,     3,   211,   199,   115,   508,   199,     4,     3,
     159,   309,   178,   304,   307,    25,   105,   106,   135,   137,
     138,   139,   142,   143,   145,   146,   342,   352,   196,   345,
     346,   348,   159,     3,   211,     3,   196,   395,   236,   347,
     407,   408,   409,   410,   432,   413,   198,     3,   211,   204,
     211,     4,     3,   199,     3,   199,   441,   527,   211,   199,
     454,   211,   551,     4,   118,    38,   132,   134,   264,   265,
     464,   481,   520,   531,   559,   575,   592,   594,   600,   607,
     611,   614,   617,   630,   633,   636,   688,   553,   549,   562,
     264,   198,   110,   568,   569,   570,   573,   544,   139,   672,
     196,   211,   187,   199,   199,   442,   452,   457,   472,   473,
     474,    49,    53,    54,    55,    56,    57,    58,    59,    66,
      67,    68,    69,   598,   603,   604,    49,    51,    52,    53,
      54,    56,    58,    60,    66,    68,    69,    70,   598,   640,
     641,   598,   265,   441,   448,   449,   441,   446,   447,   606,
       3,   199,   598,   265,   448,   606,     3,   199,   474,   484,
       3,   199,   198,   253,   254,   255,   702,   713,   187,   199,
     608,     5,    21,   597,   621,   196,   619,   265,   265,   265,
     448,   606,     3,   199,   199,     3,   211,   236,   695,    39,
     535,   540,   587,   196,   211,   196,   584,   690,   236,   259,
     441,   508,   696,   697,   698,    21,   196,   707,   722,   723,
     724,   211,   722,   187,   187,   518,   199,   268,   518,   518,
       3,   332,   236,   263,   280,   283,   287,   696,     5,     3,
     199,     3,   198,   508,   508,   685,   236,   420,   421,   442,
      22,   198,     3,   434,   301,   196,   302,   303,   236,   385,
     386,   552,   211,   379,   198,    21,   388,   388,   198,     3,
      22,   388,   281,   284,   199,   211,   211,     4,   508,   265,
     508,     5,     3,   199,     4,   159,   462,   198,   508,   624,
     625,   626,   108,   508,   211,   512,   105,   106,   135,   143,
     310,   311,   352,   236,   313,   314,   196,   211,   198,   198,
       3,   338,   198,   353,   342,   374,   198,     3,   394,   405,
       3,   211,   196,   416,   417,   415,   196,   528,   480,   513,
     479,   525,   479,   526,   151,   211,   211,   174,   196,   211,
     198,   196,   197,   198,   729,   198,   115,   267,   563,   553,
     508,   196,   211,   111,   568,   574,   544,   159,     3,   673,
     119,   650,   211,   538,   581,     3,   467,   198,   475,   509,
     510,   605,   509,   509,   509,   513,     5,   598,   448,   265,
     509,   448,   606,     3,   199,   605,     5,   448,   446,   446,
     448,   598,   441,   443,   444,   265,   448,   606,   443,     3,
     199,   635,   211,   632,   211,     3,   485,   236,   508,   255,
      30,   670,   258,   264,   265,    57,    58,    66,    68,    69,
     598,   609,   610,   513,   620,   615,   624,   198,   441,   612,
     622,   623,   626,   622,   539,    40,    41,   541,   542,   533,
       4,   508,   588,   589,   590,   211,   585,   211,   211,     5,
       3,   199,   199,     3,   199,    31,   712,    29,   719,   199,
       3,   199,     3,   199,   199,   236,   334,   335,   552,   199,
     199,   508,   515,   402,   400,   426,   427,   451,   199,     3,
       3,   419,   438,   437,   430,   433,     3,   199,     5,   198,
     236,   260,   261,   262,   263,   280,   283,   319,   323,   351,
     389,   390,   199,   259,   324,   325,   326,   441,   508,   527,
     530,   387,   108,   508,     4,    21,   508,   461,     4,   508,
     508,   508,   625,   627,   628,     3,   211,   211,   199,   198,
       3,   159,   198,   315,     3,   211,   211,   343,   344,   346,
     211,   354,    21,   288,   396,   196,   211,   211,   409,    20,
       3,   199,     4,     4,   527,   211,   551,   211,   539,    11,
      19,    20,    44,   114,   160,   170,   171,   189,   190,   192,
     193,   197,   219,   220,   224,   226,   229,   231,   236,   237,
     238,   508,   731,   508,   258,   199,   211,   196,   211,    27,
     196,   674,   675,   659,   196,   651,   211,   101,   150,   468,
     469,   473,   199,   476,   477,   478,   479,   509,   604,   602,
     446,   199,   641,   637,   101,   150,   474,   486,   487,   199,
     199,   196,   671,   115,     3,     5,   598,   265,   448,   606,
       3,   199,   211,   616,   211,     3,   613,   211,   199,   196,
     211,   196,   211,   533,   590,     3,   199,     4,   211,   236,
     698,   697,   692,   724,    26,   708,   709,   651,   651,    98,
      74,   199,     5,   211,     3,   199,   199,   508,   421,   211,
     433,     4,   355,   356,   357,   358,   359,   360,   361,   362,
     363,   364,   365,   366,   367,   368,   513,   514,   211,   432,
     303,   211,   518,   236,   280,     5,     3,   199,   211,   508,
     265,   508,     4,     3,   625,   108,   312,   311,     4,   316,
     356,   358,   362,   288,   314,   355,   162,   163,   164,   369,
     355,   292,     5,   151,   349,   350,   355,   198,   196,   417,
     211,   211,   199,   508,   199,   199,   508,   204,   450,   452,
     198,   198,   199,   196,   189,   239,   450,     5,     6,     7,
       8,     9,    10,    12,    13,    14,    15,    16,    17,    19,
      20,    21,    22,    23,   200,   201,   221,   199,   199,   108,
     211,     4,   151,     3,   211,   649,   446,   471,     5,     3,
     466,     3,   199,   480,     4,   211,   624,   211,   471,     5,
       3,   483,   580,   669,   508,   265,   509,   610,   211,   211,
     623,   211,   196,   211,   211,   211,   589,   590,   211,   198,
     705,   718,   718,     5,     5,   513,   427,     3,    22,   199,
       3,     4,     3,     3,   358,   366,   514,    21,     3,   435,
       3,   326,   325,   508,   552,   628,   629,   211,   316,   199,
       3,     4,     5,   151,   317,   318,   199,   199,   199,   515,
     319,   351,   439,   199,   397,   730,   227,   228,   230,     5,
     223,   508,   508,   508,   508,   508,   508,   508,   508,   508,
     508,   508,   508,   508,   508,   508,     5,    22,   222,   508,
     508,     5,   508,     5,   508,   196,   211,   196,   662,   675,
     676,   677,   678,   679,   196,   675,   211,   448,   470,   469,
     211,   477,   638,   470,   487,   211,   211,   211,   199,     3,
     211,   196,   211,   711,   211,   518,   268,     3,   508,   196,
     357,   359,   514,     4,   358,   361,   363,     4,    21,   368,
     436,   518,     5,   199,   199,   515,   319,   351,   355,   224,
     236,   225,   232,   232,   232,   508,   508,   508,   508,   508,
     211,   660,     3,   211,   265,   199,   211,   199,   199,   513,
      22,     4,     3,   199,   513,   199,   198,     4,   233,   234,
     235,   508,   199,   199,   199,   211,   677,   211,     3,   420,
     518,     3,     4,   508,     3,     4,   450,   513,   199,   513,
     508,     4,   234,   508,     3,   508,     4,   513,   508
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   208,   209,   209,   210,   210,   210,   211,   211,   211,
     211,   211,   212,   212,   212,   213,   213,   213,   214,   215,
     215,   215,   216,   216,   217,   218,   218,   219,   219,   219,
     219,   219,   220,   220,   221,   221,   221,   221,   221,   221,
     221,   221,   221,   221,   221,   221,   221,   221,   221,   221,
     221,   221,   221,   221,   221,   222,   222,   222,   222,   223,
     223,   224,   224,   224,   225,   226,   226,   227,   226,   228,
     226,   229,   230,   229,   231,   232,   232,   233,   233,   234,
     234,   235,   235,   235,   235,   235,   235,   235,   236,   237,
     237,   237,   237,   237,   237,   237,   237,   238,   238,   238,
     238,   239,   239,   240,   240,   241,   242,   242,   243,   243,
     244,   244,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   246,   246,   247,   248,   248,
     249,   249,   250,   250,   251,   251,   252,   253,   253,   254,
     254,   255,   255,   256,   256,   256,   256,   256,   256,   256,
     256,   256,   257,   257,   257,   257,   257,   258,   258,   258,
     258,   258,   258,   258,   258,   258,   258,   258,   258,   258,
     258,   258,   258,   258,   258,   258,   258,   258,   258,   258,
     258,   258,   259,   260,   261,   261,   262,   262,   262,   262,
     262,   263,   264,   264,   265,   265,   266,   266,   267,   268,
     268,   268,   270,   269,   269,   269,   272,   271,   273,   271,
     274,   271,   275,   271,   276,   271,   277,   271,   278,   278,
     279,   279,   279,   280,   280,   281,   281,   282,   282,   283,
     283,   284,   284,   285,   286,   286,   286,   287,   287,   287,
     288,   288,   289,   289,   290,   290,   290,   290,   290,   291,
     291,   291,   291,   292,   292,   293,   293,   293,   294,   294,
     296,   295,   297,   297,   298,   298,   299,   299,   300,   300,
     301,   302,   302,   303,   304,   304,   305,   305,   306,   306,
     307,   308,   309,   309,   309,   310,   310,   311,   311,   311,
     312,   311,   311,   313,   313,   314,   315,   315,   316,   316,
     317,   317,   318,   318,   318,   319,   320,   320,   321,   321,
     322,   322,   323,   323,   324,   324,   325,   325,   326,   326,
     326,   327,   327,   328,   329,   330,   331,   331,   332,   332,
     333,   334,   334,   335,   337,   338,   336,   339,   339,   340,
     340,   341,   341,   342,   342,   342,   343,   342,   342,   344,
     342,   342,   342,   342,   342,   342,   342,   345,   345,   346,
     347,   348,   349,   349,   350,   350,   350,   351,   352,   352,
     353,   354,   353,   355,   355,   355,   355,   355,   356,   356,
     357,   357,   358,   359,   360,   360,   361,   361,   362,   362,
     363,   364,   365,   365,   366,   366,   367,   367,   368,   369,
     369,   369,   371,   370,   372,   372,   373,   373,   374,   374,
     376,   375,   377,   377,   378,   378,   379,   380,   380,   381,
     381,   382,   382,   383,   383,   384,   384,   385,   385,   385,
     386,   387,   387,   387,   387,   387,   387,   387,   387,   388,
     388,   389,   389,   389,   389,   389,   389,   389,   390,   391,
     393,   394,   392,   396,   395,   397,   395,   399,   400,   398,
     401,   401,   402,   404,   405,   403,   406,   406,   407,   407,
     408,   408,   409,   409,   409,   410,   411,   412,   413,   412,
     414,   414,   415,   416,   416,   417,   417,   418,   419,   419,
     420,   420,   421,   422,   423,   423,   425,   424,   426,   426,
     427,   427,   427,   429,   430,   428,   431,   431,   432,   432,
     433,   433,   434,   435,   434,   436,   436,   437,   438,   437,
     439,   439,   439,   439,   440,   441,   442,   443,   444,   445,
     446,   447,   448,   449,   450,   450,   450,   451,   452,   453,
     453,   454,   455,   454,   456,   457,   458,   459,   459,   460,
     460,   460,   461,   461,   461,   461,   461,   461,   461,   461,
     461,   461,   462,   462,   462,   462,   462,   462,   463,   465,
     466,   464,   467,   467,   468,   468,   469,   469,   470,   471,
     472,   472,   473,   474,   474,   475,   475,   476,   476,   477,
     478,   478,   479,   480,   482,   483,   481,   484,   484,   485,
     485,   486,   486,   487,   487,   488,   488,   488,   488,   488,
     489,   490,   490,   491,   491,   492,   492,   492,   492,   492,
     493,   494,   494,   495,   495,   496,   496,   497,   498,   498,
     499,   499,   499,   499,   499,   499,   499,   499,   499,   499,
     499,   499,   500,   500,   501,   501,   502,   502,   503,   503,
     504,   505,   506,   507,   507,   508,   509,   510,   511,   512,
     512,   513,   514,   515,   516,   517,   518,   519,   520,   520,
     521,   521,   521,   522,   522,   523,   523,   524,   524,   525,
     526,   527,   528,   529,   530,   530,   530,   531,   532,   533,
     533,   534,   534,   535,   535,   536,   537,   537,   537,   538,
     539,   540,   540,   541,   541,   542,   542,   543,   544,   545,
     545,   546,   546,   546,   547,   547,   548,   548,   548,   548,
     549,   549,   549,   549,   550,   550,   550,   550,   551,   551,
     551,   551,   552,   553,   554,   554,   555,   555,   556,   556,
     557,   558,   559,   559,   559,   559,   559,   559,   559,   559,
     559,   559,   559,   559,   559,   559,   559,   559,   559,   560,
     560,   561,   561,   562,   563,   564,   564,   565,   566,   567,
     567,   567,   568,   569,   569,   569,   570,   571,   571,   571,
     572,   572,   573,   573,   574,   574,   575,   576,   577,   577,
     577,   578,   580,   579,   581,   579,   582,   582,   584,   583,
     585,   583,   587,   586,   586,   588,   588,   589,   589,   589,
     589,   590,   591,   591,   592,   593,   594,   595,   595,   596,
     596,   597,   597,   597,   598,   599,   601,   602,   600,   603,
     603,   604,   604,   604,   604,   604,   604,   604,   604,   604,
     604,   604,   604,   604,   605,   606,   608,   607,   609,   609,
     610,   610,   610,   610,   610,   610,   612,   611,   613,   611,
     611,   611,   615,   614,   616,   614,   617,   617,   618,   618,
     619,   620,   620,   620,   620,   620,   620,   620,   620,   620,
     620,   620,   620,   621,   621,   621,   622,   622,   623,   623,
     624,   624,   625,   625,   626,   627,   627,   628,   629,   629,
     630,   630,   631,   631,   632,   632,   632,   632,   632,   633,
     633,   634,   634,   635,   635,   635,   635,   635,   637,   636,
     638,   636,   639,   640,   640,   641,   641,   641,   641,   641,
     641,   641,   641,   641,   641,   641,   641,   642,   644,   643,
     645,   645,   647,   646,   649,   648,   650,   650,   651,   651,
     652,   653,   653,   654,   654,   655,   655,   656,   656,   657,
     659,   658,   660,   658,   661,   661,   661,   662,   662,   663,
     664,   664,   246,   246,   666,   665,   668,   669,   667,   670,
     670,   671,   671,   672,   673,   673,   674,   674,   675,   676,
     676,   677,   677,   677,   678,   679,   680,   680,   681,   681,
     682,   683,   684,   684,   685,   686,   687,   686,   689,   688,
     690,   688,   691,   692,   688,   694,   693,   695,   695,   695,
     696,   696,   697,   697,   698,   698,   698,   699,   699,   700,
     700,   701,   701,   701,   701,   701,   701,   702,   704,   705,
     703,   706,   707,   708,   708,   709,   711,   710,   712,   712,
     713,   715,   714,   716,   717,   718,   719,   719,   720,   721,
     720,   722,   722,   723,   723,   724,   724,   725,   725,   727,
     726,   728,   728,   729,   729,   729,   729,   729,   730,   731,
     731
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     1,     2,     1,     1,
       3,     2,     1,     3,     3,     1,     3,     1,     0,     1,
       1,     1,     1,     1,     1,     0,     1,     1,     1,     2,
       2,     2,     1,     1,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     3,     3,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     0,     1,     2,     2,     2,
       1,     1,     1,     1,     0,     1,     2,     0,     5,     0,
       6,     1,     0,     5,     4,     1,     2,     1,     3,     1,
       1,     3,     5,     4,     3,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     2,     1,
       1,     0,     1,     0,     1,     2,     0,     1,     0,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     2,     0,     1,
       1,     2,     1,     1,     0,     1,     3,     0,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     4,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     1,     0,     1,     1,     1,
       1,     1,     0,     2,     3,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     1,
       3,     5,     2,     1,     2,     1,     3,     1,     1,     1,
       2,     1,     3,     5,     1,     1,     1,     1,     1,     1,
       0,     2,     0,     1,     1,     9,     5,     5,     9,     3,
       5,     2,     3,     3,     1,     1,     1,     1,     1,     1,
       0,     4,     4,     7,     0,     2,     0,     2,     1,     3,
       1,     1,     3,     1,     2,     3,     0,     1,     1,     2,
       1,     4,     0,     1,     3,     1,     3,     1,     1,     1,
       0,     5,     1,     1,     3,     4,     0,     3,     1,     1,
       0,     1,     2,     2,     2,     1,     1,     4,     1,     3,
       1,     3,     3,     4,     1,     3,     1,     3,     1,     1,
       1,     3,     3,     1,     1,     1,     1,     3,     1,     1,
       5,     5,     7,     1,     0,     0,     6,     0,     2,     0,
       1,     2,     3,     1,     1,     1,     0,     5,     1,     0,
       5,     1,     1,     1,     1,     1,     1,     1,     3,     4,
       1,     1,     0,     1,     2,     2,     2,     1,     1,     1,
       0,     0,     4,     1,     1,     1,     1,     1,     1,     3,
       3,     1,     1,     1,     1,     3,     1,     2,     1,     3,
       1,     3,     0,     2,     0,     2,     1,     3,     2,     1,
       1,     1,     0,     4,     0,     2,     1,     3,     1,     1,
       0,     5,     0,     1,     2,     3,     4,     1,     3,     1,
       3,     1,     1,     9,    11,     1,     3,     1,     1,     1,
       1,     2,     2,     2,     1,     1,     1,     1,     1,     0,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     0,     6,     0,     5,     0,     7,     0,     0,     7,
       1,     3,     3,     0,     0,     6,     0,     1,     0,     1,
       1,     3,     1,     1,     1,     1,     0,     4,     0,     5,
       1,     3,     4,     1,     3,     1,     3,     7,     0,     6,
       1,     3,     1,     3,     1,     3,     0,     6,     1,     3,
       1,     1,     1,     0,     0,     7,     0,     1,     1,     3,
       0,     1,     0,     0,     5,     1,     3,     1,     0,     5,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     4,     4,     3,     2,     0,
       3,     1,     0,     5,     1,     1,     1,     1,     4,     0,
       1,     3,     2,     1,     2,     3,     4,     2,     1,     3,
       4,     2,     1,     2,     3,     4,     2,     0,     1,     0,
       0,     8,     0,     2,     1,     3,     2,     3,     1,     1,
       1,     3,     2,     1,     1,     0,     3,     1,     3,     2,
       0,     2,     1,     1,     0,     0,     8,     1,     3,     0,
       2,     1,     3,     2,     3,     1,     1,     1,     1,     3,
       1,     1,     3,     1,     3,     1,     2,     3,     1,     2,
       1,     1,     1,     1,     1,     1,     3,     1,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     3,     1,     3,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     5,
       5,     7,     4,     0,     3,     1,     3,     1,     3,     2,
       3,     1,     1,     3,     1,     1,     1,     5,     5,     0,
       2,     0,     3,     0,     3,     5,     1,     1,     1,     1,
       1,     4,     5,     2,     3,     2,     3,     0,     1,     0,
       2,     1,     1,     1,     3,     3,     4,     2,     5,     3,
       4,     2,     5,     3,     4,     2,     5,     3,     6,     8,
       5,     3,     1,     1,     1,     2,     3,     4,     1,     1,
       3,     2,     1,     1,     1,     1,     1,     1,     1,     2,
       4,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       3,     2,     3,     3,     2,     0,     1,     3,     5,     0,
       1,     2,     2,     0,     1,     2,     2,     7,     8,     6,
       6,     7,     2,     3,     2,     3,     5,     3,     0,     1,
       2,     2,     0,     8,     0,     6,     3,     4,     0,     3,
       0,     4,     0,     4,     1,     1,     3,     1,     2,     2,
       3,     1,     2,     3,     3,    10,     3,     2,     3,     1,
       1,     1,     1,     1,     1,     1,     0,     0,     7,     1,
       3,     1,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     3,     1,     1,     0,     7,     1,     3,
       1,     2,     2,     2,     2,     3,     0,     6,     0,     7,
       4,     6,     0,     6,     0,     7,     4,     6,     1,     3,
       1,     1,     2,     1,     1,     2,     2,     2,     2,     2,
       2,     2,     3,     1,     1,     1,     1,     3,     1,     1,
       1,     3,     1,     1,     5,     1,     3,     1,     5,     7,
       3,     5,     1,     3,     1,     2,     2,     2,     2,     3,
       5,     1,     3,     1,     2,     2,     2,     2,     0,     7,
       0,     9,     0,     1,     3,     1,     2,     2,     2,     2,
       2,     2,     2,     2,     3,     2,     2,     2,     0,     5,
       0,     1,     0,     4,     0,     6,     0,     1,     0,     1,
       2,     0,     1,     1,     2,     1,     1,     1,     2,     0,
       0,     8,     0,    11,     0,     1,     3,     0,     1,     5,
       0,     1,     0,     1,     0,     4,     0,     0,     6,     0,
       1,     0,     1,     1,     0,     2,     1,     3,     3,     1,
       3,     1,     1,     1,     1,     1,     3,     4,     1,     3,
       1,     4,     1,     3,     1,     3,     0,     5,     0,     3,
       0,     5,     0,     0,     7,     0,     4,     1,     1,     1,
       1,     3,     1,     3,     1,     1,     1,     0,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     5,     0,     0,
      10,     1,     1,     0,     1,     4,     0,     7,     0,     1,
       5,     0,     6,     1,     6,     0,     0,     1,     0,     0,
       4,     0,     1,     1,     3,     1,     1,     3,     4,     0,
       4,     1,     1,     3,     3,     1,     3,     1,     0,     1,
       3
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

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
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 6:
#line 524 "fortran.y" /* yacc.c:1646  */
    {yyerrok;yyclearin;}
#line 3545 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 527 "fortran.y" /* yacc.c:1646  */
    {token_since_endofstmt = 0; increment_nbtokens = 0;}
#line 3551 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 540 "fortran.y" /* yacc.c:1646  */
    {
            if (inmoduledeclare == 0 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
            }
        }
#line 3563 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 565 "fortran.y" /* yacc.c:1646  */
    { pos_cur = setposcur(); }
#line 3569 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 601 "fortran.y" /* yacc.c:1646  */
    { Add_Include_1((yyvsp[0].na)); }
#line 3575 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 1123 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3581 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 1124 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3587 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 1125 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3593 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 1126 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3599 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 1127 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3605 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 1129 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),"+"); }
#line 3611 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 1130 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),"-"); }
#line 3617 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 1134 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"+%s",(yyvsp[0].na)); }
#line 3623 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 1135 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"-%s",(yyvsp[0].na)); }
#line 3629 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 1136 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"*%s",(yyvsp[0].na)); }
#line 3635 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 1137 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3641 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 1138 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3647 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 1139 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3653 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 1140 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3659 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 1141 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," > %s",(yyvsp[0].na)); }
#line 3665 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 1142 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," < %s",(yyvsp[0].na)); }
#line 3671 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 1143 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," >= %s",(yyvsp[0].na)); }
#line 3677 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 1144 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," <= %s",(yyvsp[0].na)); }
#line 3683 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 1145 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3689 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 1146 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3695 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 1147 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3701 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 1148 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3707 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 1149 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3713 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 1150 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3719 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 1151 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3725 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 1152 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3731 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 1153 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s",(yyvsp[0].na)); }
#line 3737 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 1154 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s",(yyvsp[0].na)); }
#line 3743 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 1156 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),""); }
#line 3749 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 1157 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"/%s",(yyvsp[0].na)); }
#line 3755 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 1158 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"/= %s",(yyvsp[0].na));}
#line 3761 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 1159 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"//%s",(yyvsp[0].na)); }
#line 3767 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 1162 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"==%s",(yyvsp[0].na)); }
#line 3773 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 1163 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"= %s",(yyvsp[0].na)); }
#line 3779 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 1166 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3785 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 1167 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3791 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 1168 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3797 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 1172 "fortran.y" /* yacc.c:1646  */
    {
            agrif_parentcall = 0;
            if ( !strcasecmp(identcopy, "Agrif_Parent") )   agrif_parentcall = 1;
            if ( Agrif_in_Tok_NAME(identcopy) )
            {
                inagrifcallargument = 1;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
#line 3811 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 1183 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); if ( incalldeclare == 0 ) inagrifcallargument = 0;   }
#line 3817 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 1184 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," %s %s ",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3823 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 1185 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 3829 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 1185 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," %s ( %s )",(yyvsp[-4].na),(yyvsp[-1].na)); }
#line 3835 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 1186 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 3841 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 1186 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," %s ( %s ) %s ",(yyvsp[-5].na),(yyvsp[-2].na),(yyvsp[0].na)); }
#line 3847 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 1189 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 3853 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 1190 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            sprintf((yyval.na)," %s ( %s )",(yyvsp[-4].na),(yyvsp[-1].na));
            ModifyTheAgrifFunction_0((yyvsp[-1].na));
            agrif_parentcall = 0;
        }
#line 3864 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 1199 "fortran.y" /* yacc.c:1646  */
    {
            sprintf((yyval.na)," %s %% %s ",(yyvsp[-3].na),(yyvsp[0].na));
            if ( incalldeclare == 0 ) inagrifcallargument = 0;
        }
#line 3873 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 1210 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na)," "); }
#line 3879 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 1211 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3885 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 1214 "fortran.y" /* yacc.c:1646  */
    {  strcpy((yyval.na),(yyvsp[0].na)); }
#line 3891 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 1215 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na)); }
#line 3897 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 1218 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 3903 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 1219 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 3909 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 1222 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),"%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 3915 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 1223 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),"%s :%s :%s",(yyvsp[-4].na),(yyvsp[-2].na),(yyvsp[0].na));}
#line 3921 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 1224 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),":%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 3927 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 1225 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),": : %s",(yyvsp[0].na));}
#line 3933 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 1226 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),":%s",(yyvsp[0].na));}
#line 3939 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 1227 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),"%s :",(yyvsp[-1].na));}
#line 3945 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 1228 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),":");}
#line 3951 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 1231 "fortran.y" /* yacc.c:1646  */
    {
       //  if (indeclaration == 1) break;
            if ( afterpercent == 0 )
            {
                if ( Agrif_in_Tok_NAME((yyvsp[0].na)) ) Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
                if ( !strcasecmp((yyvsp[0].na),"Agrif_Parent") )   agrif_parentcall = 1;
                if ( VariableIsFunction((yyvsp[0].na)) )
                {
                    if ( inagrifcallargument == 1 )
                    {
                        if ( !strcasecmp((yyvsp[0].na),identcopy) )
                        {
                            strcpy(sameagrifname,identcopy);
                            sameagrifargument = 1;
                        }
                    }
                    strcpy(identcopy,(yyvsp[0].na));
                    pointedvar = 0;

                    if (variscoupled_0((yyvsp[0].na))) strcpy(truename, getcoupledname_0((yyvsp[0].na)));
                    else                    strcpy(truename, (yyvsp[0].na));

                    if ( VarIsNonGridDepend(truename) == 0 && (! Variableshouldberemoved(truename)) )
                    {
                        if ( inagrifcallargument == 1 || varispointer_0(truename) == 1 )
                        {
                            if ( (IsinListe(List_UsedInSubroutine_Var,(yyvsp[0].na)) == 1) || (inagrifcallargument == 1) )
                            {
                                if (varistyped_0(truename) == 0)    ModifyTheVariableName_0(truename,strlen((yyvsp[0].na)));
                            }
                        }
                        if ( inagrifcallargument != 1 || sameagrifargument ==1 )
                        {
                            Add_UsedInSubroutine_Var_1(truename);
                        }
                    }
                    NotifyAgrifFunction_0(truename);
                }
            }
            else
            {
                afterpercent = 0;
            }
        }
#line 4000 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 1277 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),".TRUE.");}
#line 4006 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 1278 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),".FALSE.");}
#line 4012 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 1279 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),"NULL()"); }
#line 4018 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 1280 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4024 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 1281 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4030 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 1282 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4036 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 1284 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 4042 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 1288 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4048 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 1290 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4054 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 1291 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4060 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 101:
#line 1293 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na)," ");}
#line 4066 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 1294 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4072 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 1304 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na)," ");}
#line 4078 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 104:
#line 1305 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4084 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 169:
#line 1503 "fortran.y" /* yacc.c:1646  */
    {
            /* if we never meet the contains keyword               */
            if ( firstpass == 0 )
            {
                RemoveWordCUR_0(fortran_out, strlen((yyvsp[0].na))+11);    // Remove word "end module"
                if ( inmoduledeclare && ! aftercontainsdeclare )
                {
                    Write_Closing_Module(1);
                }
                fprintf(fortran_out,"\n      end module %s\n", curmodulename);
                if ( module_declar && insubroutinedeclare == 0 )
                {
                    fclose(module_declar);
                }
            }
            inmoduledeclare = 0 ;
            inmodulemeet = 0 ;
            aftercontainsdeclare = 1;
            strcpy(curmodulename, "");
            GlobalDeclaration = 0 ;
        }
#line 4110 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 189:
#line 1556 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 4116 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 192:
#line 1580 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 4122 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 196:
#line 1590 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 4128 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 197:
#line 1592 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 4134 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 202:
#line 1612 "fortran.y" /* yacc.c:1646  */
    {pos_cur_decl=my_position_before;}
#line 4140 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 203:
#line 1613 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 4146 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 205:
#line 1616 "fortran.y" /* yacc.c:1646  */
    {strcpy(DeclType,"type"); GlobalDeclarationType = 1;  }
#line 4152 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 206:
#line 1620 "fortran.y" /* yacc.c:1646  */
    {in_kind_selector = 1;}
#line 4158 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 207:
#line 1621 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na)); in_kind_selector =0;}
#line 4164 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 208:
#line 1622 "fortran.y" /* yacc.c:1646  */
    {in_kind_selector = 1;}
#line 4170 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 209:
#line 1623 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_kind_selector =0;}
#line 4176 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 210:
#line 1624 "fortran.y" /* yacc.c:1646  */
    {in_kind_selector = 1;}
#line 4182 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 211:
#line 1625 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,"real"); strcpy(NamePrecision,"8");in_kind_selector =0;}
#line 4188 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 212:
#line 1626 "fortran.y" /* yacc.c:1646  */
    {in_kind_selector = 1;}
#line 4194 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 213:
#line 1627 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_kind_selector =0;}
#line 4200 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 214:
#line 1628 "fortran.y" /* yacc.c:1646  */
    {in_char_selector = 1;}
#line 4206 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 215:
#line 1629 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_char_selector = 0;}
#line 4212 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 216:
#line 1630 "fortran.y" /* yacc.c:1646  */
    {in_kind_selector = 1;}
#line 4218 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 217:
#line 1631 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_kind_selector =0;}
#line 4224 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 218:
#line 1635 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");strcpy(NamePrecision,"");}
#line 4230 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 219:
#line 1637 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 4236 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 220:
#line 1643 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"(%s)",(yyvsp[-1].na)); strcpy(NamePrecision,(yyvsp[-1].na));}
#line 4242 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 221:
#line 1645 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"(KIND=%s)",(yyvsp[-1].na)); strcpy(NamePrecision,(yyvsp[-1].na));}
#line 4248 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 222:
#line 1647 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"*%s",(yyvsp[0].na));strcpy(NamePrecision,(yyvsp[0].na));}
#line 4254 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 224:
#line 1655 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 4260 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 226:
#line 1661 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s_%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 4266 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 230:
#line 1684 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 4272 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 232:
#line 1690 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s_%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 4278 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 233:
#line 1697 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"(%s,%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 4284 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 241:
#line 1715 "fortran.y" /* yacc.c:1646  */
    {char_length_toreset = 1;}
#line 4290 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 242:
#line 1719 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 4296 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 243:
#line 1721 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 4302 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 249:
#line 1734 "fortran.y" /* yacc.c:1646  */
    {strcpy(CharacterSize,(yyvsp[-1].na));}
#line 4308 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 250:
#line 1736 "fortran.y" /* yacc.c:1646  */
    {strcpy(CharacterSize,(yyvsp[-1].na));}
#line 4314 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 253:
#line 1743 "fortran.y" /* yacc.c:1646  */
    {c_star=1; strcpy(CharacterSize,(yyvsp[-1].na));}
#line 4320 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 254:
#line 1745 "fortran.y" /* yacc.c:1646  */
    {c_selectorgiven = 1; strcpy(c_selectorname,(yyvsp[0].na));}
#line 4326 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 260:
#line 1760 "fortran.y" /* yacc.c:1646  */
    { inside_type_declare = 1;}
#line 4332 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 261:
#line 1761 "fortran.y" /* yacc.c:1646  */
    { inside_type_declare = 0;}
#line 4338 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 290:
#line 1827 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 4344 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 295:
#line 1837 "fortran.y" /* yacc.c:1646  */
    {
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            contiguousdeclare = 0 ;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
            GlobalDeclarationType = 0;
         }
#line 4370 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 305:
#line 1881 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 4376 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 306:
#line 1886 "fortran.y" /* yacc.c:1646  */
    {strcpy(NamePrecision,(yyvsp[0].na));}
#line 4382 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 321:
#line 1921 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"(/%s/)",(yyvsp[-1].na));}
#line 4388 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 322:
#line 1923 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"[%s]",(yyvsp[-1].na)); }
#line 4394 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 327:
#line 1951 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 4400 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 330:
#line 1961 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"(%s,%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 4406 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 331:
#line 1966 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s=%s,%s",(yyvsp[-4].na),(yyvsp[-2].na),(yyvsp[0].na));}
#line 4412 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 332:
#line 1968 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s=%s,%s,%s",(yyvsp[-6].na),(yyvsp[-4].na),(yyvsp[-2].na),(yyvsp[0].na));}
#line 4418 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 334:
#line 1976 "fortran.y" /* yacc.c:1646  */
    {indeclaration=1;}
#line 4424 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 335:
#line 1977 "fortran.y" /* yacc.c:1646  */
    {
            /* if the variable is a parameter we can suppose that is*/
            /*    value is the same on each grid. It is not useless */
            /*    to create a copy of it on each grid               */
            if ( ! inside_type_declare )
            {
                pos_end = setposcur();
                //printf("POS = %d %d\n",pos_cur_decl,pos_end);
                RemoveWordSET_0(fortran_out,pos_cur_decl,pos_end-pos_cur_decl);
                ReWriteDeclarationAndAddTosubroutine_01((yyvsp[0].l));
                pos_cur_decl = setposcur();
                if ( firstpass == 0 && GlobalDeclaration == 0
                                    && insubroutinedeclare == 0 )
                {
                    fprintf(fortran_out,"\n#include \"Module_Declar_%s.h\"\n", curmodulename);
                    sprintf(ligne, "Module_Declar_%s.h", curmodulename);
                    module_declar = open_for_write(ligne);
                    GlobalDeclaration = 1 ;
                    pos_cur_decl = setposcur();
                }

                if ( firstpass )
                {
                    Add_Globliste_1((yyvsp[0].l));
                    if ( insubroutinedeclare )
                    {
                        if ( pointerdeclare ) Add_Pointer_Var_From_List_1((yyvsp[0].l));
                        Add_Parameter_Var_1((yyvsp[0].l));
                    }
                    else
                        Add_GlobalParameter_Var_1((yyvsp[0].l));

                    /* If there's a SAVE declaration in module's subroutines we should    */
                    /*    remove it from the subroutines declaration and add it in the    */
                    /*    global declarations                                             */
                                        
                    if ( aftercontainsdeclare && SaveDeclare )
                    {
                        if ( inmodulemeet ) Add_SubroutineDeclarationSave_Var_1((yyvsp[0].l));
                        else                Add_Save_Var_dcl_1((yyvsp[0].l));
                    }
                }
            }
            indeclaration = 0;
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            contiguousdeclare = 0 ;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
            strcpy(DeclType,"");
            GlobalDeclarationType = 0;
        }
#line 4494 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 344:
#line 2061 "fortran.y" /* yacc.c:1646  */
    { Allocatabledeclare = 1; }
#line 4500 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 345:
#line 2063 "fortran.y" /* yacc.c:1646  */
    { contiguousdeclare = 1 ; }
#line 4506 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 346:
#line 2064 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 4512 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 347:
#line 2065 "fortran.y" /* yacc.c:1646  */
    { dimsgiven = 1; curdim = (yyvsp[-1].d); }
#line 4518 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 348:
#line 2067 "fortran.y" /* yacc.c:1646  */
    { ExternalDeclare = 1; }
#line 4524 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 349:
#line 2068 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 4530 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 350:
#line 2069 "fortran.y" /* yacc.c:1646  */
    { strcpy(IntentSpec,(yyvsp[-1].na)); }
#line 4536 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 352:
#line 2072 "fortran.y" /* yacc.c:1646  */
    { optionaldeclare = 1 ; }
#line 4542 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 353:
#line 2074 "fortran.y" /* yacc.c:1646  */
    {VariableIsParameter = 1; }
#line 4548 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 354:
#line 2076 "fortran.y" /* yacc.c:1646  */
    { pointerdeclare = 1 ; }
#line 4554 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 355:
#line 2078 "fortran.y" /* yacc.c:1646  */
    { SaveDeclare = 1 ; }
#line 4560 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 356:
#line 2080 "fortran.y" /* yacc.c:1646  */
    { Targetdeclare = 1; }
#line 4566 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 357:
#line 2085 "fortran.y" /* yacc.c:1646  */
    {(yyval.l)=insertvar(NULL,(yyvsp[0].v));}
#line 4572 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 358:
#line 2087 "fortran.y" /* yacc.c:1646  */
    {(yyval.l)=insertvar((yyvsp[-2].l),(yyvsp[0].v));}
#line 4578 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 359:
#line 2092 "fortran.y" /* yacc.c:1646  */
    {
            if ( ! inside_type_declare )
            {
                if (dimsgiven == 1) curvar = createvar((yyvsp[-3].na),curdim);
                else                curvar = createvar((yyvsp[-3].na),(yyvsp[-2].d));
                CreateAndFillin_Curvar(DeclType, curvar);
                strcpy(curvar->v_typevar,DeclType);
                curvar->v_catvar = get_cat_var(curvar);
                
                if (!strcasecmp(DeclType,"character"))
                {
                    if (c_selectorgiven == 1)
                    {
                        Save_Length(c_selectorname,1);
                        strcpy(curvar->v_dimchar,c_selectorname);
                    }
                }
            }
            strcpy(vallengspec,"");
            if (char_length_toreset == 1)
            {
            c_selectorgiven = 0;
            c_star = 0;
            strcpy(c_selectorname,"");
            strcpy(CharacterSize,"");
            char_length_toreset = 0;
            }
            (yyval.v)=curvar;
        }
#line 4612 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 362:
#line 2131 "fortran.y" /* yacc.c:1646  */
    {InitialValueGiven = 0; }
#line 4618 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 364:
#line 2137 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            strcpy(InitValue,(yyvsp[0].na));
            InitialValueGiven = 1;
        }
#line 4628 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 365:
#line 2143 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            strcpy(InitValue,(yyvsp[0].na));
            InitialValueGiven = 2;
        }
#line 4638 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 366:
#line 2149 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            strcpy(InitValue,(yyvsp[0].na));
            InitialValueGiven = 2;
        }
#line 4648 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 368:
#line 2162 "fortran.y" /* yacc.c:1646  */
    {PublicDeclare = 1;  }
#line 4654 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 369:
#line 2164 "fortran.y" /* yacc.c:1646  */
    {PrivateDeclare = 1;  }
#line 4660 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 370:
#line 2168 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=NULL;}
#line 4666 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 371:
#line 2169 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 4672 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 372:
#line 2170 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[-1].d);}
#line 4678 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 373:
#line 2175 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[0].d);}
#line 4684 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 374:
#line 2177 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[0].d);}
#line 4690 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 375:
#line 2179 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[0].d);}
#line 4696 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 376:
#line 2181 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[0].d);}
#line 4702 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 377:
#line 2183 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[0].d);}
#line 4708 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 378:
#line 2187 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=insertdim(NULL,(yyvsp[0].dim1));
        }
#line 4718 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 379:
#line 2193 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) (yyval.d)=insertdim((yyvsp[-2].d),(yyvsp[0].dim1));
        }
#line 4728 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 380:
#line 2202 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.dim1).first,(yyvsp[-2].na));  Save_Length((yyvsp[-2].na),2); strcpy((yyval.dim1).last,(yyvsp[0].na)); Save_Length((yyvsp[0].na),1); }
#line 4734 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 381:
#line 2204 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.dim1).first,"1"); strcpy((yyval.dim1).last,(yyvsp[0].na)); Save_Length((yyvsp[0].na),1);}
#line 4740 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 382:
#line 2209 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 4746 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 384:
#line 2218 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=insertdim(NULL,(yyvsp[0].dim1));
        }
#line 4756 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 385:
#line 2224 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) (yyval.d)=insertdim((yyvsp[-2].d),(yyvsp[0].dim1));
        }
#line 4766 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 386:
#line 2233 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.dim1).first,"");  strcpy((yyval.dim1).last,"");  }
#line 4772 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 387:
#line 2235 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.dim1).first,(yyvsp[-1].na));  Save_Length((yyvsp[-1].na),2); strcpy((yyval.dim1).last,""); }
#line 4778 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 388:
#line 2240 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=insertdim(NULL,(yyvsp[0].dim1));
        }
#line 4788 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 389:
#line 2246 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) (yyval.d)=insertdim((yyvsp[-2].d),(yyvsp[0].dim1));
        }
#line 4798 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 390:
#line 2255 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.dim1).first,"");  strcpy((yyval.dim1).last,"");  }
#line 4804 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 391:
#line 2260 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 ) 
            {
            if (!strcasecmp((yyvsp[-1].na),""))
            {
            strcpy(my_dim.first,"1");
            }
            else
            {
            strcpy(my_dim.first,(yyvsp[-1].na));
            }
            strcpy(my_dim.last,"*");
            (yyval.d)=insertdim((yyvsp[-2].d),my_dim);
            strcpy(my_dim.first,"");
            strcpy(my_dim.last,"");
            }
        }
#line 4828 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 392:
#line 2282 "fortran.y" /* yacc.c:1646  */
    {(yyval.d) = (listdim *) NULL;}
#line 4834 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 393:
#line 2284 "fortran.y" /* yacc.c:1646  */
    {(yyval.d) = (yyvsp[-1].d);}
#line 4840 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 394:
#line 2302 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 4846 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 395:
#line 2304 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[-1].na));}
#line 4852 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 399:
#line 2317 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4858 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 400:
#line 2319 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4864 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 401:
#line 2321 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4870 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 402:
#line 2326 "fortran.y" /* yacc.c:1646  */
    {
            if ((firstpass == 0) && (PublicDeclare == 1))
            {
                if ((yyvsp[0].lnn))
                {
                    removeglobfromlist(&((yyvsp[0].lnn)));
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_cur,pos_end-pos_cur);
                    writelistpublic((yyvsp[0].lnn));
                }
            }
     PublicDeclare = 0;
     PrivateDeclare = 0;
     }
#line 4889 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 404:
#line 2344 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn)=(listname *)NULL;}
#line 4895 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 405:
#line 2346 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn)=(yyvsp[0].lnn);}
#line 4901 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 406:
#line 2350 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn)=Insertname(NULL,(yyvsp[0].na),0);}
#line 4907 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 407:
#line 2352 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn)=Insertname((yyvsp[-2].lnn),(yyvsp[0].na),0);}
#line 4913 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 410:
#line 2362 "fortran.y" /* yacc.c:1646  */
    {
            /* we should remove the data declaration                */
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curdata,pos_end-pos_curdata);
            if ( aftercontainsdeclare == 1  && firstpass == 0 )
            {
                ReWriteDataStatement_0(fortran_out);
                pos_end = setposcur();
            }
            Init_List_Data_Var();
        }
#line 4929 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 416:
#line 2386 "fortran.y" /* yacc.c:1646  */
    {
            if (firstpass == 1)  
            {
            Add_Data_Var_Names_01(&List_Data_Var,(yyvsp[-3].l),(yyvsp[-1].lnn));
            }
            else                 Add_Data_Var_Names_01(&List_Data_Var_Cur,(yyvsp[-3].l),(yyvsp[-1].lnn));
        }
#line 4941 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 417:
#line 2396 "fortran.y" /* yacc.c:1646  */
    { (yyval.l)=insertvar(NULL,(yyvsp[0].v)); }
#line 4947 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 418:
#line 2398 "fortran.y" /* yacc.c:1646  */
    {
     (yyval.l) = insertvar((yyvsp[-2].l),(yyvsp[0].v));
     }
#line 4955 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 419:
#line 2404 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn)=Insertname(NULL,(yyvsp[0].na),0);}
#line 4961 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 420:
#line 2406 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn) = Insertname((yyvsp[-2].lnn),(yyvsp[0].na),1);   }
#line 4967 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 423:
#line 2416 "fortran.y" /* yacc.c:1646  */
    {printf("DOVARIABLE = %s %s %s\n",(yyvsp[-5].na),(yyvsp[-3].na),(yyvsp[-1].na));
     printf("AUTRE = %s %s\n",(yyvsp[-7].l)->var->v_nomvar,(yyvsp[-7].l)->var->v_initialvalue_array);
     Insertdoloop((yyvsp[-7].l)->var,(yyvsp[-5].na),(yyvsp[-3].na),(yyvsp[-1].na),"");
     (yyval.v)=(yyvsp[-7].l)->var;
     }
#line 4977 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 424:
#line 2422 "fortran.y" /* yacc.c:1646  */
    {
     Insertdoloop((yyvsp[-9].l)->var,(yyvsp[-7].na),(yyvsp[-5].na),(yyvsp[-3].na),(yyvsp[-1].na));
     (yyval.v)=(yyvsp[-9].l)->var;
     }
#line 4986 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 425:
#line 2429 "fortran.y" /* yacc.c:1646  */
    {(yyval.l)=insertvar(NULL,(yyvsp[0].v));}
#line 4992 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 426:
#line 2431 "fortran.y" /* yacc.c:1646  */
    {(yyval.l) = insertvar((yyvsp[-2].l),(yyvsp[0].v));}
#line 4998 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 428:
#line 2437 "fortran.y" /* yacc.c:1646  */
    {(yyval.v)->v_initialvalue_array=Insertname((yyval.v)->v_initialvalue_array,my_dim.last,0);
     strcpy(my_dim.last,"");
     }
#line 5006 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 431:
#line 2450 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 5012 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 432:
#line 2452 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 5018 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 433:
#line 2454 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 5024 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 439:
#line 2463 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 5030 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 440:
#line 2465 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"*%s",(yyvsp[0].na));}
#line 5036 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 449:
#line 2501 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5042 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 450:
#line 2505 "fortran.y" /* yacc.c:1646  */
    {positioninblock = 0; pos_curdimension = my_position_before;}
#line 5048 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 451:
#line 2507 "fortran.y" /* yacc.c:1646  */
    {
            /* if the variable is a parameter we can suppose that is   */
            /*    value is the same on each grid. It is not useless to */
            /*    create a copy of it on each grid                     */
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    Add_Globliste_1((yyvsp[0].l));
                    /* if variableparamlists has been declared in a subroutine   */
                    if ( insubroutinedeclare )     Add_Dimension_Var_1((yyvsp[0].l));
                    
                    /* Add it to the List_SubroutineDeclaration_Var list if not present */
                    /* NB: if not done, a variable declared with DIMENSION but with no type given */
                    /* will not be declared by the conv */
                    ReWriteDeclarationAndAddTosubroutine_01((yyvsp[0].l));
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curdimension,pos_end-pos_curdimension);
                    ReWriteDeclarationAndAddTosubroutine_01((yyvsp[0].l));
                }
            }
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            contiguousdeclare = 0 ;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
        }
#line 5096 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 453:
#line 2553 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal = 0;}
#line 5102 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 454:
#line 2554 "fortran.y" /* yacc.c:1646  */
    {
        if ( inside_type_declare ) break;
        curvar = createvar((yyvsp[-4].na),(yyvsp[-1].d));
        CreateAndFillin_Curvar("", curvar);
        curlistvar=insertvar(NULL, curvar);
        (yyval.l) = settype("",curlistvar);
        strcpy(vallengspec,"");
     }
#line 5115 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 455:
#line 2562 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal = 0;}
#line 5121 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 456:
#line 2563 "fortran.y" /* yacc.c:1646  */
    {
        if ( inside_type_declare ) break;
        curvar = createvar((yyvsp[-4].na),(yyvsp[-1].d));
        CreateAndFillin_Curvar("", curvar);
        curlistvar = insertvar((yyvsp[-6].l), curvar);
        (yyval.l) = curlistvar;
        strcpy(vallengspec,"");
        }
#line 5134 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 457:
#line 2575 "fortran.y" /* yacc.c:1646  */
    { VariableIsParameter = 1; pos_curparameter = setposcur()-9; }
#line 5140 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 458:
#line 2576 "fortran.y" /* yacc.c:1646  */
    {
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    if ( insubroutinedeclare )  Add_Parameter_Var_1((yyvsp[-1].l));
                    else                        Add_GlobalParameter_Var_1((yyvsp[-1].l));
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out, pos_curparameter, pos_end-pos_curparameter);
                }
            }
            VariableIsParameter =  0 ;
        }
#line 5161 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 460:
#line 2596 "fortran.y" /* yacc.c:1646  */
    {(yyval.l)=insertvar(NULL,(yyvsp[0].v));}
#line 5167 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 461:
#line 2598 "fortran.y" /* yacc.c:1646  */
    {(yyval.l)=insertvar((yyvsp[-2].l),(yyvsp[0].v));}
#line 5173 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 462:
#line 2603 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            curvar=(variable *) calloc(1,sizeof(variable));
            Init_Variable(curvar);
            curvar->v_VariableIsParameter = 1;
            strcpy(curvar->v_nomvar,(yyvsp[-2].na));
            strcpy(curvar->v_subroutinename,subroutinename);
            strcpy(curvar->v_modulename,curmodulename);
            curvar->v_initialvalue=Insertname(curvar->v_initialvalue,(yyvsp[0].na),0);
            strcpy(curvar->v_commoninfile,cur_filename);
            Save_Length((yyvsp[0].na),14);
            (yyval.v) = curvar;
        }
#line 5191 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 463:
#line 2619 "fortran.y" /* yacc.c:1646  */
    {pos_cursave = my_position_before;}
#line 5197 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 464:
#line 2620 "fortran.y" /* yacc.c:1646  */
    {
     pos_end = setposcur();
     RemoveWordSET_0(fortran_out,pos_cursave,pos_end-pos_cursave);
     }
#line 5206 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 472:
#line 2641 "fortran.y" /* yacc.c:1646  */
    {if ( ! inside_type_declare ) Add_Save_Var_1((yyvsp[0].na),(listdim*) NULL); }
#line 5212 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 476:
#line 2651 "fortran.y" /* yacc.c:1646  */
    {my_position = my_position_before;}
#line 5218 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 478:
#line 2657 "fortran.y" /* yacc.c:1646  */
    {
            if ( insubroutinedeclare == 1 )
            {
                Add_ImplicitNoneSubroutine_1();
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
            }
        }
#line 5231 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 496:
#line 2709 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5237 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 503:
#line 2724 "fortran.y" /* yacc.c:1646  */
    { positioninblock = 0; pos_curcommon = my_position_before; indeclaration=1;}
#line 5243 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 504:
#line 2725 "fortran.y" /* yacc.c:1646  */
    {
            indeclaration = 0;
            if ( inside_type_declare ) break;
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
     }
#line 5254 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 507:
#line 2736 "fortran.y" /* yacc.c:1646  */
    {
     if ( inside_type_declare ) break;
     sprintf(charusemodule,"%s",(yyvsp[0].na));
     Add_NameOfCommon_1((yyvsp[0].na),subroutinename);
     }
#line 5264 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 508:
#line 2744 "fortran.y" /* yacc.c:1646  */
    {
            strcpy((yyval.na),"");
            positioninblock=0;
            strcpy(commonblockname,"");
        }
#line 5274 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 509:
#line 2750 "fortran.y" /* yacc.c:1646  */
    {
            strcpy((yyval.na),(yyvsp[-1].na));
            positioninblock=0;
            strcpy(commonblockname,(yyvsp[-1].na));
        }
#line 5284 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 513:
#line 2763 "fortran.y" /* yacc.c:1646  */
    {
     if ( inside_type_declare ) break;
     sprintf(charusemodule,"%s",(yyvsp[0].na));
     Add_NameOfCommon_1((yyvsp[0].na),subroutinename);
     }
#line 5294 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 515:
#line 2773 "fortran.y" /* yacc.c:1646  */
    {if ( ! inside_type_declare ) Add_Common_var_1(); }
#line 5300 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 516:
#line 2775 "fortran.y" /* yacc.c:1646  */
    {if ( ! inside_type_declare ) Add_Common_var_1(); }
#line 5306 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 517:
#line 2783 "fortran.y" /* yacc.c:1646  */
    {
            positioninblock = positioninblock + 1 ;
            strcpy(commonvar,(yyvsp[0].na));
            commondim = (listdim*) NULL;
        }
#line 5316 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 518:
#line 2788 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5322 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 519:
#line 2789 "fortran.y" /* yacc.c:1646  */
    {
            positioninblock = positioninblock + 1 ;
            strcpy(commonvar,(yyvsp[-4].na));
            commondim = (yyvsp[-1].d);
        }
#line 5332 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 523:
#line 2801 "fortran.y" /* yacc.c:1646  */
    {(yyval.v)=createvar((yyvsp[0].na),NULL);}
#line 5338 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 525:
#line 2813 "fortran.y" /* yacc.c:1646  */
    {if (strcmp(my_dim.last,""))
       {
       (yyval.v)->v_initialvalue_array=Insertname(NULL,my_dim.last,0);
       }
       strcpy(my_dim.last,"");
       }
#line 5349 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 535:
#line 2855 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s(%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 5355 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 536:
#line 2857 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s(%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 5361 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 537:
#line 2872 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s:%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5367 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 538:
#line 2877 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].v)->v_nomvar,(yyvsp[0].na));}
#line 5373 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 539:
#line 2881 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 5379 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 540:
#line 2883 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%%%s",(yyvsp[-2].na),(yyvsp[0].v)->v_nomvar);}
#line 5385 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 541:
#line 2888 "fortran.y" /* yacc.c:1646  */
    {(yyval.v)=createvar((yyvsp[0].na),NULL);}
#line 5391 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 542:
#line 2889 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5397 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 543:
#line 2890 "fortran.y" /* yacc.c:1646  */
    {sprintf(ligne,"%s(%s)",(yyvsp[-4].na),(yyvsp[-1].na));(yyval.v)=createvar((yyvsp[-4].na),NULL);strcpy(my_dim.last,(yyvsp[-1].na));}
#line 5403 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 545:
#line 2906 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5409 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 546:
#line 2911 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5415 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 547:
#line 2916 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5421 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 548:
#line 2918 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5427 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 549:
#line 2924 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 5433 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 550:
#line 2926 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 5439 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 551:
#line 2928 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5445 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 552:
#line 2950 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 5451 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 553:
#line 2952 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),":");}
#line 5457 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 554:
#line 2954 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),":%s",(yyvsp[0].na));}
#line 5463 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 555:
#line 2956 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),": :%s",(yyvsp[0].na));}
#line 5469 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 556:
#line 2958 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),":%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5475 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 557:
#line 2960 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"::%s",(yyvsp[0].na));}
#line 5481 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 559:
#line 2963 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s=%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5487 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 560:
#line 2965 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s=*%s",(yyvsp[-3].na),(yyvsp[0].na));}
#line 5493 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 561:
#line 2967 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"*%s",(yyvsp[0].na));}
#line 5499 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 562:
#line 2971 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),":");}
#line 5505 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 563:
#line 2973 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),":%s",(yyvsp[0].na));}
#line 5511 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 564:
#line 2975 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),": :%s",(yyvsp[0].na));}
#line 5517 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 565:
#line 2977 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),":%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5523 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 566:
#line 2979 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"::%s",(yyvsp[0].na));}
#line 5529 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 567:
#line 2981 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 5535 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 569:
#line 2999 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5541 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 570:
#line 3000 "fortran.y" /* yacc.c:1646  */
    {inallocate = 0;}
#line 5547 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 594:
#line 3070 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5553 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 595:
#line 3071 "fortran.y" /* yacc.c:1646  */
    {inallocate = 0;}
#line 5559 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 605:
#line 3101 "fortran.y" /* yacc.c:1646  */
    {
      strcpy((yyval.na),(yyvsp[0].v)->v_nomvar);
      if (strcasecmp(my_dim.last,""))
      {
      strcat((yyval.na),"(");
      strcat((yyval.na),my_dim.last);
      strcat((yyval.na),")");
      }
      }
#line 5573 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 609:
#line 3114 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"(%s)",(yyvsp[-1].na));}
#line 5579 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 610:
#line 3119 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5585 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 612:
#line 3125 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s**%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5591 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 614:
#line 3130 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5597 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 616:
#line 3138 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5603 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 617:
#line 3140 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5609 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 619:
#line 3143 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5615 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 621:
#line 3152 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"*");}
#line 5621 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 623:
#line 3158 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"+");}
#line 5627 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 624:
#line 3160 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"-");}
#line 5633 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 626:
#line 3166 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5639 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 629:
#line 3175 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5645 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 638:
#line 3188 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"<");}
#line 5651 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 640:
#line 3191 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),">");}
#line 5657 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 643:
#line 3199 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5663 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 645:
#line 3206 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5669 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 647:
#line 3213 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5675 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 649:
#line 3219 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5681 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 659:
#line 3255 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 5687 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 662:
#line 3264 "fortran.y" /* yacc.c:1646  */
    {
     strcpy((yyval.na),(yyvsp[0].na));
     }
#line 5695 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 663:
#line 3271 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 5701 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 792:
#line 3644 "fortran.y" /* yacc.c:1646  */
    {in_select_case_stmt++;}
#line 5707 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 794:
#line 3645 "fortran.y" /* yacc.c:1646  */
    {in_select_case_stmt++;}
#line 5713 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 798:
#line 3654 "fortran.y" /* yacc.c:1646  */
    {in_select_case_stmt--;}
#line 5719 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 800:
#line 3655 "fortran.y" /* yacc.c:1646  */
    {in_select_case_stmt--;}
#line 5725 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 802:
#line 3660 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5731 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 826:
#line 3723 "fortran.y" /* yacc.c:1646  */
    {close_or_connect = 1;}
#line 5737 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 827:
#line 3723 "fortran.y" /* yacc.c:1646  */
    {close_or_connect = 0;}
#line 5743 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 846:
#line 3755 "fortran.y" /* yacc.c:1646  */
    {close_or_connect = 1;}
#line 5749 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 847:
#line 3756 "fortran.y" /* yacc.c:1646  */
    {close_or_connect = 0;}
#line 5755 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 856:
#line 3774 "fortran.y" /* yacc.c:1646  */
    {
         in_io_control_spec = 0;
         }
#line 5763 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 858:
#line 3779 "fortran.y" /* yacc.c:1646  */
    {
         in_io_control_spec = 0;
         }
#line 5771 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 862:
#line 3789 "fortran.y" /* yacc.c:1646  */
    {
         in_io_control_spec = 0;
         }
#line 5779 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 864:
#line 3794 "fortran.y" /* yacc.c:1646  */
    {
         in_io_control_spec = 0;
         }
#line 5787 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 918:
#line 3912 "fortran.y" /* yacc.c:1646  */
    {in_inquire=0;}
#line 5793 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 920:
#line 3915 "fortran.y" /* yacc.c:1646  */
    {in_inquire=0;}
#line 5799 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 922:
#line 3919 "fortran.y" /* yacc.c:1646  */
    {in_inquire=1;}
#line 5805 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 938:
#line 3947 "fortran.y" /* yacc.c:1646  */
    {pos_endsubroutine=setposcur();}
#line 5811 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 942:
#line 3956 "fortran.y" /* yacc.c:1646  */
    {
            GlobalDeclaration = 0;
            strcpy(curmodulename,(yyvsp[0].na));
            strcpy(subroutinename,"");
            Add_NameOfModule_1((yyvsp[0].na));
            if ( inmoduledeclare == 0 )
            {
                /* To know if there are in the module declaration    */
                inmoduledeclare = 1;
                /* to know if a module has been met                  */
                inmodulemeet = 1;
                /* to know if we are after the keyword contains      */
                aftercontainsdeclare = 0 ;
            }
        }
#line 5831 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 944:
#line 3976 "fortran.y" /* yacc.c:1646  */
    {
            /* if we never meet the contains keyword               */
            if ( firstpass == 0 )
            {
                RemoveWordCUR_0(fortran_out, setposcur()-my_position);    // Remove word "end module"
                if ( inmoduledeclare && ! aftercontainsdeclare )
                {
                    Write_Closing_Module(1);
                }
                fprintf(fortran_out,"\n      end module %s\n", curmodulename);
                if ( module_declar && insubroutinedeclare == 0 )
                {
                    fclose(module_declar);
                }
            }
            inmoduledeclare = 0 ;
            inmodulemeet = 0 ;
            aftercontainsdeclare = 1;
            strcpy(curmodulename, "");
            GlobalDeclaration = 0 ;
        }
#line 5857 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 959:
#line 4028 "fortran.y" /* yacc.c:1646  */
    {if (firstpass == 0 && oldfortran_out) pos_curuseold = setposcurname(oldfortran_out);}
#line 5863 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 960:
#line 4033 "fortran.y" /* yacc.c:1646  */
    {
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                    if ((yyvsp[0].lc)) {
                      Add_CouplePointed_Var_1((yyvsp[-1].na),(yyvsp[0].lc));
                      coupletmp = (yyvsp[0].lc);
                      strcpy(ligne,"");
                      while ( coupletmp )
                      {
                        strcat(ligne, coupletmp->c_namevar);
                        strcat(ligne, " => ");
                        strcat(ligne, coupletmp->c_namepointedvar);
                        coupletmp = coupletmp->suiv;
                        if ( coupletmp ) strcat(ligne,",");
                      }
                      }
                  sprintf(charusemodule,"%s",(yyvsp[-1].na));
                }
                Add_NameOfModuleUsed_1((yyvsp[-1].na));
            }
            else
            {
                if ( insubroutinedeclare )
                {
                  copyuse_0((yyvsp[-1].na));
                    }

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
                }
            }
    }
#line 5904 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 962:
#line 4071 "fortran.y" /* yacc.c:1646  */
    {
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                  if ((yyvsp[0].lc))
                  {
                    Add_CouplePointed_Var_1((yyvsp[-4].na),(yyvsp[0].lc));
                    coupletmp = (yyvsp[0].lc);
                    strcpy(ligne,"");
                    while ( coupletmp )
                    {
                        strcat(ligne,coupletmp->c_namevar);
                        if ( strcasecmp(coupletmp->c_namepointedvar,"") )   strcat(ligne," => ");
                        strcat(ligne,coupletmp->c_namepointedvar);
                        coupletmp = coupletmp->suiv;
                        if ( coupletmp ) strcat(ligne,",");
                    }
                  }
                  sprintf(charusemodule,"%s",(yyvsp[-4].na));
                }
                Add_NameOfModuleUsed_1((yyvsp[-4].na));
            }
            else
            {
                if ( insubroutinedeclare )
                    copyuseonly_0((yyvsp[-4].na));

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
                    if ((yyvsp[0].lc))
                    {
                    if (oldfortran_out)  variableisglobalinmodule((yyvsp[0].lc),(yyvsp[-4].na),oldfortran_out,pos_curuseold);
                    }
                }
                else
                {
                  if ((yyvsp[0].lc))
                  {
                    /* if we are in the module declare and if the    */
                    /* onlylist is a list of global variable         */
                    variableisglobalinmodule((yyvsp[0].lc), (yyvsp[-4].na), fortran_out,my_position);
                  }
                }
            }
    }
#line 5957 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 967:
#line 4128 "fortran.y" /* yacc.c:1646  */
    {(yyval.lc)=NULL;}
#line 5963 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 968:
#line 4130 "fortran.y" /* yacc.c:1646  */
    {(yyval.lc)=(yyvsp[0].lc);}
#line 5969 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 974:
#line 4147 "fortran.y" /* yacc.c:1646  */
    {
            strcpy(subroutinename,(yyvsp[0].na));
            insubroutinedeclare = 1;
            inprogramdeclare = 1;
            /* in the second step we should write the head of       */
            /*    the subroutine sub_loop_<subroutinename>          */
            if ( ! firstpass )
                WriteBeginof_SubLoop();
        }
#line 5983 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 976:
#line 4160 "fortran.y" /* yacc.c:1646  */
    {pos_endsubroutine=my_position_before;}
#line 5989 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 977:
#line 4161 "fortran.y" /* yacc.c:1646  */
    {
            insubroutinedeclare = 0;
            inprogramdeclare = 0;
            pos_cur = setposcur();
            closeandcallsubloopandincludeit_0(3);
            functiondeclarationisdone = 0;
            strcpy(subroutinename,"");     
     }
#line 6002 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 984:
#line 4183 "fortran.y" /* yacc.c:1646  */
    {
    (yyval.lc)=NULL;
    }
#line 6010 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 985:
#line 4187 "fortran.y" /* yacc.c:1646  */
    {
    (yyval.lc)=(yyvsp[0].lc);
    }
#line 6018 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 986:
#line 4193 "fortran.y" /* yacc.c:1646  */
    {
     (yyval.lc)=(yyvsp[0].lc);
     }
#line 6026 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 987:
#line 4197 "fortran.y" /* yacc.c:1646  */
    {
     /* insert the variable in the list $1                 */
     (yyvsp[0].lc)->suiv = (yyvsp[-2].lc);
     (yyval.lc)=(yyvsp[0].lc);
     }
#line 6036 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 988:
#line 4206 "fortran.y" /* yacc.c:1646  */
    {
            coupletmp = (listcouple *) calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[-2].na));
            strcpy(coupletmp->c_namepointedvar,(yyvsp[0].na));
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
        }
#line 6048 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 989:
#line 4216 "fortran.y" /* yacc.c:1646  */
    {(yyval.lc)=(yyvsp[0].lc);}
#line 6054 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 990:
#line 4218 "fortran.y" /* yacc.c:1646  */
    {
            /* insert the variable in the list $1                 */
            (yyvsp[0].lc)->suiv = (yyvsp[-2].lc);
            (yyval.lc) = (yyvsp[0].lc);
        }
#line 6064 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 991:
#line 4227 "fortran.y" /* yacc.c:1646  */
    {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[0].na));
            strcpy(coupletmp->c_namepointedvar,"");
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
        }
#line 6076 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 992:
#line 4235 "fortran.y" /* yacc.c:1646  */
    {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[0].na));
            strcpy(coupletmp->c_namepointedvar,"");
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
        }
#line 6088 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 993:
#line 4243 "fortran.y" /* yacc.c:1646  */
    {
     (yyval.lc)=(yyvsp[0].lc);
     pointedvar = 1;
      Add_UsedInSubroutine_Var_1((yyvsp[0].lc)->c_namevar);
     }
#line 6098 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1006:
#line 4283 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 6104 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1007:
#line 4284 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s(%s)",(yyvsp[-4].na),(yyvsp[-1].na));}
#line 6110 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1008:
#line 4290 "fortran.y" /* yacc.c:1646  */
    {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
#line 6126 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1010:
#line 4303 "fortran.y" /* yacc.c:1646  */
    {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
#line 6142 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1012:
#line 4315 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 6148 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1013:
#line 4316 "fortran.y" /* yacc.c:1646  */
    {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
#line 6164 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1015:
#line 4330 "fortran.y" /* yacc.c:1646  */
    {pos_curcall=my_position_before-strlen((yyvsp[-1].na))-4;}
#line 6170 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1016:
#line 4331 "fortran.y" /* yacc.c:1646  */
    {
            if (!strcasecmp((yyvsp[0].na),"MPI_Init") )    callmpiinit = 1;
            else                                callmpiinit = 0;

            if (!strcasecmp((yyvsp[0].na),"Agrif_Init_Grids") )
            {
                callagrifinitgrids = 1;
                strcpy(meetagrifinitgrids,subroutinename);
            }
            else
            {
                callagrifinitgrids = 0;
            }
            if ( Vartonumber((yyvsp[0].na)) == 1 )
            {
                incalldeclare = 0;
                inagrifcallargument = 0 ;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
#line 6195 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1021:
#line 4362 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 6201 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1022:
#line 4367 "fortran.y" /* yacc.c:1646  */
    {
            if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,(yyvsp[0].na));
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
        }
#line 6213 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1023:
#line 4375 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s = %s",(yyvsp[-2].na),(yyvsp[0].na));
                 if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,(yyvsp[0].na));
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
            }
#line 6225 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1025:
#line 4387 "fortran.y" /* yacc.c:1646  */
    {
     strcpy((yyval.na),(yyvsp[0].v)->v_nomvar);
     if ((yyvsp[0].v)->v_initialvalue_array)
     {
     strcat((yyval.na),"(");
     strcat((yyval.na),(yyvsp[0].v)->v_initialvalue_array->n_name);
     strcat((yyval.na),")");
     }
     }
#line 6239 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1027:
#line 4399 "fortran.y" /* yacc.c:1646  */
    {isrecursive = 0; ispure = 0; isimpure = 0; iselemental = 0;}
#line 6245 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1031:
#line 4410 "fortran.y" /* yacc.c:1646  */
    {isrecursive = 0; ispure = 0; isimpure = 0; iselemental = 0; functiondeclarationisdone = 1;}
#line 6251 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1032:
#line 4412 "fortran.y" /* yacc.c:1646  */
    {isrecursive = 0; ispure = 0; isimpure = 0; iselemental = 0;}
#line 6257 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1033:
#line 4414 "fortran.y" /* yacc.c:1646  */
    {isrecursive = 1;}
#line 6263 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1034:
#line 4416 "fortran.y" /* yacc.c:1646  */
    {ispure = 1;}
#line 6269 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1035:
#line 4418 "fortran.y" /* yacc.c:1646  */
    {isimpure = 1;}
#line 6275 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1036:
#line 4420 "fortran.y" /* yacc.c:1646  */
    {iselemental = 1;}
#line 6281 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1038:
#line 4429 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 6287 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1039:
#line 4430 "fortran.y" /* yacc.c:1646  */
    {
            insubroutinedeclare = 1;
            suborfun = 0;
            /* we should to list of the subroutine argument the  */
            /*    name of the function which has to be defined   */
            if ( firstpass )
            {
                Add_SubroutineArgument_Var_1((yyvsp[-2].l));
                if ( ! is_result_present )
                    Add_FunctionType_Var_1((yyvsp[-5].na));
            }
            else
            /* in the second step we should write the head of    */
            /*    the subroutine sub_loop_<subroutinename>       */
               {
                if (todebug == 1) fprintf(fortran_out,"      !DEBUG: Avant Writebeginof subloop\n");
                WriteBeginof_SubLoop();
                if (todebug == 1) fprintf(fortran_out,"      !DEBUG: Apres Writebeginof subloop\n");
                }
                strcpy(NamePrecision,"");
     }
#line 6313 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1041:
#line 4455 "fortran.y" /* yacc.c:1646  */
    {
     if (strcmp(subroutinename,""))
     {
     strcpy(old_subroutinename,subroutinename); // can occur in internal-subprogram
     old_oldfortran_out=oldfortran_out;
     }
     else
     {
     old_oldfortran_out=(FILE *)NULL;
     }
     strcpy((yyval.na),(yyvsp[0].na));strcpy(subroutinename,(yyvsp[0].na));
     }
#line 6330 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1042:
#line 4480 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 6336 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1043:
#line 4484 "fortran.y" /* yacc.c:1646  */
    {is_result_present = 0; }
#line 6342 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1045:
#line 4490 "fortran.y" /* yacc.c:1646  */
    {is_result_present = 1;
                 if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[-1].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                strcpy(curvar->v_typevar,"");
                curlistvar = insertvar(NULL,curvar);
                Add_SubroutineArgument_Var_1(curlistvar);
            }
     }
#line 6359 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1046:
#line 4506 "fortran.y" /* yacc.c:1646  */
    {strcpy(DeclType, "");}
#line 6365 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1051:
#line 4520 "fortran.y" /* yacc.c:1646  */
    {
            insubroutinedeclare = 1;
            suborfun = 1;
            if ( firstpass )
                Add_SubroutineArgument_Var_1((yyvsp[0].l));
            else
              {
                WriteBeginof_SubLoop();
              }
        }
#line 6380 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1053:
#line 4535 "fortran.y" /* yacc.c:1646  */
    {
     if (strcmp(subroutinename,""))
     {
     strcpy(old_subroutinename,subroutinename); // can occur in internal-subprogram
     old_oldfortran_out=oldfortran_out;
     }
     else
     {
     old_oldfortran_out=(FILE *)NULL;
     }
     strcpy((yyval.na),(yyvsp[0].na));strcpy(subroutinename,(yyvsp[0].na));
     }
#line 6397 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1055:
#line 4556 "fortran.y" /* yacc.c:1646  */
    {pos_endsubroutine = my_position;
            GlobalDeclaration = 0 ;
            if ( firstpass == 0 && strcasecmp(subroutinename,"") )
            {
                if ( module_declar && insubroutinedeclare == 0 )    fclose(module_declar);
            }
            if ( strcasecmp(subroutinename,"") )
            {
                if ( inmodulemeet == 1 )
                {
                    /* we are in a module                                */
                    if ( insubroutinedeclare == 1 )
                    {
                        /* it is like an end subroutine <name>            */
                        insubroutinedeclare = 0 ;
                        pos_cur = setposcur();
                        closeandcallsubloopandincludeit_0(suborfun);
                        functiondeclarationisdone = 0;
                    }
                    else
                    {
                        /* it is like an end module <name>                */
                        inmoduledeclare = 0 ;
                        inmodulemeet = 0 ;
                    }
                }
                else
                {
                    insubroutinedeclare = 0;
                    pos_cur = setposcur();
                    closeandcallsubloopandincludeit_0(2);
                    functiondeclarationisdone = 0;
                }
            }
            strcpy(subroutinename,"");
            if (strcmp(old_subroutinename,""))
            {
            strcpy(subroutinename,old_subroutinename);
            strcpy(old_subroutinename,"");
            oldfortran_out=old_oldfortran_out;
            insubroutinedeclare=1;
            }
        }
#line 6445 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1058:
#line 4605 "fortran.y" /* yacc.c:1646  */
    {if (firstpass) (yyval.l)=NULL;}
#line 6451 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1059:
#line 4606 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 6457 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1060:
#line 4607 "fortran.y" /* yacc.c:1646  */
    {if (firstpass) (yyval.l)=(yyvsp[-1].l);}
#line 6463 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1061:
#line 4611 "fortran.y" /* yacc.c:1646  */
    {if (firstpass) (yyval.l)=NULL;}
#line 6469 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1062:
#line 4613 "fortran.y" /* yacc.c:1646  */
    {if (firstpass) (yyval.l)=(yyvsp[0].l);}
#line 6475 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1063:
#line 4618 "fortran.y" /* yacc.c:1646  */
    {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[0].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                curlistvar = insertvar(NULL,curvar);
                (yyval.l) = settype("",curlistvar);
            }
        }
#line 6491 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1064:
#line 4630 "fortran.y" /* yacc.c:1646  */
    {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[0].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                (yyval.l) = insertvar((yyvsp[-2].l),curvar);
            }
        }
#line 6506 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1065:
#line 4644 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 6512 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1066:
#line 4646 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"*");}
#line 6518 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1069:
#line 4656 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            if ( inmoduledeclare )
            {
                if ( firstpass == 0 )
                {
                    RemoveWordCUR_0(fortran_out,9);   // Remove word 'contains'
                    Write_Closing_Module(0);
                }
                inmoduledeclare = 0 ;
                aftercontainsdeclare = 1;
            }
            else if ( insubroutinedeclare )
            {
                incontainssubroutine = 1;
                insubroutinedeclare  = 0;
                incontainssubroutine = 0;
                functiondeclarationisdone = 0;

                if ( firstpass )
                    List_ContainsSubroutine = Addtolistnom(subroutinename, List_ContainsSubroutine, 0);
                else
                    closeandcallsubloop_contains_0();

                strcpy(subroutinename, "");
            }
            else printf("l.%4d -- TOK_CONTAINS -- MHCHECK\n",line_num_input);
        }
#line 6551 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1071:
#line 4691 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 6557 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1072:
#line 4692 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 6563 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1078:
#line 4820 "fortran.y" /* yacc.c:1646  */
    { afterpercent = 1; }
#line 6569 "fortran.tab.c" /* yacc.c:1646  */
    break;


#line 6573 "fortran.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

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

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 4917 "fortran.y" /* yacc.c:1906  */


void process_fortran(const char *input_file)
{
    extern FILE *fortran_in;
    extern FILE *fortran_out;

    char output_file[LONG_FNAME];
    char input_fullpath[LONG_FNAME];

    if ( todebug == 1 ) printf("Firstpass == %d \n", firstpass);

     yydebug=0;
/******************************************************************************/
/*  1-  Open input file                                                       */
/******************************************************************************/

    strcpy(cur_filename, input_file);
    sprintf(input_fullpath, "%s/%s", input_dir, input_file);

    fortran_in = fopen(input_fullpath, "r");
    if (! fortran_in)
    {
        printf("Error : File %s does not exist\n", input_fullpath);
        exit(1);
    }

/******************************************************************************/
/*  2-  Variables initialization                                              */
/******************************************************************************/

    line_num_input = 1;
    PublicDeclare = 0;
    PrivateDeclare = 0;
    ExternalDeclare = 0;
    SaveDeclare = 0;
    pointerdeclare = 0;
    contiguousdeclare = 0;
    optionaldeclare = 0;
    incalldeclare = 0;
    inside_type_declare = 0;
    Allocatabledeclare = 0 ;
    Targetdeclare = 0 ;
    VariableIsParameter =  0 ;
    strcpy(NamePrecision,"");
    c_star = 0 ;
    functiondeclarationisdone = 0;
    insubroutinedeclare = 0 ;
    strcpy(subroutinename," ");
    isrecursive = 0;
    ispure = 0;
    isimpure = 0;
    iselemental = 0;
    InitialValueGiven = 0 ;
    GlobalDeclarationType = 0;
    inmoduledeclare = 0;
    incontainssubroutine = 0;
    afterpercent = 0;
    aftercontainsdeclare = 1;
    strcpy(nameinttypename,"");

/******************************************************************************/
/*  3-  Parsing of the input file (1 time)                                    */
/******************************************************************************/

    sprintf(output_file, "%s/%s", output_dir, input_file);

    if (firstpass == 0) fortran_out = fopen(output_file,"w");

    fortran_parse();

    if (firstpass == 0) NewModule_Creation_0();
    if (firstpass == 0) fclose(fortran_out);
}
#line 2 "fortran.yy.c"

#line 4 "fortran.yy.c"

#define  YY_INT_ALIGNED short int

/* A lexical scanner generated by flex */

#define yy_create_buffer fortran__create_buffer
#define yy_delete_buffer fortran__delete_buffer
#define yy_flex_debug fortran__flex_debug
#define yy_init_buffer fortran__init_buffer
#define yy_flush_buffer fortran__flush_buffer
#define yy_load_buffer_state fortran__load_buffer_state
#define yy_switch_to_buffer fortran__switch_to_buffer
#define yyin fortran_in
#define yyleng fortran_leng
#define yylex fortran_lex
#define yylineno fortran_lineno
#define yyout fortran_out
#define yyrestart fortran_restart
#define yytext fortran_text
#define yywrap fortran_wrap
#define yyalloc fortran_alloc
#define yyrealloc fortran_realloc
#define yyfree fortran_free

#define FLEX_SCANNER
#define YY_FLEX_MAJOR_VERSION 2
#define YY_FLEX_MINOR_VERSION 6
#define YY_FLEX_SUBMINOR_VERSION 1
#if YY_FLEX_SUBMINOR_VERSION > 0
#define FLEX_BETA
#endif

/* First, we deal with  platform-specific or compiler-specific issues. */

/* begin standard C headers. */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

/* end standard C headers. */

/* flex integer type definitions */

#ifndef FLEXINT_H
#define FLEXINT_H

/* C99 systems have <inttypes.h>. Non-C99 systems may or may not. */

#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L

/* C99 says to define __STDC_LIMIT_MACROS before including stdint.h,
 * if you want the limit (max/min) macros for int types. 
 */
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS 1
#endif

#include <inttypes.h>
typedef int8_t flex_int8_t;
typedef uint8_t flex_uint8_t;
typedef int16_t flex_int16_t;
typedef uint16_t flex_uint16_t;
typedef int32_t flex_int32_t;
typedef uint32_t flex_uint32_t;
#else
typedef signed char flex_int8_t;
typedef short int flex_int16_t;
typedef int flex_int32_t;
typedef unsigned char flex_uint8_t; 
typedef unsigned short int flex_uint16_t;
typedef unsigned int flex_uint32_t;

/* Limits of integral types. */
#ifndef INT8_MIN
#define INT8_MIN               (-128)
#endif
#ifndef INT16_MIN
#define INT16_MIN              (-32767-1)
#endif
#ifndef INT32_MIN
#define INT32_MIN              (-2147483647-1)
#endif
#ifndef INT8_MAX
#define INT8_MAX               (127)
#endif
#ifndef INT16_MAX
#define INT16_MAX              (32767)
#endif
#ifndef INT32_MAX
#define INT32_MAX              (2147483647)
#endif
#ifndef UINT8_MAX
#define UINT8_MAX              (255U)
#endif
#ifndef UINT16_MAX
#define UINT16_MAX             (65535U)
#endif
#ifndef UINT32_MAX
#define UINT32_MAX             (4294967295U)
#endif

#endif /* ! C99 */

#endif /* ! FLEXINT_H */

/* TODO: this is always defined, so inline it */
#define yyconst const

#if defined(__GNUC__) && __GNUC__ >= 3
#define yynoreturn __attribute__((__noreturn__))
#else
#define yynoreturn
#endif

/* Returned upon end-of-file. */
#define YY_NULL 0

/* Promotes a possibly negative, possibly signed char to an unsigned
 * integer for use as an array index.  If the signed char is negative,
 * we want to instead treat it as an 8-bit unsigned char, hence the
 * double cast.
 */
#define YY_SC_TO_UI(c) ((unsigned int) (unsigned char) c)

/* Enter a start condition.  This macro really ought to take a parameter,
 * but we do it the disgusting crufty way forced on us by the ()-less
 * definition of BEGIN.
 */
#define BEGIN (yy_start) = 1 + 2 *

/* Translate the current start state into a value that can be later handed
 * to BEGIN to return to the state.  The YYSTATE alias is for lex
 * compatibility.
 */
#define YY_START (((yy_start) - 1) / 2)
#define YYSTATE YY_START

/* Action number for EOF rule of a given start state. */
#define YY_STATE_EOF(state) (YY_END_OF_BUFFER + state + 1)

/* Special action meaning "start processing a new file". */
#define YY_NEW_FILE fortran_restart(fortran_in  )

#define YY_END_OF_BUFFER_CHAR 0

/* Size of default input buffer. */
#ifndef YY_BUF_SIZE
#ifdef __ia64__
/* On IA-64, the buffer size is 16k, not 8k.
 * Moreover, YY_BUF_SIZE is 2*YY_READ_BUF_SIZE in the general case.
 * Ditto for the __ia64__ case accordingly.
 */
#define YY_BUF_SIZE 32768
#else
#define YY_BUF_SIZE 16384
#endif /* __ia64__ */
#endif

/* The state buf must be large enough to hold one state per character in the main buffer.
 */
#define YY_STATE_BUF_SIZE   ((YY_BUF_SIZE + 2) * sizeof(yy_state_type))

#ifndef YY_TYPEDEF_YY_BUFFER_STATE
#define YY_TYPEDEF_YY_BUFFER_STATE
typedef struct yy_buffer_state *YY_BUFFER_STATE;
#endif

#ifndef YY_TYPEDEF_YY_SIZE_T
#define YY_TYPEDEF_YY_SIZE_T
typedef size_t yy_size_t;
#endif

extern int fortran_leng;

extern FILE *fortran_in, *fortran_out;

#define EOB_ACT_CONTINUE_SCAN 0
#define EOB_ACT_END_OF_FILE 1
#define EOB_ACT_LAST_MATCH 2

    #define YY_LESS_LINENO(n)
    #define YY_LINENO_REWIND_TO(ptr)
    
/* Return all but the first "n" matched characters back to the input stream. */
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up fortran_text. */ \
        yy_size_t yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		*yy_cp = (yy_hold_char); \
		YY_RESTORE_YY_MORE_OFFSET \
		(yy_c_buf_p) = yy_cp = yy_bp + yyless_macro_arg - YY_MORE_ADJ; \
		YY_DO_BEFORE_ACTION; /* set up fortran_text again */ \
		} \
	while ( 0 )

#define unput(c) yyunput( c, (yytext_ptr)  )

#ifndef YY_STRUCT_YY_BUFFER_STATE
#define YY_STRUCT_YY_BUFFER_STATE
struct yy_buffer_state
	{
	FILE *yy_input_file;

	char *yy_ch_buf;		/* input buffer */
	char *yy_buf_pos;		/* current position in input buffer */

	/* Size of input buffer in bytes, not including room for EOB
	 * characters.
	 */
	int yy_buf_size;

	/* Number of characters read into yy_ch_buf, not including EOB
	 * characters.
	 */
	int yy_n_chars;

	/* Whether we "own" the buffer - i.e., we know we created it,
	 * and can realloc() it to grow it, and should free() it to
	 * delete it.
	 */
	int yy_is_our_buffer;

	/* Whether this is an "interactive" input source; if so, and
	 * if we're using stdio for input, then we want to use getc()
	 * instead of fread(), to make sure we stop fetching input after
	 * each newline.
	 */
	int yy_is_interactive;

	/* Whether we're considered to be at the beginning of a line.
	 * If so, '^' rules will be active on the next match, otherwise
	 * not.
	 */
	int yy_at_bol;

    int yy_bs_lineno; /**< The line count. */
    int yy_bs_column; /**< The column count. */
    
	/* Whether to try to fill the input buffer when we reach the
	 * end of it.
	 */
	int yy_fill_buffer;

	int yy_buffer_status;

#define YY_BUFFER_NEW 0
#define YY_BUFFER_NORMAL 1
	/* When an EOF's been seen but there's still some text to process
	 * then we mark the buffer as YY_EOF_PENDING, to indicate that we
	 * shouldn't try reading from the input source any more.  We might
	 * still have a bunch of tokens to match, though, because of
	 * possible backing-up.
	 *
	 * When we actually see the EOF, we change the status to "new"
	 * (via fortran_restart()), so that the user can continue scanning by
	 * just pointing fortran_in at a new input file.
	 */
#define YY_BUFFER_EOF_PENDING 2

	};
#endif /* !YY_STRUCT_YY_BUFFER_STATE */

/* Stack of input buffers. */
static size_t yy_buffer_stack_top = 0; /**< index of top of stack. */
static size_t yy_buffer_stack_max = 0; /**< capacity of stack. */
static YY_BUFFER_STATE * yy_buffer_stack = NULL; /**< Stack as an array. */

/* We provide macros for accessing buffer states in case in the
 * future we want to put the buffer states in a more general
 * "scanner state".
 *
 * Returns the top of the stack, or NULL.
 */
#define YY_CURRENT_BUFFER ( (yy_buffer_stack) \
                          ? (yy_buffer_stack)[(yy_buffer_stack_top)] \
                          : NULL)

/* Same as previous macro, but useful when we know that the buffer stack is not
 * NULL or when we need an lvalue. For internal use only.
 */
#define YY_CURRENT_BUFFER_LVALUE (yy_buffer_stack)[(yy_buffer_stack_top)]

/* yy_hold_char holds the character lost when fortran_text is formed. */
static char yy_hold_char;
static int yy_n_chars;		/* number of characters read into yy_ch_buf */
int fortran_leng;

/* Points to current character in buffer. */
static char *yy_c_buf_p = NULL;
static int yy_init = 0;		/* whether we need to initialize */
static int yy_start = 0;	/* start state number */

/* Flag which is used to allow fortran_wrap()'s to do buffer switches
 * instead of setting up a fresh fortran_in.  A bit of a hack ...
 */
static int yy_did_buffer_switch_on_eof;

void fortran_restart (FILE *input_file  );
void fortran__switch_to_buffer (YY_BUFFER_STATE new_buffer  );
YY_BUFFER_STATE fortran__create_buffer (FILE *file,int size  );
void fortran__delete_buffer (YY_BUFFER_STATE b  );
void fortran__flush_buffer (YY_BUFFER_STATE b  );
void fortran_push_buffer_state (YY_BUFFER_STATE new_buffer  );
void fortran_pop_buffer_state (void );

static void fortran_ensure_buffer_stack (void );
static void fortran__load_buffer_state (void );
static void fortran__init_buffer (YY_BUFFER_STATE b,FILE *file  );

#define YY_FLUSH_BUFFER fortran__flush_buffer(YY_CURRENT_BUFFER )

YY_BUFFER_STATE fortran__scan_buffer (char *base,yy_size_t size  );
YY_BUFFER_STATE fortran__scan_string (yyconst char *yy_str  );
YY_BUFFER_STATE fortran__scan_bytes (yyconst char *bytes,int len  );

void *fortran_alloc (yy_size_t  );
void *fortran_realloc (void *,yy_size_t  );
void fortran_free (void *  );

#define yy_new_buffer fortran__create_buffer

#define yy_set_interactive(is_interactive) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){ \
        fortran_ensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            fortran__create_buffer(fortran_in,YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_is_interactive = is_interactive; \
	}

#define yy_set_bol(at_bol) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){\
        fortran_ensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            fortran__create_buffer(fortran_in,YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = at_bol; \
	}

#define YY_AT_BOL() (YY_CURRENT_BUFFER_LVALUE->yy_at_bol)

/* Begin user sect3 */

#define fortran_wrap() (/*CONSTCOND*/1)
#define YY_SKIP_YYWRAP

typedef unsigned char YY_CHAR;

FILE *fortran_in = NULL, *fortran_out = NULL;

typedef int yy_state_type;

extern int fortran_lineno;

int fortran_lineno = 1;

extern char *fortran_text;
#ifdef yytext_ptr
#undef yytext_ptr
#endif
#define yytext_ptr fortran_text

static yy_state_type yy_get_previous_state (void );
static yy_state_type yy_try_NUL_trans (yy_state_type current_state  );
static int yy_get_next_buffer (void );
static void yynoreturn yy_fatal_error (yyconst char* msg  );

/* Done after the current pattern has been matched and before the
 * corresponding action - sets up fortran_text.
 */
#define YY_DO_BEFORE_ACTION \
	(yytext_ptr) = yy_bp; \
	fortran_leng = (int) (yy_cp - yy_bp); \
	(yy_hold_char) = *yy_cp; \
	*yy_cp = '\0'; \
	(yy_c_buf_p) = yy_cp;

#define YY_NUM_RULES 183
#define YY_END_OF_BUFFER 184
/* This struct is not used in this scanner,
   but its presence is necessary. */
struct yy_trans_info
	{
	flex_int32_t yy_verify;
	flex_int32_t yy_nxt;
	};
static yyconst flex_int16_t yy_acclist[1649] =
    {   0,
      148,  148,  184,  183,  172,  183,  171,  183,  182,  183,
      183,  161,  183,  165,  183,  175,  183,  183,  164,  183,
      164,  183,  164,  183,  167,  183,  162,  183,  145,  183,
      160,  183,  164,  183,  166,  183,  169,  183,  168,  183,
      170,  183,  156,  183,  156,  183,  156,  183,  156,  183,
      156,  183,  156,  183,  156,  183,  156,  183,  156,  183,
      156,  183,  156,  183,  156,  183,  156,  183,  156,  183,
      156,  183,  156,  183,  156,  183,  156,  183,  156,  183,
      156,  183,  156,  183,  172,  183,  171,  181,  183,  182,
      183,  156,  183,  156,  183,  156,  183,  156,  183,  156,

      183,  183,  183,  179,  183,  183,  183,  154,  183,  183,
      183,  148,  183,  149,  183,  183,  171,  183,  156,  183,
      156,  183,  156,  183,  156,  183,  156,  183,  156,  183,
      156,  183,  156,  183,  156,  183,  156,  183,  156,  183,
      156,  183,  156,  183,  156,  183,  156,  183,  156,  183,
      156,  183,  156,  183,  156,  183,  156,  183,  156,  183,
      171,  181,  183,  172,  183,  164,  183,  160,  183,  156,
      183,  156,  183,  156,  183,  156,  183,  156,  183,  172,
      183,  160,  183,  172,  182,  182,  182,  151,  175,  150,
      143,   20,  159,  144,  142,   34,  160,  141,   35,   33,

       18,   36,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,   42,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,   95,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  172,  181,  182,  182,  182,  182,  156,
      156,  156,  156,   95,  156,  156,  179,  154,  148,  147,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,   42,  156,  156,  156,  156,  156,

      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
       95,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  181,  172,  172,  180,   20,  160,  180,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,   95,  156,
      156,  172,  160,  182,  182,  146,  150,  158,  157,  158,
      159,  159,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,    9,  156,  156,  156,  156,  156,  156,  156,

      156,  156,  156,  156,  156,  107,16489,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,   98,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,   11,  156,  156,  156,  156,  182,  182,  182,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,    9,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,

      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
       98,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,   11,  156,  156,  156,  156,  172,
      172,  160,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  182,  182,  159,   22,   24,
       23,   26,   25,   28,   30,  156,  156,  156,  156,  156,
      156,  156,   15,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,   41,   41,  156,  156,  156,  103,

      156,  121,  156,  156,  156,  156,  156,  122,  156,  131,
      156,  156,   83,  156,  156,  156,  156,  119,  156,  156,
       97,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  123,  156,  156,  156,  156,  156,  120,
       14,  156,  156,   64,  156,   81,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,   67,  156,   87,  156,
       43,  156,  135,  156,  156,  156,  156,  156,   76,  156,
      156,  156,   80,  156,   58,  156,  156,  156,  101,  156,
      156,  156,  156,  156,   47,  182,  182,  182,  109,  156,
      156,  156,  156,  156,  156,16462,  156,  156,  156,  156,

      156,  156,  156,   15,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,   41,  156,  156,  156,  103,
      156,  156,  156,  156,  156,  156,  156,  156,  156,   83,
      156,  156,  156,  156,  156,  156,   97,  156,  156,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,   14,  156,  156,   64,  156,   81,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
       67,  156,   87,  156,   43,  156,  156,  156,  156,  156,
      156,   76,  156,  156,  156,   80,  156,   58,  156,  156,
      156,  101,  156,  156,  156,  156,  156,  172,  160,   15,

      156,  109,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,  156,16462,  182,  182,  163,
       32,   21,   29,   31,  156,  156,  156,  156,  156,  156,
      156,  156,   53,  156,  156,  156,  156,  156,  139,  156,
      156,  156,  156,  156,  156,  156,  156,   40,  156,  104,
      156,  156,  156,  156,  156,  156,  156,  156,  112,   91,
      156,  132,  156,   97,  106,  156,  156,  156,   99,  156,
      156,  156,  156,  156,  156,  156,  156,  124,  156,  156,
      126,  133,  156,  156,  156,  156,  156,  156,   56,  156,
      156,  156,   84,  156,  156,  156,  156,   86,  134,  156,

      156,  156,  156,  156,  156,  156,  156,  156,  116,   59,
      156,   38,  156,   90,  156,  109,16462,  182,  182,  182,
      109,  156,   96,  156,  156, 8270,   77, 8270,  156,  156,
      156,  156,  156,  156,  156,  156,   53,  156,  156,  156,
      156,  156,  139,  156,  156,  156,  156,  156,  156,  156,
      156,   40,  156,  104,  156,  156,  156,  156,  156,  156,
      156,  156,   91,  156,  156,  156,  156,  156,   99,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  156,   56,  156,  156,  156,   84,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,

      156,  156,  156,   59,  156,   38,  156,   90,  156,  172,
      160,  109,  156,  156,   53,  156,  156,  156,  156,  156,
      156,  156,  139,  156,  156,  156,   16,  182,   16,  182,
       16,   16,  151,   16,   16,   16,  150,   16,   16,   16,
       16,   16,   16,   27,  156,  156,  156,  156,  156,   16,
      156,  156,  156,   70,  156,  156,  156,  156,  156,  156,
      156,  156,  156,  156,  102,  156,  156,   40,  104,  156,
      156,  156,  156,  156,  138,  156,  156,  106, 8297,  106,
      156,   68,  156,  156,  156,  156,   73,  156,  156,  156,
      129,  156,  156,   37,  156,  156,  156,  156,  156,  156,

      156,  156,  156,  156,  156,   93,  156,  156,    7,  156,
       82,  156,   12,  156,  156,  156,  137,  156,  156,   92,
      156,   89,  182,  182,   16,  182,  156,  156,  156,  156,
      156,  156,  156,  156,   16,  156,  156,  156,   70,  156,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  102,
      156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
       68,  156,  156,  156,  156,   73,  156,  156,  156,  156,
      156,   37,  156,  156,  156,  156,  156,  156,  156,  156,
      156,  156,  156,   93,  156,  156,    7,  156,   82,  156,
       12,  156,  156,  156,  137,  156,  156,   92,  156,   16,

      156,  156,   70,  156,  156,  156,  156,  156,  156,   16,
      156,  156,  156,   17,   17,  182,   17,   17,  151,   17,
       17,   17,  150,   17,   17,   17,   17,   17,   17,  113,
      114,   17,  156,  156,  156,  156,  156,   50,  156,  156,
      156,  156,  156,  110,  156,  156,  156,  156,  156,  102,
      156,  156,   79,  156,  156,  156,  125,  156,  156, 8297,
      156,   10,  156,   54,  156,   44,  156,  156,  156,  130,
       45,  156,  156,  156,  156,    5,  156,  118,  156,  156,
       74,  156,  156,   94,  156,    2,  156,  156,  156,  127,
      136,  156,  182,   17,  182,  156,   71,  156,  176,   17,

      156,  156,  156,  156,  156,   50,  156,  156,  156,  156,
      156,  110,  156,  156,  156,  156,  156,  156,  156,   79,
      156,  156,  156,  156,  156,  156,   10,  156,   54,  156,
       44,  156,  156,  156,   45,  156,  156,  156,  156,    5,
      156,  156,  156,   74,  156,  156,   94,  156,    2,  156,
      156,  156,  156,  176,   17,   17,  156,  156,   50,  156,
      156,  156,  156,  156,  156,  156,    3,  156,  156,  156,
      156,  156,  156,    4,  156,  156,  156,  156,  156,  156,
      156,   79,  156,   60,  156,  156,   72,  156,    8,  156,
       13,  156,  156,  156,  156,   88,  156,  117,   75,  156,

      156,  156,  156,  156,  156,  182,   63,  156,  156,  156,
        3,  156,  156,  156,  156,  156,  156,    4,  156,  156,
      156,  156,  156,  156,  156,  156,   60,  156,  156,   72,
      156,    8,  156,   13,  156,  156,  156,  156,   88,  156,
       75,  156,  156,  156,  156,  156,  156,  156,  156,   63,
      156,  156,    4,  156,  156,  142,  156,  156,  140,  156,
       46,  156,  156,  156,  156,   55,  156,  156,  156,   69,
      156,   62,  156,   60,  111,  156,  156,  100,  156,  115,
      156,   65,  156,  128,   66,  156,  156,  156,   63,  182,
      152,  156,  155,  156,  156,  140,  156,   46,  156,  156,

      156,  156,   55,  156,  156,  156,   69,  156,   62,  156,
      111,  156,  156,  100,  156,  156,   65,  156,   66,  156,
      156,  156,   46,  156,  156,  156,  152,  156,  174,  142,
      156,  156,   39,  156,   52,  156,    6,  156,  156,  156,
       62,   61,  111,  156,  156,  108,  156,    1,  156,  152,
      182,  156,  156,   39,  156,   52,  156,    6,  156,  156,
      156,  156,  156,  108,  156,    1,  156,  173,   39,  156,
       52,  156,   51,  156,  156,  156,   57,  156,  156,  108,
      182,   51,  156,  156,  156,   57,  156,  156,  174,  156,
      156,  156,  182,  156,  156,  156,  173,   19,   49,  156,

      156,  156,  182,  153,  154,   49,  156,  156,  156,  173,
      173,   49,  156,  156,  182,  156,  156,   48,  156,   85,
      156,  182,   48,  156,   85,  156,  173,   48,   85,  182,
      182,  182,  182,  182,  182,  177,  182,  177,  177,  180,
      177,  181,  182,  180,  178,  179,  178,  179
    } ;

static yyconst flex_int16_t yy_accept[1937] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    3,    3,    3,    3,    3,    4,    5,    7,
        9,   11,   12,   14,   16,   18,   19,   21,   23,   25,
       27,   29,   31,   33,   35,   37,   39,   41,   43,   45,
       47,   49,   51,   53,   55,   57,   59,   61,   63,   65,
       67,   69,   71,   73,   75,   77,   79,   81,   83,   85,
       87,   90,   92,   94,   96,   98,  100,  102,  103,  104,
      106,  107,  108,  110,  111,  112,  114,  116,  117,  119,
      121,  123,  125,  127,  129,  131,  133,  135,  137,  139,
      141,  143,  145,  147,  149,  151,  153,  155,  157,  159,

      161,  164,  166,  168,  170,  172,  174,  176,  178,  180,
      182,  184,  184,  184,  185,  186,  187,  188,  188,  189,
      189,  189,  190,  190,  190,  190,  190,  191,  191,  191,
      191,  191,  192,  192,  192,  192,  193,  193,  194,  194,
      194,  194,  194,  194,  194,  194,  194,  194,  194,  195,
      196,  197,  197,  198,  198,  199,  200,  201,  202,  203,
      204,  205,  206,  207,  208,  209,  210,  211,  212,  213,
      214,  215,  216,  217,  219,  220,  221,  222,  223,  224,
      225,  226,  227,  228,  229,  230,  231,  232,  233,  235,
      236,  237,  238,  239,  240,  241,  242,  243,  244,  245,

      246,  247,  248,  249,  250,  251,  252,  253,  254,  255,
      256,  257,  258,  259,  260,  261,  262,  263,  264,  264,
      265,  266,  266,  266,  266,  266,  266,  266,  266,  267,
      267,  268,  269,  270,  270,  271,  272,  273,  274,  276,
      277,  277,  278,  278,  278,  279,  279,  279,  279,  280,
      280,  281,  281,  281,  281,  281,  281,  281,  282,  283,
      284,  285,  286,  287,  288,  289,  290,  291,  292,  293,
      294,  295,  297,  298,  299,  300,  301,  302,  303,  304,
      305,  306,  307,  308,  309,  310,  311,  313,  314,  315,
      316,  317,  318,  319,  320,  321,  322,  323,  324,  325,

      326,  327,  328,  329,  330,  331,  332,  333,  334,  335,
      336,  337,  338,  339,  340,  341,  342,  342,  343,  343,
      343,  344,  345,  345,  345,  346,  347,  347,  347,  347,
      347,  348,  349,  349,  350,  351,  352,  353,  354,  355,
      356,  357,  358,  359,  361,  362,  363,  363,  363,  364,
      364,  364,  364,  365,  366,  366,  366,  366,  366,  366,
      366,  366,  366,  368,  368,  368,  368,  368,  368,  368,
      368,  368,  368,  368,  368,  368,  368,  368,  368,  368,
      368,  368,  368,  368,  368,  368,  368,  368,  368,  368,
      368,  368,  368,  368,  368,  369,  372,  372,  373,  374,

      375,  376,  377,  378,  379,  380,  381,  382,  383,  384,
      385,  386,  387,  388,  389,  389,  390,  391,  392,  393,
      395,  396,  397,  398,  399,  400,  401,  402,  403,  404,
      405,  405,  406,  406,  408,  409,  410,  411,  412,  413,
      414,  415,  416,  417,  418,  419,  420,  421,  422,  423,
      424,  425,  426,  427,  428,  429,  431,  432,  433,  434,
      435,  436,  437,  438,  439,  440,  441,  442,  443,  444,
      445,  446,  447,  448,  449,  450,  451,  452,  453,  455,
      456,  457,  458,  458,  458,  458,  458,  458,  458,  458,
      458,  458,  459,  460,  461,  461,  462,  463,  464,  465,

      466,  467,  467,  467,  467,  467,  467,  467,  467,  467,
      467,  467,  467,  467,  468,  469,  470,  471,  472,  473,
      474,  475,  476,  477,  478,  479,  480,  481,  482,  483,
      484,  485,  486,  487,  489,  490,  491,  492,  493,  494,
      495,  496,  497,  498,  499,  500,  501,  502,  503,  504,
      505,  506,  507,  508,  509,  510,  511,  512,  513,  514,
      515,  516,  517,  518,  519,  520,  521,  523,  524,  525,
      526,  527,  528,  529,  530,  531,  532,  533,  534,  535,
      536,  537,  538,  539,  540,  541,  542,  543,  544,  545,
      547,  548,  549,  550,  550,  550,  550,  550,  551,  551,

      552,  552,  552,  552,  552,  552,  552,  553,  553,  554,
      555,  556,  557,  558,  559,  560,  561,  562,  563,  564,
      565,  566,  566,  566,  566,  566,  567,  568,  568,  568,
      568,  568,  568,  568,  568,  568,  568,  568,  568,  568,
      568,  568,  568,  568,  568,  568,  569,  569,  569,  570,
      570,  571,  572,  573,  574,  574,  575,  575,  575,  576,
      576,  576,  576,  576,  576,  576,  576,  576,  576,  576,
      576,  577,  578,  579,  580,  581,  582,  583,  585,  586,
      587,  588,  589,  590,  591,  592,  593,  594,  595,  596,
      598,  599,  600,  602,  602,  603,  604,  605,  606,  607,

      608,  608,  609,  610,  610,  611,  612,  613,  615,  616,
      617,  618,  618,  619,  620,  621,  621,  623,  623,  623,
      623,  623,  624,  625,  626,  627,  628,  629,  630,  631,
      632,  633,  634,  634,  635,  636,  637,  638,  639,  640,
      640,  641,  643,  644,  646,  648,  649,  650,  651,  652,
      653,  654,  655,  656,  657,  659,  661,  663,  663,  664,
      665,  666,  667,  668,  669,  671,  672,  673,  675,  677,
      678,  679,  681,  682,  683,  684,  685,  686,  686,  686,
      686,  686,  686,  686,  687,  688,  689,  689,  691,  692,
      693,  694,  695,  697,  697,  697,  697,  697,  697,  697,

      697,  697,  697,  698,  699,  700,  701,  702,  703,  704,
      706,  707,  708,  709,  710,  711,  712,  713,  714,  715,
      716,  718,  719,  720,  722,  723,  724,  725,  726,  727,
      728,  729,  730,  732,  733,  734,  735,  736,  737,  739,
      740,  741,  742,  743,  744,  745,  746,  747,  748,  749,
      750,  751,  752,  753,  754,  755,  757,  758,  760,  762,
      763,  764,  765,  766,  767,  768,  769,  770,  771,  773,
      775,  777,  778,  779,  780,  781,  782,  784,  785,  786,
      788,  790,  791,  792,  794,  795,  796,  797,  798,  798,
      798,  798,  799,  799,  799,  799,  799,  799,  800,  800,

      802,  804,  805,  806,  807,  808,  809,  810,  811,  812,
      813,  814,  815,  816,  818,  818,  818,  818,  819,  820,
      820,  820,  820,  820,  820,  821,  821,  821,  821,  821,
      821,  821,  822,  823,  823,  824,  825,  825,  825,  825,
      825,  825,  825,  826,  827,  828,  829,  830,  831,  832,
      833,  835,  836,  837,  838,  839,  841,  842,  843,  844,
      845,  846,  846,  847,  848,  848,  848,  848,  848,  848,
      850,  852,  853,  854,  855,  856,  857,  858,  859,  859,
      860,  862,  862,  863,  864,  865,  865,  865,  865,  866,
      867,  868,  869,  871,  872,  873,  874,  875,  876,  877,

      878,  878,  879,  880,  881,  881,  882,  882,  883,  884,
      885,  886,  887,  888,  889,  891,  892,  893,  895,  896,
      897,  898,  898,  899,  899,  900,  901,  902,  903,  904,
      905,  906,  907,  908,  909,  909,  910,  912,  914,  916,
      917,  917,  917,  917,  917,  918,  919,  920,  921,  921,
      922,  923,  924,  925,  926,  927,  928,  929,  929,  929,
      929,  929,  929,  929,  930,  931,  932,  933,  934,  935,
      936,  937,  939,  940,  941,  942,  943,  945,  946,  947,
      948,  949,  950,  951,  952,  954,  956,  957,  958,  959,
      960,  961,  962,  963,  965,  966,  967,  968,  969,  971,

      972,  973,  974,  975,  976,  977,  978,  979,  980,  981,
      982,  983,  984,  985,  986,  988,  989,  990,  992,  993,
      994,  995,  996,  997,  998,  999, 1000, 1001, 1002, 1003,
     1004, 1006, 1008, 1010, 1010, 1010, 1010, 1011, 1011, 1011,
     1011, 1011, 1011, 1012, 1012, 1013, 1014, 1015, 1017, 1018,
     1019, 1020, 1021, 1022, 1023, 1025, 1026, 1027, 1027, 1027,
     1028, 1029, 1031, 1031, 1032, 1034, 1034, 1035, 1036, 1038,
     1038, 1038, 1038, 1038, 1039, 1040, 1041, 1042, 1043, 1044,
     1045, 1045, 1045, 1045, 1045, 1046, 1047, 1048, 1049, 1050,
     1052, 1053, 1054, 1056, 1057, 1058, 1059, 1060, 1061, 1062,

     1063, 1064, 1065, 1065, 1065, 1067, 1068, 1069, 1070, 1070,
     1070, 1070, 1071, 1072, 1073, 1074, 1075, 1075, 1076, 1077,
     1078, 1078, 1079, 1079, 1079, 1079, 1079, 1080, 1081, 1082,
     1084, 1085, 1086, 1087, 1089, 1090, 1091, 1091, 1092, 1093,
     1094, 1096, 1097, 1098, 1099, 1100, 1101, 1102, 1103, 1104,
     1105, 1106, 1108, 1109, 1111, 1113, 1115, 1116, 1117, 1119,
     1120, 1122, 1122, 1123, 1123, 1123, 1123, 1124, 1125, 1127,
     1127, 1128, 1129, 1130, 1130, 1130, 1130, 1130, 1130, 1130,
     1131, 1132, 1133, 1134, 1135, 1137, 1138, 1139, 1141, 1142,
     1143, 1144, 1145, 1146, 1147, 1148, 1149, 1150, 1152, 1153,

     1154, 1155, 1156, 1157, 1158, 1159, 1160, 1161, 1163, 1164,
     1165, 1166, 1168, 1169, 1170, 1171, 1172, 1174, 1175, 1176,
     1177, 1178, 1179, 1180, 1181, 1182, 1183, 1184, 1186, 1187,
     1189, 1191, 1193, 1194, 1195, 1197, 1198, 1200, 1200, 1200,
     1200, 1200, 1201, 1201, 1202, 1203, 1205, 1206, 1207, 1208,
     1209, 1210, 1212, 1213, 1214, 1214, 1215, 1217, 1218, 1220,
     1221, 1222, 1224, 1224, 1225, 1226, 1227, 1228, 1229, 1230,
     1230, 1230, 1230, 1230, 1230, 1231, 1231, 1232, 1234, 1235,
     1236, 1237, 1238, 1240, 1241, 1242, 1243, 1244, 1246, 1247,
     1247, 1248, 1249, 1250, 1251, 1251, 1252, 1252, 1252, 1252,

     1253, 1255, 1256, 1257, 1257, 1258, 1259, 1260, 1261, 1261,
     1261, 1262, 1264, 1266, 1268, 1269, 1270, 1270, 1271, 1273,
     1273, 1274, 1275, 1276, 1278, 1278, 1279, 1280, 1281, 1283,
     1284, 1286, 1288, 1289, 1289, 1290, 1290, 1291, 1291, 1292,
     1293, 1293, 1293, 1293, 1294, 1296, 1296, 1297, 1298, 1299,
     1299, 1299, 1299, 1300, 1300, 1300, 1302, 1303, 1304, 1305,
     1306, 1308, 1309, 1310, 1311, 1312, 1314, 1315, 1316, 1317,
     1318, 1319, 1320, 1322, 1323, 1324, 1325, 1326, 1327, 1329,
     1331, 1333, 1334, 1335, 1337, 1338, 1339, 1340, 1342, 1343,
     1344, 1346, 1347, 1349, 1351, 1352, 1353, 1354, 1354, 1355,

     1355, 1356, 1356, 1358, 1359, 1361, 1362, 1363, 1364, 1365,
     1366, 1366, 1366, 1366, 1366, 1366, 1367, 1369, 1370, 1371,
     1372, 1373, 1374, 1376, 1377, 1378, 1378, 1378, 1379, 1380,
     1381, 1381, 1382, 1382, 1383, 1383, 1384, 1386, 1387, 1389,
     1391, 1391, 1393, 1394, 1395, 1395, 1396, 1398, 1398, 1399,
     1401, 1402, 1403, 1404, 1404, 1405, 1406, 1406, 1406, 1407,
     1407, 1409, 1410, 1410, 1410, 1410, 1410, 1410, 1411, 1413,
     1414, 1415, 1416, 1417, 1418, 1420, 1421, 1422, 1423, 1424,
     1425, 1426, 1427, 1429, 1430, 1432, 1434, 1436, 1437, 1438,
     1439, 1441, 1443, 1444, 1445, 1446, 1447, 1448, 1448, 1448,

     1448, 1449, 1450, 1452, 1453, 1455, 1456, 1456, 1456, 1456,
     1456, 1456, 1456, 1456, 1457, 1458, 1459, 1461, 1463, 1464,
     1465, 1466, 1468, 1468, 1468, 1469, 1470, 1472, 1472, 1474,
     1474, 1475, 1477, 1478, 1480, 1480, 1481, 1481, 1482, 1484,
     1484, 1485, 1487, 1487, 1488, 1489, 1490, 1490, 1491, 1491,
     1493, 1493, 1493, 1493, 1494, 1495, 1496, 1498, 1500, 1501,
     1502, 1503, 1505, 1506, 1507, 1509, 1511, 1513, 1514, 1516,
     1517, 1519, 1521, 1522, 1523, 1523, 1523, 1523, 1525, 1526,
     1527, 1529, 1529, 1529, 1530, 1530, 1530, 1530, 1531, 1532,
     1533, 1535, 1537, 1539, 1539, 1539, 1540, 1541, 1542, 1542,

     1543, 1544, 1545, 1545, 1546, 1546, 1548, 1550, 1551, 1552,
     1552, 1552, 1552, 1553, 1554, 1556, 1558, 1560, 1561, 1562,
     1563, 1564, 1566, 1568, 1568, 1568, 1568, 1569, 1569, 1571,
     1573, 1573, 1573, 1573, 1573, 1573, 1573, 1575, 1575, 1575,
     1575, 1575, 1576, 1577, 1579, 1579, 1580, 1581, 1582, 1582,
     1582, 1582, 1584, 1585, 1586, 1588, 1589, 1589, 1589, 1589,
     1589, 1589, 1589, 1589, 1589, 1589, 1589, 1589, 1590, 1590,
     1590, 1590, 1590, 1591, 1592, 1592, 1593, 1594, 1594, 1594,
     1594, 1595, 1596, 1597, 1597, 1597, 1597, 1597, 1597, 1597,
     1597, 1597, 1597, 1597, 1597, 1598, 1598, 1598, 1598, 1598,

     1598, 1599, 1599, 1599, 1601, 1602, 1602, 1603, 1604, 1604,
     1604, 1604, 1606, 1608, 1609, 1610, 1610, 1610, 1610, 1610,
     1610, 1610, 1611, 1611, 1611, 1611, 1612, 1612, 1612, 1612,
     1612, 1613, 1613, 1614, 1614, 1615, 1616, 1616, 1616, 1617,
     1618, 1618, 1618, 1618, 1618, 1618, 1618, 1618, 1618, 1618,
     1618, 1618, 1620, 1620, 1622, 1623, 1623, 1623, 1625, 1627,
     1627, 1627, 1627, 1627, 1627, 1627, 1628, 1628, 1629, 1630,
     1631, 1631, 1631, 1631, 1631, 1631, 1631, 1631, 1631, 1632,
     1632, 1632, 1632, 1632, 1632, 1632, 1632, 1632, 1633, 1633,
     1633, 1633, 1633, 1634, 1634, 1634, 1634, 1635, 1635, 1635,

     1635, 1636, 1637, 1638, 1638, 1639, 1639, 1639, 1639, 1641,
     1641, 1641, 1643, 1643, 1644, 1644, 1644, 1644, 1644, 1644,
     1644, 1645, 1645, 1645, 1645, 1645, 1647, 1647, 1647, 1648,
     1648, 1648, 1649, 1649, 1649, 1649
    } ;

static yyconst YY_CHAR yy_ec[256] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    4,    5,    6,    7,    8,    9,   10,   11,   12,
       13,   14,   15,   16,   17,   18,   19,   20,   20,   20,
       20,   20,   20,   20,   20,   20,   20,   21,   22,   23,
       24,   25,    1,    1,   26,   27,   28,   29,   30,   31,
       32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
       42,   43,   44,   45,   46,   47,   48,   49,   50,   51,
       52,    1,   53,    1,   54,    1,   55,   56,   57,   58,

       59,   60,   61,   62,   63,   35,   64,   65,   66,   67,
       68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
       78,   79,    1,   80,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1
    } ;

static yyconst YY_CHAR yy_meta[81] =
    {   0,
        1,    2,    3,    2,    4,    5,    4,    4,    1,    4,
        6,    7,    8,    4,    9,   10,   11,   12,   13,   14,
        1,    4,    1,    1,    1,   15,   14,   14,   14,   14,
       15,   16,   17,   17,   17,   17,   16,   17,   16,   16,
       17,   17,   16,   16,   16,   16,   17,   17,   17,   17,
       17,    1,    1,   18,   15,   14,   14,   14,   14,   15,
       16,   17,   17,   17,   16,   17,   16,   16,   17,   17,
       16,   16,   16,   16,   17,   17,   17,   17,   17,    5
    } ;

static yyconst flex_uint16_t yy_base[2104] =
    {   0,
        0,   79,    0,    0,    0,  151, 3825,   82, 3822,   86,
       89,   92,  224,  303,    0,  375, 3821,   70,  102, 9682,
       78,  113,   86,   90,  308,  311,  355,  129,  147,  137,
      447,  386,  440,  145,  146,  285,  302,  361,  444,  356,
      499,  497,  547,  594,  382,  352,  535,  495,  503,  582,
      643,  639,  679,  606,  700,  725,  604,  732,  445,  804,
      123,  538,  674,  784,  780,  835,  837, 9682, 3799, 9682,
       94, 3766, 9682,  491,  102,  110, 9682, 3750,  886,  876,
      796,  947,  895,  996,  945,  154,  585,  878,  384,  570,
      698, 1016,  921, 1046,  959, 1076, 1074,  126, 1095, 1020,

     1149,  316, 1127, 1205, 1278,   94, 1130,   90, 1164,  347,
      437,  161,  128,  130,    0,  289,  281, 3739, 3724,  448,
      322,  459, 3697,  434,  733,  542, 3532,  738, 1038,  767,
      774, 9682, 1357, 1363, 1380, 9682, 1172, 1103,  302,  321,
      642,  645,  760,  355,  362,  967, 1180, 1241, 9682, 9682,
     9682, 1198, 1372,  323, 9682, 9682, 9682, 9682, 9682,    0,
      785,  299,  446,  447,  480,  366,  544,  568,  551,  654,
      569,  741,  555,  955,  875,  665,  620,  742,  752,  849,
      789,  785,  805,  868,  889, 1092, 1401,  896, 1379, 1350,
      957,  967,  701, 1039,  978,  984, 1046, 1060, 1081, 1106,

     1151, 1130, 1187, 1204, 1152, 1377, 1409, 1141, 1176, 1317,
     1400,  727, 1328,  825,  898,  944, 1411,  942,    0, 1413,
     1250, 3527, 1420,  988, 1373, 1400, 1419, 1267, 3514, 1474,
     1014, 1406, 1422, 1048, 1443, 1445, 1418, 1432, 1461, 1451,
     3454, 9682, 1491, 3450, 9682, 1519, 1493, 1495,  199, 3431,
     3365,  642, 1523, 1364, 3353, 3280, 1536, 1516, 1534, 1540,
     1553, 1514, 1560, 1547, 1571, 1593, 1584, 1604, 1578, 1627,
     1602, 1635, 1634, 1645, 1667, 1668, 1676, 1671, 1694, 1707,
     1670, 1717, 1709, 1728, 1766, 1730, 1765, 1780, 1774, 1787,
     1823, 1820, 1831, 1822, 1844, 1855, 1845, 1878, 1727, 1830,

     1888, 1885, 1892, 1911, 1943, 1919, 1944, 1879, 1967, 1969,
     1975, 1999, 2000, 2009, 2031, 2036, 1793, 2075, 1825, 3267,
     1565,  144,  777, 3194, 9682, 3168, 1634, 1492, 1663, 2065,
     2082, 2103, 1788, 2160, 2240, 2084, 2154, 2078, 2149, 2083,
     2166, 1976, 2161, 2238, 2240, 1607, 2267, 1832, 2299, 2061,
     1465, 1459, 1479, 1623, 3117, 1834, 1934, 3114, 1263, 1975,
     2134, 2020, 3099, 3098, 2279, 2214, 1360, 2218, 2319, 2320,
     3033, 2329, 2330, 1641, 1065, 1528, 1478, 2268, 3012, 2988,
     2976, 2951, 2300, 1656, 2940, 1718, 2048, 2349, 2093, 2944,
     2941, 2376, 2386, 2889, 9682, 2378, 2871, 2850, 1626, 1654,

     1849, 1906, 1702, 1937, 1977, 2006, 2004, 2011, 2161, 2085,
     2054, 2086, 1724, 1747, 2352, 2367, 1756, 2162, 1832, 2447,
     1404, 1887, 2397, 2320, 1911, 1914, 2160, 2272, 2170, 2298,
     2358, 2171,  800,  683, 2365, 2242, 2336, 2339, 2379, 2385,
     2373, 2373, 2394, 2430, 2390, 2373, 2397, 2397, 2383, 2456,
     2403, 2404, 2392, 2420, 2427,    0, 2438, 2422, 2429, 2438,
     2436, 2441, 2441, 2449, 2460, 2480, 2439, 2440, 2464, 2469,
     2470, 2445, 2462, 2468, 2469, 2483, 2482, 2477,    0, 2481,
     2493, 2486, 2747, 2482, 2747, 2489, 2495, 2491, 2498, 2494,
     2500, 2539, 2540, 2543, 2515, 2521, 2525, 2529, 2529, 2535,

     2527, 2560, 2570, 2723, 2586, 2591, 2682, 2673, 2597, 2606,
     2610, 2641, 2640, 2594, 2584, 2576, 2600, 2604, 2580, 2613,
     2619, 2616, 2620, 2624, 2629, 2632, 2633, 2625, 2622, 2626,
     2639, 2634, 2645, 2703, 2706, 2655, 2711, 2680, 2652, 2658,
     2647, 2718, 2691, 2698, 2676, 2725, 2675, 2657, 2693, 2730,
     2733, 2738, 2720, 2740, 2781, 2753, 2735, 2765, 2756, 2751,
     2800, 2761, 2743, 2767, 2775, 2785, 2636, 2792, 2769, 2793,
     2803, 2797, 2808, 2809, 2804, 2824, 2847, 2827, 2830, 2818,
     2826, 2828, 2819, 2823, 2836, 2832, 2825, 2840, 2835, 2628,
     2851, 2862, 2852, 2911, 2915, 2578, 2926, 2982, 2605,  330,

     2271, 2922, 2895, 2864, 2931, 2932, 2937, 2940, 3055, 3135,
     2934, 2918, 2946, 2982, 2974, 2929, 2985, 2942, 2953, 2978,
     2970, 2838, 2840, 2851, 2715, 2865, 2829, 3040, 3022, 3014,
     3072, 2564, 3083, 3103, 3083, 3086, 3159, 3114, 3095, 2058,
     3162, 3165, 2947, 3087, 2548, 2528, 2459, 3171, 9682, 2456,
     9682, 9682, 9682, 9682, 3182, 9682, 2899, 2410, 9682, 2398,
     3030, 3117, 2372, 2365, 3188, 3207, 3226, 2364, 2322, 3236,
     2906, 2930, 2982, 3067, 3064, 3072, 3060,    0, 3144, 3166,
     3164, 3168, 3168, 3174, 3188, 3183, 3176, 3184, 2294, 2280,
     3206, 3197, 3250, 3325, 9682, 3204, 3218, 3223, 3207, 3225,

     3257, 9682, 3213, 3265, 9682, 3218, 3218,    0, 3222, 3271,
     3235, 3272, 9682, 3275, 3221, 3240,    0,  873, 2286, 2281,
     3299, 3252, 3249, 3254, 3259, 3271, 3278, 3272, 3270, 3275,
     3292, 3317, 3320, 9682, 3303, 3288, 3338, 3344, 3291, 3348,
     9682,    0, 3304,    0, 3321, 3293, 3305, 3325, 3312, 3318,
     3319, 3339, 3331, 3332,    0, 1862,    0, 3401, 9682, 3402,
     3339, 3349, 3344, 3350,    0, 3350, 3362, 3347,    0, 3356,
     3369,    0, 3409, 3370, 3378, 3382, 9682, 3386, 3373, 3394,
     3395, 3393, 3397, 3427, 3428, 3429, 3391,  219, 3410, 1176,
     3409, 3415, 3459, 3435, 3465, 3440, 3124, 3479, 3485, 3489,

     2264, 2234, 3452, 3437, 3457, 3482, 3471, 3436, 3489, 2204,
     3494, 3501, 3505, 3499, 3506, 3508, 3510, 3509, 3511, 3512,
      326, 3513, 3518, 3552, 3519, 3516, 3523, 3515, 3534, 3541,
     3522, 3538, 2150, 3520, 3595, 3559, 3599, 3565, 2144, 3555,
     3569, 3535, 3572, 3592, 3581, 3601, 3567, 3604, 3605, 3647,
     3602, 3587, 3654, 3659, 3630, 2111, 3619, 2089, 3611, 3626,
     3629, 3643, 3634, 3636, 3640, 3651, 3641, 3665, 2048, 2193,
     2033, 3684, 3671, 3673, 3672, 3677, 2003, 3644, 3689, 3686,
     2002, 3687, 3688, 1999, 3701, 3690, 3718, 3720, 3731, 3749,
     3655, 3778, 3737, 3754, 3764, 3719, 3734, 3774, 3716, 3851,

     3931, 3727, 3763, 3771, 3809, 3813, 3825, 3896, 3956, 3959,
     2131, 3793, 3959, 3880, 3724, 3681,    0, 3767,    0, 3951,
      532, 3962, 3524, 3818, 9682, 3962, 3975, 1979, 3784, 3983,
     4042, 9682, 9682, 1972, 9682, 9682, 3838, 3883, 3904, 3908,
     1972, 4069, 3845, 3852, 3775, 3782, 3854, 4126, 3856, 3858,
        0, 3929, 3865, 3926, 3939,    0, 3936, 3957, 3961, 3960,
     3968, 4036, 3981, 4006, 3968, 4038, 4045, 3967, 4043,    0,
        0, 4041, 4039, 4051, 4056, 4053, 4085, 4051, 4089, 9682,
        0, 4099, 9682, 4052, 9682, 4150, 4151, 4168, 4174, 4064,
     4070, 4088,    0, 4113, 4129, 4117, 4130, 4141, 4191, 4131,

     4192, 9682, 4156, 4169, 4199, 9682, 4203, 9682, 4166, 4174,
     4178, 4181, 4173, 4183,    0, 4184, 4183,    0, 4174, 4194,
     4193, 4220, 9682, 4231, 9682, 4181, 4189, 4197, 4212, 4199,
     4215, 4207, 4206, 4211, 4257, 9682,    0,    0, 4256, 2155,
     4231, 2188, 4237, 4222, 4273, 4263, 4266, 1960, 4237, 2463,
     4239, 2898, 4230, 4245, 4297, 9682, 4306, 4280, 4275, 3924,
     4023, 4156, 4286, 4279, 4295, 4300, 4303, 4301, 4368, 4325,
     4284, 1924, 4321, 4308, 4352, 4393, 1908, 4358, 4362, 4361,
     4394, 4397, 4398, 4401, 1905, 1902, 4400, 4399, 4408, 4404,
     4409, 4313, 4405, 1858, 4411, 4414, 4416, 4428, 1853, 4406,

     4425, 4407, 4412, 4415, 4329, 4436, 4440, 4432, 4453, 4458,
     4447, 4470, 4466, 4480, 1839, 4482, 4479, 1813, 4485, 4491,
     4486, 4487, 4490, 4493, 4494, 4495, 4497, 4500, 4499, 4503,
     1806, 1801, 4342, 4562, 4575, 4269, 4579, 3034, 4567, 4583,
     4531, 1760, 4599, 4523, 3199, 4669, 4749, 4293, 4480, 4492,
     4561, 4563, 4577, 4829, 4496, 4585, 4571, 4499,    0, 9682,
        0,    0,  576, 1740, 1720, 4021, 4346, 4589, 1709, 4643,
     4647, 3877, 4909, 4534, 4631, 4694, 4715, 4721, 1666, 9682,
     4591, 4657, 4699, 4776, 4703, 4722, 4989, 4274, 4463,    0,
     4568, 4580,    0, 4657, 4665, 4669, 4670, 4638, 4731, 4666,

     4821, 4737, 4680, 4680,    0, 4753, 9682, 9682, 4748, 4746,
     4758, 4759, 4760, 4749, 4756, 4792, 4803, 9682, 4771, 4760,
     4855, 4856, 4872, 4871, 1501, 1663, 4941, 4888, 4829,    0,
     4834, 4835, 4812,    0, 4894, 4894, 4873, 9682, 4945, 4905,
     4946, 4902, 4910, 4907, 5014, 4882, 4915, 4921, 4941, 4987,
     4981,    0, 4986,    0,    0,    0, 5020, 5021, 5026, 4981,
        0, 4854, 9682, 4993, 4992, 5000, 5030, 1651, 1643, 5003,
     4997, 3373, 5011, 5036, 5029, 3446, 3623, 4902, 4971, 5049,
     5068, 5096, 5071, 5084, 1642, 5120, 5079, 1631, 5076, 5083,
     5081, 5082, 5037, 5086, 5126, 5130, 5090, 1619, 5032, 5127,

     5134, 5135, 5133, 5137, 5139, 5140, 5148, 1612, 5142, 5144,
     5159, 1611, 5163, 5164, 5181, 5176, 5188, 5166, 5170, 5174,
     5193, 5178, 5197, 5185, 5209, 5215, 5213, 1574, 5219, 1554,
     1549, 1506, 5244, 5256, 5259, 5220, 1496, 4975, 5264, 5195,
     1447, 1426, 5224, 5301, 5381, 5461, 5006, 5183, 5197, 5225,
     5254,    0, 3843, 5272, 5276, 9682,    0, 1406, 1387, 4788,
     5065, 1375, 5274, 5327, 4807, 5353, 5354, 5359, 1365, 3768,
     4728, 5331, 5412, 5328, 9682, 5335, 9682,    0, 5321, 5214,
     5310, 5371,    0, 5379, 5300, 5380, 5385,    0, 5378, 5453,
     5379, 5377, 5386, 9682, 5391, 5380, 5394, 5397, 5389, 5401,

        0, 5429, 5457, 5340, 9682, 5452, 5451, 5491, 5483, 5521,
     5446,    0,    0,    0, 5458, 5464, 5507, 9682,    0, 5499,
     5456, 5457, 5533,    0, 5540, 9682, 5483, 5499,    0, 5489,
        0,    0, 5485, 5545, 5517, 5550, 9682, 5551, 9682, 5516,
     5500, 4743, 5525,  602, 1367, 1306, 5515, 4980, 5533,  783,
     5543, 5076, 9682, 5230, 5284, 1274, 5562, 5559, 5563, 5566,
     1259, 5567, 5573, 5574, 5579, 1256, 5575, 5582, 5580, 5581,
     5592, 5591, 1219, 5578, 5597, 5595, 5590, 5600, 1214, 1209,
     1202, 5596, 5606, 1188, 5603, 5607, 5651, 1176, 5621, 5608,
     1166, 5625, 1156, 1153, 5637, 5639, 5613, 5287, 1139, 5599,

     1122,  818,    0, 5619,    0, 5631, 5625, 5630, 5632, 5668,
     5433, 5292, 5685, 5697, 5716, 5650,    0, 5642, 5649, 5639,
     5644, 5639,    0, 5658, 5666, 5674, 5672, 5679, 5663, 5686,
     5681, 5697, 5700, 9682, 5699, 5687,    0, 5695,    0,    0,
     5746,    0, 5708, 5742, 5696, 5700,    0, 5745, 9682,    0,
     5710, 5769, 5724, 5729, 5720, 5733, 5730, 5747, 5774, 5747,
        0, 5748, 5776, 5777, 5783, 5792,    0, 5783, 1109, 5779,
     5786, 5787, 5788, 5792, 1075, 5795, 5808, 5796, 5798, 5806,
     5811, 5809, 1067, 5810, 1055, 1050, 1049, 5816, 5818, 5812,
     1044, 1034, 5820, 5857, 5826, 5821, 5828, 5884, 5440, 5845,

     5782, 5797,    0, 5804,    0, 5866, 5888,  883, 5897, 5902,
     5905, 5909, 5927, 5931, 5814, 5849,    0,    0, 5853, 5870,
     5846,    0, 5878, 5888, 5876, 5892,    0, 5893, 5936, 5882,
     9682,    0, 5908,    0, 5949, 9682, 5899, 5915,    0, 5952,
     9682,    0, 5913, 5928, 5929, 9682, 5930, 5958, 5922,    0,
     5960, 5965, 5973,    0, 5963, 5961, 1018,  995, 5966, 5968,
     5971,  969, 5967, 5976,  961, 5980,  950, 5990,  939, 5991,
      928,  868, 5983, 5992, 6003, 4403, 5977,    0, 5936, 5952,
      857,  917, 6050, 1078, 6054, 6063, 6066,  843, 5972, 6075,
        0,    0,    0, 5956, 5975, 5973, 5980, 6076, 6079, 9682,

     9682, 5991, 6018, 6031, 6042,    0,    0, 9682,  903,  780,
     6071, 6081, 6059, 6090,  724,  704,  694, 6083, 6092, 6093,
     6094,  658,  650, 6095, 6108, 6129, 6104, 1054,    0,    0,
     6141, 6154, 6127, 6158, 6168,  649,    0, 6123, 6177, 5979,
     6065, 6091, 6075,    0, 6136, 6140, 9682, 6146, 6135, 1145,
     6173,  638, 6177, 6172,  603, 6180, 6184, 6215, 6195,  596,
      582, 6237, 6249, 6201, 6188, 6202, 6263, 6218, 6254, 6277,
     6163, 6162, 6184, 6174, 6206, 6200, 6220, 6217, 6246,  544,
     6221, 6277, 6245, 6282, 6291,  519,  477, 6305, 6318, 6331,
     6314, 6335, 6339, 6351, 6347, 6297, 6364, 6368, 6373, 6102,

     9682, 6248, 6249,    0, 6262, 6305, 6316, 6340, 6317, 6323,
      463, 9682,  462, 6344, 6355, 6390, 6395, 6401, 6405, 6414,
     6418, 6422, 6426, 6438, 6443, 6441, 6455, 6459, 6464, 6430,
     9682, 6348, 6363, 6407, 6425, 1456,  399, 1516, 6431, 6446,
     6477, 6481, 6486, 6504, 6508, 6520, 6491, 6533, 6516, 1766,
     6453,    0, 6441,    0, 6494, 6482, 6512,  437,  432, 5344,
     6579, 6555, 6559, 6537, 6603, 6539, 6525, 9682, 9682, 6528,
     6502, 6553, 6582, 6607, 6659, 6624, 6612, 6571, 6594, 6559,
     6632, 6634, 6641, 6637, 6645, 6683, 6649, 6575, 6568, 2307,
     6687, 6675, 6653, 6621, 6654, 6664, 6712, 6716, 6692, 6720,

     6729, 6738,  416, 6742, 6746,  405, 6699, 6750, 6757,  370,
     6762, 6766,  352,  316, 6770, 6774,  212, 6778, 6781,  208,
     6783,  201, 6753, 6787, 6791, 6797,  178, 6801, 6806,  119,
      115, 6810,   83, 6814, 9682, 6836, 6854, 6872, 6890, 6908,
     6926, 6943, 6947, 6965, 6983, 7001, 7019, 7035, 7053, 7071,
     7089, 7107, 7125, 7143, 7160, 7177, 7182,   84, 7200, 7218,
     7236, 7254, 7272, 7290, 7308, 7326, 7344, 7362, 7380, 7398,
     7416, 7434, 7452, 7470, 7487, 7503, 7508, 7525, 7543, 7561,
     7579, 7584, 7602, 7615, 7630, 7648, 7666, 7684, 7702, 7720,
     7738, 7756, 7774, 7790, 7808, 7826, 7844, 7862, 7880, 7898,

     7916, 7934, 7951, 7967, 7984, 8002, 8020, 8038, 8056, 8061,
     8079, 8097, 8115, 8133, 8151, 8169, 8187, 8205, 8223, 8241,
     8259, 8277, 8295, 8313, 8331, 8349, 8367, 8384, 8389, 8405,
     8422, 8440, 8458, 8476, 8494, 8512, 8530, 8548, 8566, 8584,
     8602, 8620, 8638, 8656, 8674, 8692, 8710, 8728, 8746, 8764,
     8782, 8800, 8818, 8836, 8853, 8871, 8888, 8904, 8909, 8926,
     8944, 8962, 8980, 8998, 9016, 9034, 9052, 9070, 9087, 9104,
     9122, 9140, 9158, 9176, 9194, 9212, 9230, 9247, 9264, 9280,
     9285, 9301, 9317, 9334, 9339, 9357, 9375, 9393, 9411, 9429,
     9447, 9465, 9483, 9501, 9519, 9537, 9555, 9573, 9591, 9609,

     9627, 9645, 9663
    } ;

static yyconst flex_int16_t yy_def[2104] =
    {   0,
     1935,    1, 1936, 1936,    1,    1, 1937, 1937, 1938, 1938,
     1936, 1936, 1935,   13,    1,    1, 1935, 1935, 1935, 1935,
     1939, 1940, 1935, 1935, 1935, 1941, 1942, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1943, 1943,
     1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943,
     1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943, 1935,
     1935, 1944,   41, 1943, 1943, 1943, 1943, 1935, 1945, 1935,
     1945, 1946, 1935, 1946, 1946, 1935, 1935, 1947, 1935, 1948,
     1948, 1948, 1948,   83,   83,   83, 1948, 1948,   83,   83,
       83,   83, 1948,   93,   83,   83, 1948,   93, 1948, 1948,

     1935,   60, 1949, 1935, 1935,   83,   83,   88,   82,   60,
      104, 1935, 1935, 1935, 1950, 1950, 1950, 1951, 1935, 1951,
     1951, 1935, 1952, 1953, 1954, 1953, 1935, 1953, 1953, 1955,
     1955, 1935, 1955, 1955, 1955, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1956, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,

     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1958,   60,
     1935, 1959, 1935, 1935, 1935, 1935, 1935, 1935, 1960, 1935,
     1960, 1960, 1960, 1935, 1957, 1957, 1957, 1957, 1957, 1957,
     1961, 1935, 1961, 1962, 1935, 1962, 1962, 1962, 1935, 1963,
     1935, 1935, 1935, 1935, 1964, 1965, 1935,   88,   88,  259,
      259,  259,  259,  259,  259,  259,  259,  259,  259,  259,
      259,  259,  259,  259,  259,  259,  259,  259,  259,  259,
      259,  259,  259,  259,  259,  259,  259,  259,  259,  259,
      259,  259,  259,  259,  259,  259,  259,  259,  259,  259,

      259,  259,  259,  259,  259,  259,  259,  259,  259,  259,
      259,  259,  259,  259,  259,  259, 1935, 1935, 1935, 1966,
      220,  321, 1935, 1967, 1935, 1967, 1967, 1967, 1935, 1935,
     1935, 1935, 1967, 1968, 1968,  335,  335,  335,  335,  335,
      335,  259,  259,  259,  259,  220, 1935, 1935, 1935, 1935,
     1935, 1935, 1969, 1969, 1970, 1970, 1970, 1971, 1972, 1972,
     1972, 1972, 1935, 1973, 1974, 1974, 1935, 1975, 1935, 1976,
     1977, 1976, 1976, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1978,
     1979, 1935, 1935, 1980, 1935, 1981, 1935, 1935, 1982, 1982,

     1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982,
     1982, 1982, 1982, 1982, 1935, 1982, 1982, 1982, 1982, 1982,
     1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982,
     1935, 1982, 1935, 1983, 1982, 1982, 1982, 1982, 1982, 1982,
     1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982,
     1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982,
     1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982,
     1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982,
     1982, 1982, 1984, 1935, 1985, 1935, 1935, 1935, 1935, 1935,
     1935, 1986, 1986, 1986, 1935, 1982, 1982, 1982, 1982, 1982,

     1982, 1987, 1988, 1989, 1935, 1935, 1990, 1991, 1935, 1935,
     1935, 1992, 1993, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1935, 1935, 1995, 1935, 1935, 1935,  598,

     1935, 1935, 1996, 1996, 1935, 1935, 1935, 1996, 1997, 1997,
      610,  610,  610,  610,  610,  610,  610, 1994, 1994, 1994,
     1994, 1935, 1935, 1935, 1935, 1998, 1998, 1999, 1999, 2000,
     2001, 2002, 2001, 2001, 2003, 2003, 2003, 1935, 1935, 2004,
     2005, 2005, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 2006, 2007, 1935, 1935, 1935, 2008, 2009, 1935,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 1935, 2010,
     2010, 2010, 2010, 1935, 1935, 2010, 2010, 2010, 2010, 2010,

     1935, 1935, 2010, 1935, 1935, 2010, 2010, 2010, 2010, 2010,
     2010, 1935, 1935, 2010, 2010, 1935, 2010, 2011, 2012, 2013,
     2011, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 2010, 1935, 1935, 2010, 2010, 2010, 2010, 2010, 1935,
     1935, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 1935, 1935, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 2014, 2014, 2014, 1935, 2010, 2010, 2010,
     2010, 2010, 2010, 2015, 2016, 2016, 1935, 1935, 1935, 1935,

     2017, 2018, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 2019, 2019, 1935, 2019, 2020,

     2020,  901,  901,  901,  901,  901,  901,  901,  901,  901,
     1994, 1994, 1994, 1994, 1935, 1935, 2021, 2022, 2023, 2024,
     2025, 2026, 2027, 1935, 1935, 1935, 2028, 2029, 2030, 2031,
     2032, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     2033, 1935, 2010, 2010, 2010, 2010, 2010, 2034, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 1935, 2010, 2010, 1935, 1935, 1935, 1935, 1935, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 1935, 1935,
     2010, 1935, 1935, 2010, 1935, 2035, 2036, 2037, 2038, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,

     1935, 1935, 2010, 2010, 1935, 1935, 1935, 1935, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 1935, 1935, 1935, 1935, 2010, 2010, 2010, 2010, 2010,
     2010, 2010, 2010, 2010, 1935, 1935, 2010, 2010, 2010, 1935,
     1935, 1935, 1935, 1935, 1935, 2039, 2039, 2040, 1935, 1935,
     2010, 1935, 2010, 2010, 1935, 1935, 1935, 2041, 2042, 1935,
     1935, 1935, 1935, 1994, 1994, 1994, 1994, 1994, 2043, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,

     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     2044, 2045, 1935, 2044, 2044, 2046, 2046, 1147, 1147, 1147,
     1147, 1147, 1147, 2047, 1147, 1994, 1994, 1935, 2048, 1935,
     2049, 2050, 2051, 2052, 1935, 2053, 2054, 2054, 1935, 1935,
     1935, 2055, 2056, 1935, 2057, 1935, 2058, 2058, 2059, 1935,
     1935, 1935, 1935, 1935, 2010, 2010, 2060, 2010, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,

     2010, 2010, 1935, 1935, 2010, 2010, 1935, 1935, 1935, 1935,
     1935, 2010, 2010, 2010, 2010, 2010, 1935, 1935, 2010, 2010,
     2061, 2061, 2062, 2063, 2064, 2063, 2064, 2064, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 1935, 1935, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 1935, 1935, 1935, 1935, 1935, 2065, 2066, 2065, 1935,
     2010, 2010, 2010, 2067, 2068, 1935, 2069, 1935, 1935, 1994,
     1994, 2070, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,

     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1935, 2069, 1935,
     2071, 2072, 2072, 2073, 2074, 2074, 1346, 1346, 1346, 1346,
     1346, 1346, 1994, 1994, 1935, 1935, 2075, 2076, 1935, 2077,
     2077, 1935, 2078, 1935, 2079, 1935, 2080, 2080, 2081, 1935,
     2082, 1935, 1935, 1935, 1935, 1935, 1935, 2010, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 1935,
     2010, 2010, 2010, 1935, 1935, 2010, 1935, 1935, 1935, 2010,

     2010, 2010, 2010, 1935, 1935, 2010, 2010, 2063, 2063, 2064,
     2010, 2010, 2010, 2010, 2010, 2010, 1935, 1935, 2010, 1935,
     2010, 2010, 2010, 2010, 1935, 1935, 2010, 2010, 2010, 2010,
     2010, 2010, 2010, 1935, 2010, 1935, 1935, 1935, 1935, 2010,
     1935, 1935, 1935, 2065, 2065, 1935, 2010, 1935, 2010, 2067,
     2068, 1935, 1935, 1935, 2083, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1935, 2084, 1935,

     2072, 2072, 1346, 1346, 1346, 1346, 1346, 1346, 1346, 1994,
     1935, 1935, 1935, 1935, 2082, 2010, 2010, 2010, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 1935, 1935, 2010, 2010, 2010,
     1935, 2010, 1935, 1935, 1935, 2010, 2010, 2010, 2010, 2010,
     2063, 2010, 2010, 2010, 1935, 2010, 2010, 1935, 1935, 2010,
     2010, 2010, 2010, 1935, 2010, 2010, 1935, 1935, 2065, 1935,
     2010, 2010, 2067, 2068, 1935, 1935, 2085, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1935, 1935, 2072,

     1346, 1346, 1346, 1346, 1346, 1994, 1935, 2086, 1935, 1935,
     1935, 2087, 1935, 1935, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 2010, 1935, 1935, 2010, 2010, 2010, 1935, 2010, 1935,
     1935, 2010, 2010, 2010, 1935, 1935, 1935, 2010, 2010, 1935,
     1935, 2010, 1935, 2010, 2010, 1935, 1935, 2065, 1935, 2010,
     2067, 2068, 1935, 2085, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1935, 2088, 2072, 1346, 1346, 1346,
     1994, 2086, 2086, 2086, 1935, 2087, 2087, 2087, 2010, 2010,
     2010, 2010, 2010, 1935, 1935, 2010, 2010, 1935, 1935, 1935,

     1935, 2010, 1935, 2010, 1935, 2010, 2010, 1935, 2065, 1935,
     2067, 2068, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 2088, 1935, 2088, 2088, 2072, 1346, 1346,
     2086, 2086, 2089, 2087, 1935, 2087, 2010, 1935, 1935, 1935,
     1935, 2010, 2010, 2010, 1935, 2010, 1935, 2065, 1935, 2067,
     2068, 1994, 1994, 1994, 1994, 1994, 1935, 1935, 1935, 2090,
     2091, 2088, 2088, 2092, 2072, 2089, 2089, 2089, 1935, 1935,
     1935, 1935, 2010, 2010, 1935, 2010, 2065, 1935, 2067, 2093,
     1994, 1994, 1994, 1935, 1935, 2090, 2091, 2088, 2088, 2088,
     2094, 2095, 2092, 2092, 2092, 2072, 2089, 2086, 2089, 1935,

     1935, 1935, 1935, 2010, 2010, 1935, 2010, 2065, 1935, 2067,
     2093, 1935, 1994, 1994, 1994, 1935, 1935, 2088, 2088, 2094,
     2094, 2094, 2095, 1935, 2095, 2095, 2092, 2088, 2092, 2072,
     1935, 1935, 2010, 1935, 2010, 2065, 1935, 2067, 1994, 1994,
     1935, 1935, 2088, 2088, 2094, 2088, 2094, 2095, 2096, 2072,
     1935, 2010, 1935, 2010, 2065, 1935, 2067, 1994, 1994, 1935,
     2088, 2088, 2088, 2096, 2096, 2096, 2072, 1935, 1935, 2065,
     1935, 2067, 1935, 2088, 2097, 2096, 2096, 2072, 2065, 1935,
     2067, 1935, 2088, 2092, 2088, 2088, 2072, 2065, 1935, 2067,
     2088, 2072, 2065, 1935, 2067, 2072, 2065, 1935, 2067, 2072,

     2065, 1935, 2098, 1935, 1935, 2099, 2067, 2072, 1935, 2100,
     1935, 1935, 2101, 2098, 1935, 1935, 2099, 1935, 2067, 2100,
     1935, 2101, 2067, 2067, 2067, 1935, 2102, 1935, 1935, 2103,
     2102, 1935, 2103, 1935,    0, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,

     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,

     1935, 1935, 1935
    } ;

static yyconst flex_uint16_t yy_nxt[9763] =
    {   0,
       18,   19,   20,   19,   21,   22,   18,   23,   24,   25,
       26,   27,   28,   29,   28,   30,   28,   31,   32,   33,
       34,   35,   36,   37,   38,   39,   40,   41,   42,   43,
       44,   45,   46,   47,   46,   48,   49,   50,   51,   52,
       53,   46,   54,   55,   56,   57,   46,   58,   46,   46,
       59,   28,   28,   28,   39,   40,   41,   42,   43,   44,
       45,   46,   47,   48,   49,   50,   51,   52,   53,   46,
       54,   55,   56,   57,   46,   58,   46,   46,   59,   18,
       60,   61,   60,   62,   70, 1934,   71,   74,   73,   74,
       76,   77,   76,   76,   77,   76,  242,  483,  483,   78,

      112,  243,   78,  114,  245,  114,   63,   64,  116,   71,
       65,  249,   66,  249,  113,   75,  112, 1932,  119,  342,
      112, 1934,  117,   67,  228,  221,  228,  222,  344,  112,
      113,  114,  112,  114,  113,   63,   64,  116,   71,   65,
      248,   66,  113,  120,   75,  112,  113,  600,  342,  112,
      117,   67,   60,   61,   60,   62,  344,  121,  113,  112,
      136,  112,  113,  601,  313,  155,  258,  112,  248,  314,
      352,  258,  120,  113,  113,  112,  112,  112,   63,   64,
     1932,  113,   65,  258,   66,  121,  351,  258,  112,  113,
      113,  113,  313,  284,  258,   67,  112,  314,  352,  258,

      249,  113,  249, 1915,  112,  112,  112,   63,   64,  113,
     1921,   65,  258,   66, 1918,  351,  258,  113,  113,  113,
     1050,  284, 1050,   67,   18,   19,   79,   19,   21,   22,
       18,   23,   24,   25,   26,   27,   28,   29,   28,   30,
       28,   31,   32,   33,   34,   35,   36,   37,   38,   80,
       81,   82,   83,   84,   85,   86,   87,   88,   87,   89,
       90,   91,   92,   93,   94,   87,   95,   96,   97,   98,
       87,   99,   87,   87,  100,   28,   28,   28,   80,   81,
       82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
       92,   93,   94,   87,   95,   96,   97,   98,   87,   99,

       87,   87,  100,   18,   60,  101,  102,   62,  156,  122,
      122,  122,  123,  125,  353,  112,  103,  321, 1915,  322,
      126,  127,  104,  354,  401,  157,  158,  119,  257,  113,
      105,  106,  112,  892,  107,  323,  108,  397,  112,  397,
      377,  128,  398,  353,  112,  821,  113,  109,  346,  893,
      346,  354,  113,  401, 1915,  129,  130,  113,  130,  105,
      106,  112,  378,  107,  357,  108,  347,  112,  377,  131,
      128,  131, 1921,  132,  113,  109,  110,   61,  110,   62,
      113,  166,  162,  129,  159,  134,  162,  146,  147,  146,
      378,  112,  357,  405,  111,  148,  165,  385,  149,  135,

      165,  386,   63,   64,  150,  113,   65, 1918,   66,  151,
      166,  162,  162,  258,  134,  162,  112,  289, 1915,   67,
      112,  186,  405,  258,  165,  385,  165,  135,  165,  386,
      113,   63,   64,  113,  257,   65,  125,   66,  348,  257,
      348,  162,  258,  126,  127,  112,  289,   67,  137,  186,
      137,  258, 1856,  119,  165,  219,  349,  152,  113,  153,
      122,  122,  122,  123,  257, 1812,  138,  350,  154,  154,
      112,  161,  139,  356,  162,  162,  140,  112,  141, 1758,
      163,  154,  402,  142,  113,  143,  144,  164,  165,  165,
      403,  113,  246,  245,  246,  145,  350,  154,  154,  112,

      161,  139,  356,  162,  162,  140,  112,  141,  163,  154,
      402,  142,  113,  143,  144,  164,  165,  165,  403,  113,
      247, 1758,  404,  145,  167,  162,  172,  162,  191,  162,
      173,  168,  192,  162,  118,  169,  174, 1165,  170,  165,
      230,  165,  193,  165,  125,  231, 1812,  165,  171,  247,
      404,  126,  127,  167,  162,  172,  162,  191,  162,  173,
      168,  192,  162,  169,  174,  187,  170,  165,  232,  165,
      193,  165,  188,  189,  190,  165,  171,  162,  118,  165,
      406, 1359,  233,  175, 1758,  176,  177,  257,  178,  179,
      408,  165,  414,  407,  187,  180,  411,  232, 1758,  290,

      188,  189,  190,  258,  230,  257,  162,  165,  406,  291,
      233,  175,  162,  176,  177,  260,  178,  179,  408,  165,
      414,  194,  407,  180,  162,  411,  165,  181,  290,  263,
      182,  183,  258,  184,  162,  207,  162,  291,  165,  185,
      257,  162,  215,  505,  260,  505,  255,  216,  165,  194,
      165,  670,  257,  162,  165, 1559,  181,  263,  182,  183,
      257,  184,  421,  162,  207,  162,  165,  185,  195,  162,
      215,  379,  196,  162,  381,  216,  165,  200,  165,  201,
      197,  234,  198,  165,  202,  719,  380,  165,  199,  382,
      421,  409,  410,  420,  720,  721,  257,  195,  162,  235,

      379,  196,  162,  381,  203,  200,  257,  201,  197,  162,
      198,  165,  202,  236,  380,  165,  199,  382,  204,  409,
      410,  205,  420,  165,  206,  208,  257,  258,  235,  209,
      162,  258,  445,  203,  360,  361,  360,  292,  162,  210,
      125,  236,  362,  363,  211,  212,  204,  126,  127,  205,
      213,  165,  206,  475,  208,  162,  258,  214,  209,  162,
      258,  445,  162,  365,  217,  292,  412,  210,  130,  165,
      130,  413,  211,  212,  218, 1935,  165, 1935,  329,  213,
      602,  131,  475,  131,  162,  242,  214,  422, 1935,  383,
     1935,  162,  365,  217,  423,  412,  601,  165,  257,  384,

      413,  433,  218,  433,  165,  220,  221,  220,  222,  237,
      162,  434,  399,  172,  162,  422,  238,  173,  383,  186,
      325,  264,  423,  174,  165,  426,  260,  384,  165,  400,
      427,  223,  224, 1749,  112,  225, 1563,  226,  237,  162,
      263,  399,  172,  162,  238,  670,  173,  186,  227,  428,
      264,  174,  165,  426,  477,  260,  165,  400,  427,  257,
      223,  224,  213,  112,  225,  187,  226,  162,  263,  214,
      257, 1600,  188,  239,  190,  719,  227,  428,  257,  165,
      257,  165,  424,  477,  720,  721,  240,  252,  253,  254,
      255,  213, 1683,  425,  187, 1684,  162,  257,  214,  256,

      188,  239,  190,  259,  418,  230,  260,  165,  285,  165,
      429,  424,  261,  256,  240,  286,  287,  288,  419,  262,
      263,  425,  263,  257,  270,  260, 1683,  430,  271, 1684,
      257,  478,  259,  418,  272,  260,  435,  285,  429,  263,
      261,  257,  256,  286,  287,  288,  419,  262,  263,  257,
      263,  260,  257,  270,  260,  430, 1748,  271,  415,  298,
      478,  299,  272,  257,  435,  263,  300,  263,  146,  147,
      146,  257,  265,  479,  258,  482,  148,  260,  279,  266,
      260,  280,  281,  267,  282,  150,  268,  298,  305,  299,
      283,  263,  258,  263,  300,  443,  269,  257,  258,  416,

      417,  265,  479,  258,  482,  444,  260,  279,  266,  280,
      281,  267,  282,  488,  268,  448,  230,  305,  283,  263,
      257,  258,  257,  443,  269,  258,  258,  416,  417,  258,
      219,  449,  273,  444,  274,  275,  257,  276,  277,  492,
      125,  293,  488,  448,  278,  294,  257,  126,  127,  258,
      260,  257,  257,  295,  258,  296,  325,  257,  258,  449,
      273,  297,  274,  275,  263,  276,  277,  446,  492,  257,
      293,  301,  278,  495,  294,  447,  257,  257,  258,  260,
      366,  295,  450,  296,  258,  302,  258, 1683,  303,  297,
     1684,  304,  263,  431,  644,  431,  446,  257,  451,  311,

      301,  306,  495,  447,  260,  307,  312, 1765,  366,  258,
      450,  257,  258,  302,  258,  308,  303,  452,  263,  304,
      309,  310,  138,  644,  325,  260,  451,  315,  311,  325,
      306,  376,  376,  260,  307,  312,  432,  316,  258,  263,
      326,  230,  453,  308,  376,  452,  263,  242,  309,  310,
      317,  318,  319,  320,  260,  257,  315,  327,  257,  258,
      376,  376,  256,  258,  432,  316,  343,  263,  257,  284,
      453,  328,  376,  137,  456,  137,  256, 1052,  257, 1052,
      454,  387,  388,  389,  390,  461,  327,  470,  258,  311,
      257,  462,  258,  391,  343,  455,  312,  284, 1779,  328,

      258,  374,  456,  258,  257,  256,  329,  391,  330,  454,
      375,  257,  471,  345,  461,  470,  257,  396,  311,  462,
      145,  257,  152,  455,  331,  312,  154,  154,  258,  457,
      374,  258,  458,  154,  154,  112,  391,  459,  375,  154,
      471,  345,  392,  393,  392,  394,  154,  460,  145,  113,
      148,  228,  221,  228,  222,  154,  154,  457,  257,  150,
      458,  257,  154,  154,  112,  361,  459,  154,  228,  221,
      228,  222,  630,  363,  154,  460,  257,  113,  324,  324,
      332,  324,  324,  324,  324,  333,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  334,  324,  324,

      324,  324,  324,  335,  334,  334,  334,  334,  336,  334,
      337,  334,  334,  334,  338,  334,  334,  339,  334,  334,
      334,  334,  340,  334,  334,  334,  334,  341,  334,  324,
      324,  334,  335,  334,  334,  334,  334,  336,  334,  337,
      334,  334,  338,  334,  334,  339,  334,  334,  334,  334,
      340,  334,  334,  334,  334,  341,  334,  324,  367, 1560,
      367,  367,  472,  367,  367,  505,  367,  506,  255,  230,
      476, 1935,  368, 1935,  369,  368,  370, 1935,  368, 1935,
      369,  367,  370,  367,  639,  364,  440,  441,  372,  152,
      472,  153,  355,  442, 1935,  368, 1935,  369,  476,  370,

      154,  154,  433,  463,  433,  701,  436,  701,  230,  489,
      371,  119,  434,  154,  440,  441,  371,  372,  437,  464,
      438,  442,  373,  439,  230,  473,  401,  702,  325,  154,
      154,  493,  463,  371,  465,  436,  466,  489,  490,  474,
      480,  154,  404, 1935,  481,  486,  437,  464,  438,  325,
      373,  439,  467,  468,  473,  401,  469,  484,  230,  487,
      493,  352,  498,  465,  494,  466,  490,  474,  491,  480,
      404,  499, 1935,  481,  486,  228,  221,  228,  222,  406,
      467,  468,  409,  497,  469,  484,  496,  487,  436,  352,
      498,  501,  494,  242,  325,  245,  491,  245,  257,  499,

      437,  624,  438,  719,  625,  500,  647,  406,  257, 1855,
      409,  497, 1226, 1227,  496,  626,  502,  436,  242,  501,
      246,  245,  246,  503,  252,  253,  254,  255,  437,  624,
      438,  248,  625,  500,  604,  647,  256,  509,  510,  511,
      512,  258,  645,  626,  645,  502,  258,  646,  247,  513,
      256,  257,  503,  258,  258,  258,  257,  518,  258,  248,
      258,  514,  604,  513,  258,  516,  598,  258,  598, 1857,
      258,  258,  258,  258,  520,  258,  257,  247,  515,  256,
      258,  258,  258,  258,  258,  518,  258,  258,  258,  517,
      514,  258,  513,  258,  516,  599,  258,  258,  258,  258,

      258,  258,  519,  520,  258,  526,  515,  521,  346,  258,
      346,  258,  258,  257,  257,  258,  258,  517,  522,  258,
      258,  257,  258,  523,  599,  258,  347,  258,  258,  258,
      519,  258,  258,  257,  526,  521,  325,  258,  415,  529,
      258,  524,  525,  258,  257,  230,  258,  522,  258,  258,
      258,  523,  527,  230,  258,  671,  258,  528,  258,  603,
      258,  258,  258,  532,  605,  258,  605,  529,  627,  524,
      525,  258,  258,  534,  258, 1408,  258,  533,  258,  530,
      531,  527,  643,  258,  671,  639,  528,  672,  603,  258,
      258,  258,  532,  599,  258,  258,  627,  258,  258,  258,

      658,  258,  534,  258,  538,  533,  258,  530,  531,  535,
      643,  258,  258,  536,  542,  539,  672,  258,  537,  364,
      258,  258,  599,  258,  258,  355,  258,  258,  658,  431,
      540,  431,  258,  538,  258,  675,  258,  535,  258,  258,
      258,  536,  542,  539,  258,  119,  537,  544,  258,  687,
      258,  258,  541,  258,  258,  258,  565,  258,  540,  543,
      660,  258,  325,  258,  675,  258,  258,  433,  325,  433,
      546,  566,  545,  258,  258,  544,  688,  434,  687,  258,
      541,  258,  691,  258,  258,  565,  258,  543,  660,  258,
      325,  516,  547,  258,  594,  221,  594,  320,  546,  566,

      545,  258,  258,  257,  548,  688,  549,  258,  257,  550,
      258,  691,  554,  608,  258,  257,  551,  552,  258, 1867,
      516,  547,  258,  553,  258,  555,  594,  221,  595,  320,
      258,  258,  548,  348,  549,  348,  258,  550,  258,  119,
      554,  257,  608,  258,  551,  552,  258,  258,  557,  258,
      258,  553,  258,  555,  556,  257,  558,  258,  258,  258,
      257,  693,  622, 1022,  258, 1022,  258,  258,  559,  560,
      628,  258,  258, 1023,  567,  258,  258,  557,  258,  258,
      561,  563,  258,  556,  558,  673,  258,  258,  258,  258,
      693,  622,  258,  562,  258,  258,  559,  560,  628,  258,

      258,  258,  567,  258,  257,  258,  258,  257,  561,  563,
      257,  258,  258,  673,  564,  258,  258,  258,  570,  258,
      703,  562,  258,  258,  583,  572,  257,  258,  571,  258,
      568,  573,  258,  569,  258,  258,  258,  574,  258,  119,
      709,  258,  564,  710,  258,  674,  258,  570,  258,  703,
      258,  258,  583,  575,  572,  258,  571,  258,  568,  573,
      258,  569,  230,  258,  258,  581,  574,  258,  576,  709,
      577,  258,  710,  674,  942,  258,  360,  361,  360,  629,
      582,  575,  676,  258,  362,  363,  578,  579,  258, 1180,
      580,  258,  584,  581,  258,  586,  258,  576, 1171,  577,

      258,  257,  258,  258,  257,  257,  585,  629,  582,  519,
      676,  258,  677,  258,  578,  579,  258,  587,  580,  258,
      618,  584,  361,  258,  586,  258,  258,  258,  588,  630,
      363,  258,  258,  589,  585,  257,  258,  519,  590,  258,
      677,  258,  678,  258,  258,  587,  679,  258,  618,  661,
      257,  661,  390,  258,  680,  258,  258,  588,  258,  367,
      591,  367,  589,  258,  592,  258,  605,  590,  606,  593,
      678,  258,  258,  368,  679,  258,  317,  318,  319,  320,
      258,  258,  680,  329,  601,  602,  351,  258,  256,  591,
      685,  257,  258,  592,  661,  599,  662,  390,  593,  152,

      623,  607,  256,  258,  509,  510,  511,  512,  258,  611,
      154,  154,  683,  257,  334,  351,  513,  613,  685,  334,
      334,  334,  686,  154,  599,  616,  334,  334,  623,  684,
      513,  256, 1052,  257, 1052,  360,  361,  360,  611,  154,
      154,  683,  334,  362,  363,  613,  257,  334,  334,  334,
      686,  154,  257,  616,  334,  334, 1050,  684, 1050,  513,
      324,  324,  332,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  612,
      324,  324,  324,  324,  324,  334,  614,  615,  258, 1052,
      334, 1052,  334,  617, 1022,  257, 1022,  334,  681,  692,

      619,  682,  334,  711, 1023,  258,  257,  714,  612,  334,
      717,  324,  324,  334,  614,  615,  125,  258,  334,  635,
      334,  635,  617,  126,  127,  334,  681,  692,  619,  682,
      334,  711,  636,  258,  636,  714,  510,  334,  717,  324,
      324,  324,  332,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  634,
      324,  324,  324,  324,  324,  547,  510,  258,  348,  648,
      348,  648,  329,  712,  894,  712,  609,  548,  724,  549,
      621,  125,  620,  610,  258,  649,  347,  634,  126,  127,
      893,  324,  324,  987,  547,  713,  258,  622,  986,  690,

      348,  655,  348,  655,  609,  548,  724,  549,  621,  242,
      620,  610,  258,  689,  650,  633,  152,  656,  349,  324,
      367,  367,  367,  367,  670,  715,  622,  154,  154,  622,
      367,  367,  367,  367,  368,  368,  369,  369,  638,  370,
      154,  657,  650,  633,  368,  368,  369,  369,  370,  370,
      387,  388,  389,  390,  715,  415,  154,  154,  622,  431,
     1895,  431,  391,  707,  708,  641,  942,  388,  154,  657,
      415,  689,  371,  371,  388,  642,  391,  392,  393,  392,
      394,  725,  371,  371,  726,  148,  690,  665,  666,  667,
      668,  707,  708,  641,  150,  148,  415,  396,  704,  391,

      704,  722,  716,  642,  150,  391,  376,  376,  727,  725,
      723,  416,  726,  391,  729,  936,  730,  731,  736,  376,
      705,  728,  732,  735,  415,  737,  738,  935,  739,  722,
      716,  733,  742,  733,  706,  376,  376,  727,  723,  416,
      743,  744,  391,  729,  730,  731,  736,  376,  694,  728,
      694,  732,  735,  734,  737,  738,  739,  740,  745,  740,
      746,  742,  706,  747, 1050,  748, 1050,  749,  743,  744,
      695,  750,  753,  933,  751,  696,  932,  754,  755,  741,
      697,  758,  752,  758,  762,  763,  745,  767,  756,  746,
      698,  699,  747,  748,  700,  749,  757,  764,  765,  766,

      750,  753,  751,  759,  696,  754,  768,  755,  769,  697,
      752,  770,  762,  763,  771,  767,  760,  756,  698,  699,
      772,  773,  700,  774,  757,  761,  764,  765,  766,  775,
      776,  491,  778,  779,  768,  780,  769,  781,  782,  770,
      783,  230,  230,  771,  760,  230,  787,  646,  772,  773,
      788,  774,  683,  761,  790,  791,  793,  775,  776,  491,
      778,  779,  242,  780,  792,  781,  782,  646,  783,  789,
      784,  795,  245,  795,  127,  787,  785,  728,  257,  788,
      597,  683,  257,  790,  791,  793,  257,  797,  786,  797,
      255,  794,  797,  792,  798,  255,  257,  789,  799,  784,

      799,  512,  257,  796,  785,  728,  257,  509,  510,  511,
      512,  799,  805,  800,  512,  257,  786,  804,  257,  513,
      794,  257,  257,  803,  257,  808,  257,  257,  257,  415,
      257,  257,  796,  513,  257,  257,  257,  807,  257,  806,
      805,  257,  510,  510,  891,  821,  804,  257,  809,  257,
      819,  820,  803,  808,  257,  810,  815,  257,  811,  257,
      257,  813,  513,  812,  814,  822,  807,  806,  817,  818,
      530,  823,  891,  816,  824,  253,  809,  257,  257,  819,
      820,  834,  257,  810,  253,  815,  811,  835,  830,  813,
      836,  812,  814,  257,  822,  257,  817,  818,  530,  823,

      257,  816,  843,  824,  694,  257,  694,  701,  257,  701,
      834,  842,  704,  257,  704,  839,  835,  830,  836,  712,
      257,  712,  257,  832,  833,  838,  695,  257,  837,  702,
      843,  825,  257,  251,  705,  257,  826,  257,  844,  842,
      257,  713,  257,  839,  917,  257,  827,  828,  831,  230,
      829,  832,  833,  257,  838,  257,  837,  777,  257,  845,
      825,  840,  847,  257,  849,  826,  844,  257,  850,  257,
      841,  257,  846,  917,  827,  828,  831,  257,  829,  857,
      852,  848,  733,  257,  733,  854,  851,  257,  845,  840,
      856,  847,  849,  853,  257,  257,  855,  850,  841,  257,

      846,  740,  257,  740,  734,  257,  257,  857,  852,  848,
      257,  257,  862,  859,  854,  851,  858,  861,  860,  856,
      257,  257,  853,  741,  855,  257,  257,  257,  257,  257,
      257,  863,  257,  869,  257,  865,  864,  257,  257,  867,
      862,  859,  257,  866,  858,  868,  861,  860,  758,  257,
      758,  876,  870,  257,  257,  877,  883,  878,  919,  863,
      871,  879,  869,  865,  257,  864,  325,  880,  867,  398,
      759,  866,  874,  868,  882,  875,  881,  623,  884,  885,
      876,  870,  915,  872,  877,  883,  878,  919,  871,  879,
      398,  670,  873,  886,  916,  880,  888,  325,  887, 1052,

      874, 1052,  882,  875,  881,  623,  884,  885,  918,  897,
      915,  872,  889,  221,  889,  320,  889,  221,  890,  320,
      873,  886,  916,  605,  888,  895,  887,  317,  318,  319,
      320,  896,  605,  605,  605,  895,  918,  897,  329,  256,
      894,  893,  325,  388,  257,  934,  388,  334,  648,  943,
      648,  893,  599,  256,  152,  257,  898,  659,  334,  896,
      903,  599,  599,  334,  649,  154,  154,  911,  654,  944,
      902,  899,  257,  934,  909,  334,  334,  943,  154,  912,
      257,  599,  256,  598,  221,  598,  222,  334,  903,  904,
      599,  599,  334,  653,  154,  154,  911,  944,  902,  914,

      899,  907,  909,  334,  334,  652,  154,  913,  912,  223,
      224,  334,  599,  225,  334,  226,  361,  904,  908,  905,
      846,  910,  906,  630,  363,  945,  484,  119,  914,  651,
      907,  937,  334,  937,  390,  329,  913,  329,  223,  224,
      334,  599,  225,  334,  226,  119,  908,  905,  846,  910,
      906,  921,  639,  945,  484,  324,  324,  332,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  125,  324,  324,  324,  324,  324,
      921,  126,  127,  920,  635,  125,  635, 1935,  655, 1935,
      655,  900,  126,  127,  946,  947,  367,  636,  367,  636,

     1935,  948, 1935,  949,  656,  125,  324,  324,  127,  631,
      368,  920,  126,  127,  639,  367,  122,  367,  937,  900,
      938,  390,  119,  946,  947, 1060,  922, 1060,  255,  368,
      948,  949,  923,  638,  324,  324,  324,  332,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  922,  324,  324,  324,  324,  324,
      924,  923,  924,  367,  901,  367,  367,  371,  367,  950,
      325,  925,  648, 1935,  648, 1935,  926,  368,  927,  369,
      368,  370,  369,  655,  370,  655,  324,  324,  649,  939,
      393,  939,  668,  901,  931,  951,  325,  148,  950,  656,

     1145,  325, 1145,  952,  953,  930,  150,  955,  665,  666,
      667,  668,  928,  954,  324,  371,  148,  956,  371,  957,
      391,  958,  959,  931,  951,  150,  961,  939,  393,  940,
      668,  952,  953,  930,  391,  148,  955,  392,  393,  392,
      394,  954,  960,  970,  150,  148,  956,  957,  971,  958,
      959,  962,  972,  962,  150,  961,  973,  974,  701,  975,
      701,  976,  977,  391,  978,  984,  704,  981,  704,  597,
      960,  970,  979,  712,  979,  712,  982,  971,  982,  985,
      702,  972,  253,  963,  973,  990,  974,  975,  705,  976,
      977,  991,  978,  984,  980,  713,  981,  964,  983,  992,

      988,  719,  988,  993,  994,  997,  999,  985,  998,  995,
      720,  721,  963,  989,  990,  989,  996, 1000, 1001,  991,
     1001,  733,  989,  733, 1004,  964,  694,  992,  694, 1010,
     1003,  993, 1013,  994,  997,  999,  998, 1011,  995, 1005,
     1002, 1005, 1014,  734,  996, 1007, 1000, 1007,  695,  740,
     1012,  740, 1004,  965, 1015,  253, 1016, 1010,  966, 1003,
     1013, 1006, 1017, 1018, 1019, 1021, 1011, 1008,  967,  968,
     1014,  741,  969, 1020, 1448,  504, 1448, 1030,  989, 1012,
     1009, 1026,  965, 1015, 1016, 1027, 1028,  966, 1029, 1031,
     1017, 1018, 1032, 1019, 1021, 1033,  967,  968, 1034, 1037,

      969, 1020,  758, 1024,  758, 1024, 1030, 1038, 1009, 1026,
     1035, 1039, 1035, 1027, 1028, 1040, 1029, 1041, 1031, 1042,
     1032, 1043, 1044, 1033,  759, 1025, 1045, 1034, 1037,  230,
      230,  230, 1036, 1049, 1053, 1051, 1038,  242,  257,  257,
     1039,  251,  245,  955, 1040, 1041,  995, 1452, 1042, 1452,
     1043, 1044,  245,  996,  257, 1045,  242, 1054, 1048,  257,
     1055, 1049, 1055, 1053, 1051, 1069,  795,  245,  795, 1046,
     1056, 1047,  955,  257, 1057,  995, 1065, 1058, 1059, 1057,
     1060,  996, 1061,  255,  257, 1054, 1062, 1048, 1062,  512,
     1062,  257, 1063,  512, 1069, 1064,  257, 1046,  796, 1047,

     1066,  257, 1068,  257, 1065, 1058, 1059,  257,  257, 1067,
      257,  257,  257,  257,  257,  257,  230,  257,  257, 1071,
      257,  257,  257, 1064,  257,  257,  125,  796, 1066,  230,
     1072, 1068, 1070, 1168, 1169, 1074,  257,  257, 1067, 1077,
      257, 1076,  364,  257, 1073, 1078, 1086, 1082, 1071, 1081,
     1080, 1075, 1087,  962,  257,  962, 1079,  257, 1085, 1072,
     1070,  257, 1093, 1074, 1088, 1091, 1089,  257, 1077,  257,
     1076,  257, 1073, 1078,  257, 1086, 1082, 1081, 1080, 1075,
     1098, 1087, 1092,  257, 1079, 1083, 1085, 1090, 1096,  257,
     1093, 1094, 1088, 1091,  257, 1089,  979,  257,  979, 1084,

      982,  257,  982,  257,  257, 1104,  257,  257, 1098, 1095,
     1092, 1097, 1101,  257, 1083, 1090, 1099, 1096,  980, 1102,
     1094,  257,  983, 1108, 1452, 1100, 1452, 1084,  257, 1107,
     1106,  257,  257, 1104, 1103, 1105,  257, 1095,  257, 1097,
     1112, 1101,  257,  257, 1099,  257,  257, 1102, 1001,  257,
     1001, 1108, 1111,  257, 1100, 1005,  257, 1005, 1107, 1106,
     1007,  257, 1007, 1103, 1105, 1113, 1114,  257, 1110, 1112,
     1002, 1126, 1115,  257,  257,  257, 1119, 1006, 1116,  257,
     1117, 1111, 1008, 1120, 1118, 1024,  257, 1024,  257,  257,
      257,  257,  257, 1113, 1114, 1109, 1110, 1136, 1121,  122,

     1126, 1115, 1035,  257, 1035, 1119, 1116, 1025, 1117, 1123,
     1159, 1120, 1118, 1122, 1124, 1125, 1127, 1130,  325, 1131,
      257,  325,  257, 1109, 1036, 1136, 1129, 1121,  324,  355,
      324, 1128, 1134,  221, 1134,  320,  325, 1123,  329, 1159,
     1139, 1122, 1124, 1125,  119, 1127, 1130, 1132, 1131, 1133,
     1134,  221, 1135,  320, 1129,  605, 1138, 1140, 1144, 1128,
      251, 1158, 1141, 1142,  324,  605,  324, 1140,  245, 1512,
     1146, 1512,  324, 1138,  324,  329, 1132, 1139, 1133,  598,
      221, 1137,  222, 1138,  599,  367, 1144,  367, 1147, 1158,
     1141,  152, 1142, 1143,  599,  257, 1161, 1138, 1146,  368,

     1148,  242,  154,  154, 1187,  223,  224, 1188,  599,  225,
      324,  226,  324,  599,  324,  154,  324, 1147, 1156,  924,
     1935,  924,  484,  599,   73, 1161,  324,   70,  324, 1148,
      925,  154,  154, 1187,  223,  224, 1188,  599,  225, 1181,
      226, 1181,  390,  154, 1448,  257, 1448, 1156, 1149, 1150,
      484,  324,  324,  332,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
     1151,  324,  324,  324,  324,  324, 1149, 1150,  924, 1935,
      924, 1055,  257, 1055, 1181, 1192, 1182,  390, 1185,  925,
     1186, 1056, 1189, 1935, 1194, 1057, 1191,  324, 1151,  324,

     1057, 1935,  324,  324, 1935, 1183,  393, 1183,  668, 1183,
      393, 1184,  668,  148, 1192, 1935, 1185,  148, 1186, 1935,
     1189, 1152,  150, 1194, 1191, 1276,  150, 1276,  255, 1153,
      324,  324, 1145,  332, 1145,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
     1152,  324,  324,  324,  324,  324,  119,  324, 1153,  324,
      324,  257,  324,  924,  125,  924, 1935, 1193, 1195, 1935,
     1196,  126,  127, 1935,  925, 1198,  924, 1197,  924,  926,
     1163, 1170,  324,  324,  367, 1154,  367,  925, 1155, 1201,
     1101, 1166,  926, 1199,  927, 1193, 1195, 1102,  368, 1196,

      369, 1157,  370, 1198, 1200, 1197, 1202, 1207, 1935, 1163,
      324, 1205, 1173, 1935, 1154,  928, 1210, 1155, 1201, 1101,
     1166, 1199, 1935,  125, 1276, 1102, 1277,  255,  928, 1157,
     1361, 1362, 1200, 1935, 1202, 1207,  371,  962, 1206,  962,
     1205, 1173, 1160, 1174, 1210, 1174, 1160, 1160, 1160, 1160,
     1160, 1160, 1160, 1160, 1160, 1160, 1160, 1175, 1160, 1176,
     1160, 1177, 1160, 1160, 1160, 1160, 1160, 1206, 1208, 1203,
      665,  666,  667,  668, 1209, 1211, 1935, 1212,  148, 1213,
     1214, 1215,  391, 1204, 1216, 1220, 1217,  150, 1217, 1219,
      979, 1229,  979, 1160, 1160, 1179,  391, 1208, 1203, 1230,

      982, 1935,  982, 1209, 1211, 1212, 1935, 1213, 1218, 1214,
     1215, 1204,  980, 1216, 1220, 1935, 1231, 1219, 1935, 1935,
     1229, 1160,  983, 1935, 1935,  391, 1160, 1160, 1230, 1160,
     1160, 1160, 1160, 1160, 1160, 1160, 1160, 1160, 1160, 1160,
     1160, 1160, 1160, 1160, 1160, 1231, 1160, 1160, 1160, 1160,
     1160, 1221, 1223, 1221, 1223, 1232, 1935, 1278, 1233, 1278,
      512, 1234,  986,  987, 1222, 1224, 1222, 1224, 1235,  988,
      719,  988, 1236, 1222, 1224, 1239,  719, 1160, 1160,  720,
      721, 1240,  989, 1232,  989, 1226, 1227, 1233, 1228, 1234,
     1228,  989, 1237, 1001, 1237, 1001, 1235, 1228, 1241, 1242,

     1005, 1236, 1005, 1239, 1007, 1160, 1007, 1243, 1244, 1245,
     1240, 1246, 1247, 1248, 1238, 1002, 1249, 1935, 1250, 1251,
     1252, 1022, 1006, 1022, 1253, 1935, 1008, 1241, 1242, 1222,
     1224, 1023, 1024, 1254, 1024, 1255, 1243, 1244, 1245, 1246,
     1256, 1247, 1248, 1257, 1258, 1249, 1250,  989, 1251, 1252,
     1259, 1260, 1253, 1228, 1025, 1261, 1264, 1262, 1035, 1262,
     1035, 1254, 1265, 1255, 1266,  230, 1272, 1263,  230, 1256,
     1270, 1257, 1271, 1258, 1055, 1273, 1055,  245, 1259, 1260,
     1036,  257,  242, 1261, 1056, 1264,  257, 1278, 1057, 1279,
      512, 1265, 1266, 1057, 1272, 1268, 1267,  257, 1055, 1270,

     1055, 1271,  257,  257, 1273,  257, 1340, 1057, 1056, 1057,
      257, 1287, 1057, 1274, 1217,  257, 1217, 1057, 1379, 1275,
      334, 1057, 1280,  257, 1268, 1267, 1057,  257, 1283, 1282,
     1237,  257, 1237, 1281, 1340, 1935, 1218, 1289, 1935, 1284,
     1287, 1935, 1274, 1262,  257, 1262, 1379, 1275,  125,  334,
     1280, 1935, 1238, 1263,  257,  126,  127, 1283, 1282, 1288,
      257, 1281, 1935,  257,  257, 1286, 1289, 1284, 1160, 1160,
      257, 1160, 1160, 1160, 1160, 1160, 1160, 1160, 1160, 1160,
     1160, 1160, 1160, 1160, 1160, 1160, 1160, 1288, 1160, 1160,
     1160, 1160, 1160, 1286, 1290,  257,  257, 1293, 1294,  257,

      257,  257,  257,  257, 1295, 1725,  257,  257,  257,  257,
      257,  257, 1726,  257,  257, 1727,  257,  257,  257, 1160,
     1160, 1935, 1290, 1296, 1291, 1293, 1294,  257, 1298, 1303,
      257, 1292, 1295, 1299,  257, 1297, 1300, 1302,  257, 1301,
     1304, 1307,  257, 1305, 1306, 1308, 1314, 1160, 1310,  257,
     1313, 1312, 1296, 1291, 1311,  257, 1309, 1298, 1303, 1292,
      257, 1317, 1299, 1297, 1300, 1316, 1302, 1301,  257, 1304,
     1307, 1305,  257, 1306, 1308, 1314, 1310, 1320, 1313, 1312,
     1315,  257,  257, 1311,  257, 1309, 1318,  257,  257,  257,
     1317, 1319,  257,  257, 1316,  257,  257,  257, 1321,  257,

     1380,  257,  257, 1935, 1322,  257, 1320,  334, 1315, 1323,
     1935, 1324, 1325, 1328, 1935, 1318, 1327, 1935, 1346,  334,
     1319, 1347, 1332,  334, 1355,  325, 1334, 1321, 1380, 1326,
     1329, 1331, 1322,  325, 1330,  367,  334,  367, 1323, 1333,
     1324, 1325, 1328, 1335, 1336, 1327, 1346, 1337,  334,  368,
     1347, 1332,  334, 1355, 1935, 1334, 1343, 1326, 1329, 1331,
     1341, 1935, 1330, 1338,  221, 1338,  320, 1333,  605, 1935,
      605, 1335, 1336,  257, 1935, 1337, 1338,  221, 1339,  320,
      598,  221,  598,  222,  605, 1343,  605,  257,  334, 1341,
      334,  125, 1370, 1381, 1370,  390, 1349,  599,  126,  127,

      329, 1354,  329, 1348,  334, 1935,  223,  224, 1350,  599,
      225, 1935,  226,  599, 1935, 1351,  152,  334,  153,  334,
     1935, 1353, 1381,  484, 1382, 1349,  599,  154,  154, 1935,
     1354, 1348,  635,  334,  635,  223,  224, 1350,  599,  225,
      154,  226,  599, 1351,  924,  636,  924,  636,  924, 1353,
      924,  484, 1382, 1935, 1935,  925,  154,  154, 1370,  925,
     1371,  390, 1170, 1935, 1935, 1387, 1171, 1935,  154,  324,
      324,  332,  324,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324, 1935,  324,
      324,  324,  324,  324, 1387,  367,  928,  367, 1344, 1389,

     1372,  393, 1372,  668, 1374, 1383, 1374, 1384,  148,  368,
     1394,  369, 1395,  638, 1385, 1386,  367,  150,  367, 1935,
      324,  324,  367, 1376,  367, 1376, 1375, 1344, 1389, 1512,
      368, 1512,  369, 1383,  370, 1384,  368, 1935,  369, 1394,
      370, 1395, 1385, 1386, 1448, 1377, 1448,  371,  324,  324,
      324,  332,  324,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  371,  324,
      324,  324,  324,  324,  371, 1388, 1345, 1372,  393, 1373,
      668, 1393, 1396, 1935, 1397,  148, 1398, 1399, 1400, 1401,
      125, 1402, 1403, 1404,  150, 1404, 1406,  126,  127, 1407,

      324,  324, 1935, 1388, 1217, 1345, 1217, 1935,  635, 1393,
      635, 1396, 1397, 1935, 1398, 1405, 1399, 1400, 1401, 1402,
     1403,  636, 1390,  636, 1390, 1406, 1218, 1407,  324, 1342,
     1342,  332, 1342, 1342, 1342, 1342, 1342, 1342, 1342, 1342,
     1342, 1342, 1342, 1342, 1342, 1342, 1342, 1342, 1391, 1342,
     1342, 1342, 1342, 1342, 1414, 1262, 1221, 1262, 1221, 1935,
     1935, 1392, 1411, 1412, 1413, 1263, 1935,  986,  986, 1222,
     1222, 1222, 1222, 1223, 1237, 1223, 1237, 1391, 1222, 1222,
     1342, 1342, 1414, 1408,  987, 1409, 1224, 1409, 1224, 1392,
      719, 1411, 1412, 1413, 1409, 1224, 1238, 1935, 1935, 1226,

     1227, 1935, 1228, 1454, 1228, 1454,  512, 1427, 1342, 1356,
     1364, 1228, 1364, 1356, 1356, 1356, 1356, 1356, 1356, 1356,
     1356, 1356, 1356, 1356, 1365, 1356, 1366, 1356, 1367, 1356,
     1356, 1356, 1356, 1356, 1222, 1222, 1427, 1415, 1416, 1935,
     1935, 1419, 1410,  719, 1410, 1422, 1417, 1420, 1417, 1420,
     1409, 1224, 1226, 1227, 1423, 1228, 1424, 1228, 1935, 1428,
     1356, 1356, 1369, 1429, 1228, 1415, 1416, 1228, 1418, 1419,
     1935, 1935, 1454, 1422, 1455,  512, 1498,  221, 1498,  222,
     1430, 1448, 1423, 1448, 1424, 1935, 1421, 1428, 1356, 1356,
     1356, 1429, 1356, 1356, 1356, 1356, 1356, 1356, 1356, 1356,

     1356, 1356, 1356, 1356, 1356, 1356, 1356, 1356, 1430, 1356,
     1356, 1356, 1356, 1356, 1421, 1425, 1431, 1425, 1432, 1433,
     1228, 1434, 1436, 1434, 1436, 1440, 1441, 1438, 1442, 1438,
     1443,  245,  230, 1446,  257, 1447, 1449, 1426,  242,  257,
     1356, 1356, 1935, 1935, 1437, 1431, 1432, 1435, 1433, 1439,
     1374,  257, 1374, 1440, 1505, 1441, 1442, 1935, 1451, 1443,
     1444, 1471, 1446, 1447, 1465, 1449, 1450,  125, 1356, 1376,
      257, 1376, 1375,  257,  126,  127, 1435, 1565,  257, 1565,
      255,  257, 1505,  257,  257,  257,  257, 1451,  257, 1444,
     1471, 1377,  257, 1465, 1935, 1450, 1356, 1356,  257, 1356,

     1356, 1356, 1356, 1356, 1356, 1356, 1356, 1356, 1356, 1356,
     1356, 1356, 1356, 1356, 1356, 1457, 1356, 1356, 1356, 1356,
     1356, 1458,  257, 1460, 1461, 1462, 1463, 1464,  257,  257,
     1466, 1390,  257, 1390, 1470,  257,  257,  257, 1404,  257,
     1404,  257,  257, 1457,  257, 1459,  257, 1356, 1356, 1458,
      257, 1460, 1461, 1462, 1463, 1464, 1472, 1468, 1466, 1467,
     1405,  257, 1470, 1473, 1476,  257,  257, 1935,  257, 1475,
     1469, 1479,  257, 1480, 1459, 1356,  257, 1474,  257, 1477,
      257, 1478, 1417,  257, 1417, 1472, 1468,  257, 1467, 1420,
      257, 1420, 1473, 1476, 1425,  257, 1425, 1475, 1469,  257,

     1479, 1481, 1480, 1489, 1418, 1474, 1482, 1477, 1483, 1486,
     1478,  257, 1484, 1935, 1487,  257, 1426,  257, 1935, 1935,
     1500,  257,  257, 1488, 1935, 1506,  325, 1491, 1485, 1481,
     1935, 1566, 1489, 1566, 1482, 1507, 1483, 1486, 1935, 1935,
     1484, 1490, 1487, 1518, 1493, 1434,  257, 1434, 1492, 1500,
     1494, 1488, 1495, 1506, 1502, 1491, 1485, 1436,  257, 1436,
     1438,  257, 1438, 1507, 1497, 1498,  221, 1498, 1499, 1490,
     1508, 1496, 1518, 1493,  257,  924, 1492,  924, 1494, 1437,
     1935, 1495, 1439, 1502, 1935, 1566,  925, 1566, 1598,  221,
     1598,  320, 1497, 1609, 1935, 1609,  390, 1510, 1508, 1509,

     1496, 1501, 1501,  332, 1501, 1501, 1501, 1501, 1501, 1501,
     1501, 1501, 1501, 1501, 1501, 1501, 1501, 1501, 1501, 1501,
     1511, 1501, 1501, 1501, 1501, 1501, 1510, 1509,  367, 1374,
      367, 1374, 1514,  393, 1514,  668, 1376, 1519, 1376, 1522,
      148, 1404,  368, 1404, 1935, 1873, 1516, 1873, 1511,  150,
     1517, 1375, 1501, 1501,  367,  367,  367,  367, 1377, 1935,
      367, 1935,  367, 1405, 1935, 1935, 1519, 1522,  368,  368,
      369,  369,  638,  370,  368, 1516,  369, 1935,  370, 1517,
     1501,  324,  324,  332,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,

     1520,  324,  324,  324,  324,  324,  371,  371, 1521, 1523,
     1524, 1530,  371, 1514,  393, 1515,  668, 1525, 1528, 1529,
     1531,  148, 1532, 1533, 1935, 1504, 1534, 1935, 1536, 1520,
      150, 1535,  324,  324, 1607, 1935, 1607, 1521, 1523, 1524,
     1530, 1675, 1935, 1675, 1608, 1525, 1528, 1529, 1935, 1531,
     1532, 1676, 1533, 1504, 1390, 1534, 1390, 1536, 1537, 1535,
      324,  324,  324,  332,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
     1526,  324,  324,  324,  324,  324, 1538, 1537, 1539, 1540,
     1542, 1543, 1541, 1527, 1541, 1408, 1544, 1409, 1546, 1409,

     1420, 1547, 1420, 1408, 1935, 1409, 1409, 1409, 1417, 1526,
     1417, 1935,  324,  324, 1409, 1538, 1539, 1540, 1542, 1550,
     1543, 1527, 1410,  719, 1410, 1544, 1546, 1552, 1551, 1547,
     1418, 1553, 1226, 1227, 1548, 1228, 1548, 1228, 1557, 1545,
      324, 1425, 1555, 1425, 1228,  245, 1434, 1550, 1434, 1556,
     1558, 1436, 1438, 1436, 1438, 1552, 1549, 1551, 1561, 1553,
     1562,  257, 1409, 1426,  257,  257, 1557, 1545,  257,  257,
     1409, 1555, 1554, 1437, 1439,  257,  257,  257, 1556, 1558,
      257,  257,  257,  257,  257, 1564, 1561, 1568, 1570, 1562,
     1571, 1569,  257,  257,  257, 1572, 1573,  257,  257,  257,

     1228, 1554,  257, 1575, 1576,  257, 1580, 1583,  257,  257,
      257, 1935, 1574, 1564, 1577,  257, 1568, 1570, 1582, 1571,
     1569, 1578, 1579,  257, 1572, 1573, 1584,  257, 1586, 1588,
     1935, 1585, 1575, 1576, 1581, 1580, 1583, 1593, 1589,  257,
     1574,  257, 1577, 1599, 1587, 1590, 1597, 1582, 1601, 1578,
     1579, 1591, 1548,  257, 1548, 1584, 1586, 1592, 1588, 1585,
     1602, 1605, 1581, 1594, 1596, 1935, 1593, 1589, 1603, 1604,
      257, 1599, 1587, 1590, 1549, 1597, 1615, 1601, 1617, 1591,
     1616, 1618, 1619, 1595, 1620, 1592, 1610,  147, 1610, 1602,
     1605, 1594, 1626, 1596,  148, 1606, 1603, 1604, 1611,  393,

     1611,  394, 1621,  150, 1622, 1615,  148, 1617, 1616, 1618,
     1619, 1595, 1620, 1623, 1624,  150, 1625, 1611,  393, 1611,
     1612, 1626, 1627, 1628, 1606, 1613, 1629, 1630, 1631, 1935,
     1621, 1632, 1622, 1633, 1614, 1634, 1935, 1935, 1637, 1638,
     1935, 1623, 1624, 1635, 1625, 1635, 1548, 1541, 1548, 1541,
     1627, 1628, 1639, 1642, 1643, 1629, 1630, 1631, 1408, 1632,
     1409, 1633, 1409, 1644, 1634, 1636, 1637, 1638, 1549, 1409,
     1640, 1645, 1640, 1646, 1647, 1649,  230, 1650,  242,  245,
     1639,  257, 1642, 1643, 1565,  257, 1565,  255,  257,  257,
      257, 1644, 1641, 1653,  257, 1653,  512,  257,  257, 1645,

      257, 1646, 1648, 1647, 1649, 1651, 1650, 1652,  257, 1655,
      257,  257,  257,  257,  257, 1657, 1935, 1656,  257, 1635,
      257, 1635,  257,  257, 1678, 1409, 1659, 1664,  257, 1658,
      257, 1648, 1935, 1663, 1651, 1679, 1652, 1660, 1655, 1661,
     1666, 1636, 1665, 1669, 1657, 1656, 1662,  325, 1668, 1680,
     1689, 1670, 1678, 1667, 1659, 1672, 1664, 1658, 1640,  257,
     1640, 1663, 1671, 1679, 1673, 1660, 1674, 1661,  257, 1666,
     1665, 1935, 1669, 1677, 1662, 1693, 1668, 1680, 1689, 1670,
     1641, 1667, 1935, 1935, 1672, 1598,  221, 1598,  320, 1607,
     1671, 1607, 1673, 1690, 1674, 1681, 1935, 1691, 1609, 1608,

     1609,  390, 1677, 1610, 1693, 1610, 1685,  393, 1685,  668,
     1686,  393, 1686, 1692,  148, 1694, 1696, 1695, 1687, 1697,
      150, 1690, 1698,  150, 1681, 1691, 1701, 1688,  392,  393,
      392,  394, 1610,  147, 1610, 1702,  148, 1699, 1703, 1699,
      148, 1692, 1704, 1694, 1696,  150, 1695, 1700, 1697,  150,
     1635, 1698, 1635, 1640, 1701, 1640, 1705, 1706, 1707, 1708,
      230, 1710,  242,  257, 1702,  257, 1703,  245,  257,  257,
      257, 1704, 1636,  257, 1653, 1641, 1653,  512,  257,  325,
     1729, 1699,  257, 1699, 1705,  257, 1706, 1707, 1708, 1710,
     1712, 1700,  257,  257,  257, 1730, 1740, 1709, 1711, 1713,

     1717, 1737, 1741, 1719, 1675, 1714, 1675, 1718, 1729, 1742,
     1715, 1716, 1722, 1743, 1676, 1771, 1728, 1720, 1721, 1712,
     1744, 1723, 1935, 1730, 1740, 1709, 1711, 1713, 1935, 1717,
     1737, 1741, 1719, 1714, 1935, 1718, 1935, 1742, 1715, 1716,
     1935, 1722, 1743, 1771, 1728, 1745, 1720, 1721, 1935, 1744,
     1723, 1731, 1732, 1731, 1733, 1685,  393, 1685,  668, 1683,
     1746,  257, 1684,  148, 1686,  670, 1686, 1734, 1735, 1734,
     1736, 1747,  150,  242, 1745, 1687, 1738, 1699, 1738, 1699,
     1699, 1688, 1699,  245, 1688,  257, 1739, 1700, 1752, 1746,
     1700, 1738,  257, 1738,  257,  257,  257, 1725, 1772, 1750,

     1747, 1739, 1935, 1800, 1726, 1800, 1725, 1727, 1751, 1757,
     1758, 1759, 1760, 1726, 1801, 1935, 1727, 1752, 1774, 1753,
     1773, 1761, 1755, 1756, 1738, 1754, 1738, 1772, 1750, 1732,
     1762, 1763, 1762, 1764, 1739, 1761, 1767, 1751, 1726, 1768,
     1935, 1727, 1731, 1732, 1731, 1733, 1774, 1753,  230, 1773,
     1683, 1755, 1756, 1684, 1754, 1731, 1732, 1731, 1733, 1734,
     1735, 1734, 1736, 1683, 1761, 1775, 1684, 1687, 1776,  665,
      666,  667,  668, 1778,  257,  245, 1688,  148, 1769,  257,
     1769,  391,  257, 1935, 1777, 1784,  150, 1784, 1760, 1935,
      325, 1770, 1802, 1770, 1775,  391, 1784, 1776, 1785, 1760,

     1770, 1778, 1780, 1763, 1732, 1803, 1781, 1805, 1783, 1935,
     1794, 1767, 1777, 1795, 1768, 1782, 1757, 1758, 1759, 1760,
     1732, 1802,  230,  257,  391, 1935, 1796, 1767, 1761, 1935,
     1768, 1780, 1804, 1803, 1806, 1781, 1805, 1783, 1762, 1763,
     1762, 1764, 1761, 1782, 1935, 1807, 1726,  257,  242, 1727,
     1788, 1789, 1790, 1791, 1796, 1769, 1809, 1769, 1726, 1808,
     1804, 1727, 1792, 1806, 1797, 1798, 1797, 1799, 1770, 1813,
     1770, 1761, 1767, 1807, 1810, 1768, 1792, 1770, 1800,  257,
     1800, 1935, 1832, 1816, 1809, 1816, 1760, 1808, 1935, 1801,
     1815, 1770, 1816, 1770, 1817, 1760, 1831, 1813, 1935,  325,

     1770, 1833, 1935, 1810, 1935, 1792, 1818, 1763, 1818, 1791,
     1814, 1832, 1935, 1935, 1726, 1935, 1789, 1727, 1815, 1788,
     1789, 1790, 1791, 1821, 1831,  242, 1822, 1726, 1935, 1833,
     1727, 1792, 1818, 1763, 1819, 1791, 1830, 1824, 1935, 1814,
     1726, 1763,  230, 1727, 1825, 1792,  257, 1826, 1794, 1763,
     1834, 1795, 1827, 1828, 1827, 1829, 1794,  257, 1835, 1795,
     1794, 1837, 1838, 1795, 1830, 1797, 1798, 1797, 1799, 1731,
     1732, 1731, 1733, 1767, 1792, 1732, 1768, 1683, 1834, 1935,
     1684, 1935, 1767, 1839, 1836, 1768, 1835, 1851, 1935, 1837,
     1838, 1841, 1935, 1841, 1760, 1935, 1841, 1840, 1842, 1760,

     1935, 1852, 1843, 1763, 1843, 1791, 1843, 1763, 1844, 1791,
     1726, 1839, 1836, 1727, 1726, 1851, 1789, 1727, 1935, 1845,
     1846, 1845, 1847, 1821, 1789, 1840, 1822, 1821, 1824, 1852,
     1822, 1821,  325,  257, 1822, 1825, 1935, 1935, 1826, 1757,
     1758, 1759, 1760, 1824, 1848, 1789, 1848, 1849,  257, 1853,
     1825, 1761, 1825, 1826, 1854, 1826, 1827, 1828, 1827, 1829,
     1788, 1789, 1790, 1791, 1794, 1761, 1763, 1795, 1726, 1858,
     1869, 1727, 1792, 1794, 1850, 1859, 1795, 1853, 1860, 1935,
     1860, 1760, 1860, 1854, 1861, 1760, 1792, 1862, 1763, 1862,
     1791, 1868, 1935, 1789, 1761, 1726,  230, 1858, 1727, 1869,

     1821, 1935, 1850, 1822, 1859, 1862, 1763, 1863, 1791, 1845,
     1846, 1845, 1847, 1726,  242, 1792, 1727, 1821, 1789, 1868,
     1822, 1788, 1789, 1790, 1791, 1865, 1871,  325, 1866, 1726,
      230, 1935, 1727, 1792, 1848, 1789, 1848, 1849, 1870, 1789,
     1935, 1789, 1825, 1935, 1880, 1826, 1865, 1792, 1865, 1866,
     1872, 1866, 1935, 1935, 1871,  242, 1874, 1763, 1874, 1791,
     1874, 1763, 1875, 1791, 1726, 1935, 1870, 1727, 1726, 1878,
     1879, 1727, 1880,  325, 1935, 1935, 1792,  230, 1872, 1935,
     1873, 1935, 1873, 1882, 1935, 1882, 1760, 1935, 1889, 1935,
     1935, 1935, 1881, 1894, 1935, 1935,  230, 1878, 1879, 1935,

     1893, 1935, 1935, 1935, 1876, 1846, 1876, 1877, 1883, 1763,
     1883, 1764, 1865, 1887, 1789, 1866, 1726, 1889, 1935, 1727,
     1881, 1865, 1894, 1888, 1866, 1876, 1846, 1876, 1877, 1893,
     1935, 1935, 1935, 1865,  242, 1882, 1866, 1882, 1760, 1763,
     1935, 1887, 1891, 1763, 1891, 1791, 1794, 1725, 1935, 1795,
     1726,  325, 1888, 1727, 1726,  230,  242, 1727, 1935, 1724,
     1883, 1763, 1883, 1884, 1724, 1898,  325, 1724, 1886, 1724,
     1724, 1727, 1935, 1935, 1724, 1724, 1890,  325, 1892, 1724,
     1935, 1724, 1724, 1724, 1762, 1763, 1762, 1764, 1891, 1763,
     1891, 1791, 1726, 1898,  242, 1727, 1726, 1897, 1899, 1727,

     1896,  242, 1935, 1935, 1890, 1935, 1935, 1892, 1900, 1935,
     1724, 1724, 1724, 1901, 1902, 1901, 1903, 1904, 1905, 1904,
     1906, 1908, 1909, 1908, 1910, 1897, 1899, 1935, 1919, 1896,
     1901, 1902, 1901, 1903, 1907, 1935, 1900, 1935, 1724, 1911,
     1912, 1911, 1913, 1904, 1905, 1904, 1906, 1916, 1905, 1916,
     1906, 1908, 1909, 1908, 1910,  242, 1935, 1919, 1916, 1905,
     1916, 1906, 1907, 1911, 1912, 1911, 1913, 1911, 1912, 1911,
     1913, 1911, 1912, 1911, 1913, 1916, 1905, 1916, 1906, 1916,
     1905, 1916, 1906,  242, 1916, 1905, 1916, 1906, 1925, 1926,
     1925, 1927, 1925, 1926, 1925, 1927, 1935, 1924, 1928, 1929,

     1928, 1930, 1928, 1929, 1928, 1930, 1923, 1928, 1929, 1928,
     1930, 1928, 1929, 1928, 1930, 1928, 1929, 1928, 1930, 1935,
     1935, 1935, 1935, 1935, 1935, 1924, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1923,   68,   68,   68,   68,
       68,   68,   68,   68,   68,   68,   68,   68,   68,   68,
       68,   68,   68,   68,   69,   69,   69,   69,   69,   69,
       69,   69,   69,   69,   69,   69,   69,   69,   69,   69,
       69,   69,   72,   72,   72,   72,   72,   72,   72,   72,
       72,   72,   72,   72,   72,   72,   72,   72,   72,   72,
      115,  115, 1935,  115,  115,  115,  115,  115,  115,  115,

      115,  115,  115,  115,  115,  115,  115,  115,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  133, 1935, 1935, 1935, 1935, 1935,
     1935,  133, 1935,  133, 1935,  133,  133,  133,  133,  133,
      160,  160,  160,  160,  160,  229,  229,  229,  229,  229,
      229,  229,  229,  229,  229,  229,  229,  229,  229,  229,
      229,  229,  229,  241,  241,  241,  241,  241,  241,  241,
      241,  241,  241,  241,  241,  241,  241,  241,  241,  241,

      241,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  250,
      250,  250,  250,  250,  250,  250,  250,  250,  250,  250,
      250,  250,  250,  250,  250,  250,  250,  258, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,  258,  258,
      258,  258,  258,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
      324,  115,  115, 1935,  115,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  115,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,

      118,  118,  118,  118,  118,  118,  118,  358,  358,  358,
      358,  358,  358,  358,  358,  358,  358,  358,  358,  358,
      358,  358,  358,  358,  358,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  359,  359,  359,  359,  359,  359,  359,
      359,  359,  359,  359,  359,  359,  359,  359,  359,  359,
      359,  133, 1935, 1935, 1935, 1935, 1935, 1935,  133, 1935,
      133, 1935, 1935,  133,  133,  133,  133,  395,  395,  395,
      395, 1935,  395,  395,  395,  395,  395,  395, 1935,  395,
      395, 1935, 1935,  395,  395,  160,  160,  160,  160,  160,

      485,  485,  485,  485,  485,  485,  485,  485,  485,  485,
      485,  485,  485,  485,  485,  485,  485,  485,  229,  229,
      229,  229,  229,  229,  229,  229,  229,  229,  229,  229,
      229,  229,  229,  229,  229,  229,  241,  241,  241,  241,
      241,  241,  241,  241,  241,  241,  241,  241,  241,  241,
      241,  241,  241,  241,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  250,  250,  250,  250,  250,  250,  250,  250,
      250,  250,  250,  250,  250,  250,  250,  250,  250,  250,
      507,  507,  507,  507,  507,  507,  507,  507,  507,  507,

      507,  507,  507,  507,  507,  507,  507,  507,  508,  508,
      508,  508,  508,  508,  508,  508,  508,  508,  508,  508,
      508,  508,  508,  508,  508,  508,  596,  596,  596,  596,
      596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
      596,  596,  596,  596,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  334,  334,  334,  334,  334,  334,  334,  334,
      334,  334,  334,  334,  334,  334,  334,  334,  334,  334,
      115,  115, 1935,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  118,  118,

      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  358,  358,  358,  358,
      358,  358,  358,  358,  358,  358,  358,  358,  358,  358,
      358,  358,  358,  358,  359,  359,  359,  359,  359,  359,
      359,  359,  359,  359,  359,  359,  359,  359,  359,  359,
      359,  359,  632,  632,  632,  632,  632,  632,  632,  632,
      632,  632,  632,  632,  632,  632,  632,  632,  632,  632,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  637, 1935,
     1935, 1935, 1935, 1935, 1935,  637, 1935,  637, 1935, 1935,

      637,  637,  637,  637,  133, 1935, 1935, 1935, 1935, 1935,
     1935, 1935,  133, 1935,  133, 1935,  133,  133,  133,  133,
      133,  640,  640,  640,  640,  663,  663,  663,  663,  663,
      663,  663,  663,  663,  663,  663,  663,  663,  663,  663,
      663,  663,  663,  664,  664,  664,  664,  664,  664,  664,
      664,  664,  664,  664,  664,  664,  664,  664,  664,  664,
      664,  669,  669,  669,  669,  669,  669,  669,  669,  669,
      669,  669,  669,  669,  669,  669,  669,  669,  669,  395,
      395,  395,  395, 1935,  395,  395,  395,  395,  395,  395,
     1935,  395,  395, 1935, 1935,  395,  395,  160,  160,  160,

      160,  160,  718,  718,  718,  718,  718,  718,  718,  718,
      718,  718,  718,  718,  718,  718,  718,  718,  718,  718,
      483, 1935, 1935, 1935, 1935, 1935, 1935, 1935,  483,  483,
      485,  485,  485,  485,  485,  485,  485,  485,  485,  485,
      485,  485,  485,  485,  485,  485,  485,  485,  229,  229,
      229,  229,  229,  229,  229,  229,  229,  229,  229,  229,
      229,  229,  229,  229,  229,  229,  241,  241,  241,  241,
      241,  241,  241,  241,  241,  241,  241,  241,  241,  241,
      241,  241,  241,  241,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,

      244,  244,  250,  250,  250,  250,  250,  250,  250,  250,
      250,  250,  250,  250,  250,  250,  250,  250,  250,  250,
      507,  507,  507,  507,  507,  507,  507,  507,  507,  507,
      507,  507,  507,  507,  507,  507,  507,  507,  508,  508,
      508,  508,  508,  508,  508,  508,  508,  508,  508,  508,
      508,  508,  508,  508,  508,  508,  801,  801,  801,  801,
      801,  801,  801,  801,  801,  801,  801,  801,  801,  801,
      801,  801,  801,  801,  802,  802,  802,  802,  802,  802,
      802,  802,  802,  802,  802,  802,  802,  802,  802,  802,
      802,  802,  258, 1935, 1935, 1935, 1935, 1935, 1935, 1935,

     1935, 1935, 1935,  258,  258,  258,  258,  258,  596,  596,
      596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
      596,  596,  596,  596,  596,  596,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  334,  334,  334,  334,  334,  334,
      334,  334,  334,  334,  334,  334,  334,  334,  334,  334,
      334,  334,  115,  115, 1935,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  359,  359,

      359,  359,  359,  359,  359,  359,  359,  359,  359,  359,
      359,  359,  359,  359,  359,  359,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  632,  632,  632,  632,  632,  632,
      632,  632,  632,  632,  632,  632,  632,  632,  632,  632,
      632,  632,  637, 1935, 1935, 1935, 1935, 1935, 1935,  637,
     1935,  637, 1935, 1935,  637,  637,  637,  637,  929, 1935,
     1935, 1935, 1935, 1935, 1935, 1935,  929, 1935, 1935, 1935,
      929,  929,  929,  929,  929,  133, 1935, 1935, 1935, 1935,
     1935, 1935, 1935,  133, 1935,  133, 1935,  133,  133,  133,

      133,  133,  663,  663,  663,  663,  663,  663,  663,  663,
      663,  663,  663,  663,  663,  663,  663,  663,  663,  663,
      664,  664,  664,  664,  664,  664,  664,  664,  664,  664,
      664,  664,  664,  664,  664,  664,  664,  664,  941,  941,
      941,  941,  941,  941,  941,  941,  941,  941,  941,  941,
      941,  941,  941,  941,  941,  941,  669,  669,  669,  669,
      669,  669,  669,  669,  669,  669,  669,  669,  669,  669,
      669,  669,  669,  669,  160,  160,  160,  160,  160,  718,
      718,  718,  718,  718,  718,  718,  718,  718,  718,  718,
      718,  718,  718,  718,  718,  718,  718,  719,  719,  719,

      719,  719,  719, 1935,  719,  719,  719,  719,  719,  719,
      719,  719,  719,  719,  719,  720,  720, 1935,  720,  720,
      720,  720,  720,  720,  720,  720,  720,  720,  720,  720,
      720,  720,  720,  229,  229,  229,  229,  229,  229,  229,
      229,  229,  229,  229,  229,  229,  229,  229,  229,  229,
      229,  241,  241,  241,  241,  241,  241,  241,  241,  241,
      241,  241,  241,  241,  241,  241,  241,  241,  241,  244,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  801,  801,  801,
      801,  801,  801,  801,  801,  801,  801,  801,  801,  801,

      801,  801,  801,  801,  801,  802,  802,  802,  802,  802,
      802,  802,  802,  802,  802,  802,  802,  802,  802,  802,
      802,  802,  802,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
      324,  334,  334,  334,  334,  334,  334,  334,  334,  334,
      334,  334,  334,  334,  334,  334,  334,  334,  334, 1160,
     1160, 1935, 1160, 1160, 1160, 1160, 1160, 1160, 1160, 1160,
     1160, 1160, 1160, 1160, 1160, 1160, 1160,  115,  115, 1935,
      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115, 1162, 1162, 1935, 1162, 1162,

     1162, 1162, 1162, 1162, 1162, 1162, 1162, 1162, 1162, 1162,
     1162, 1162, 1162,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118, 1164, 1164, 1164, 1164, 1164, 1164, 1164, 1164, 1164,
     1164, 1164, 1164, 1164, 1164, 1164, 1164, 1164, 1164,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124, 1167, 1167, 1167,
     1167, 1167, 1167, 1167, 1167, 1167, 1167, 1167, 1167, 1167,
     1167, 1167, 1167, 1167, 1167,  637, 1935, 1935, 1935, 1935,
     1935,  637, 1935, 1935, 1935,  637, 1935,  637,  637,  637,

      637,  637, 1172, 1172, 1172, 1172,  929, 1935, 1935, 1935,
     1935, 1935, 1935, 1935,  929, 1935, 1935, 1935,  929,  929,
      929,  929,  929,  133, 1935, 1935, 1935, 1935, 1935, 1935,
     1935,  133, 1935,  133, 1935,  133,  133,  133,  133,  133,
     1178, 1178, 1935, 1178, 1178, 1178, 1178, 1178, 1178, 1178,
     1178, 1178, 1178, 1178, 1178, 1178, 1178, 1178,  941,  941,
      941,  941,  941,  941,  941,  941,  941,  941,  941,  941,
      941,  941,  941,  941,  941,  941, 1190, 1190, 1935, 1190,
     1190, 1190, 1190, 1190, 1190, 1190, 1190, 1190, 1190, 1190,
     1190, 1190, 1190, 1190,  719,  719,  719,  719,  719,  719,

     1935,  719,  719,  719,  719,  719,  719,  719,  719,  719,
      719,  719,  720,  720, 1935,  720,  720,  720,  720,  720,
      720,  720,  720,  720,  720,  720,  720,  720,  720,  720,
      718,  718,  718,  718,  718,  718,  718,  718,  718,  718,
      718,  718,  718,  718,  718,  718,  718,  718, 1225, 1225,
     1225, 1225, 1225, 1225, 1225, 1225, 1225, 1225, 1225, 1225,
     1225, 1225, 1225, 1225, 1225, 1225,  229,  229,  229,  229,
      229,  229,  229,  229,  229,  229,  229,  229,  229,  229,
      229,  229,  229,  229, 1269, 1269, 1269, 1269, 1269, 1269,
     1269, 1269, 1269, 1269, 1269, 1269, 1269, 1269, 1269, 1269,

     1269, 1269,  241,  241,  241,  241,  241,  241,  241,  241,
      241,  241,  241,  241,  241,  241,  241,  241,  241,  241,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244, 1285, 1285,
     1285, 1285, 1285, 1285, 1285, 1285, 1285, 1285, 1285, 1285,
     1285, 1285, 1285, 1285, 1285, 1285,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324, 1342, 1342, 1342, 1342, 1342, 1342,
     1342, 1342, 1342, 1342, 1342, 1342, 1342, 1342, 1342, 1342,
     1342, 1342,  334,  334,  334,  334,  334,  334,  334,  334,

      334,  334,  334,  334,  334,  334,  334,  334,  334,  334,
     1352, 1352, 1352, 1352, 1352, 1352, 1352, 1352, 1352, 1352,
     1352, 1352, 1352, 1352, 1352, 1352, 1352, 1352, 1356, 1356,
     1935, 1356, 1356, 1356, 1356, 1356, 1356, 1356, 1356, 1356,
     1356, 1356, 1356, 1356, 1356, 1356, 1357, 1357, 1935, 1357,
     1357, 1357, 1357, 1357, 1357, 1357, 1357, 1357, 1357, 1357,
     1357, 1357, 1357, 1357,  115,  115, 1935,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115, 1358, 1358, 1358, 1358, 1358, 1358, 1358, 1358,
     1358, 1358, 1358, 1358, 1358, 1358, 1358, 1358, 1358, 1358,

      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118, 1360, 1360,
     1360, 1360, 1360, 1360, 1360, 1360, 1360, 1360, 1360, 1360,
     1360, 1360, 1360, 1360, 1360, 1360,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124, 1363, 1935, 1935, 1935, 1935, 1935,
     1363, 1935, 1935, 1935, 1935, 1935, 1363, 1363, 1363, 1363,
     1363, 1368, 1368, 1935, 1368, 1368, 1368, 1368, 1368, 1368,
     1368, 1368, 1368, 1368, 1368, 1368, 1368, 1368, 1368,  637,
     1935, 1935, 1935, 1935, 1935, 1935,  637, 1935,  637, 1935,

     1935,  637,  637,  637,  637,  133, 1935, 1935, 1935, 1935,
     1935, 1935, 1935,  133, 1935,  133, 1935,  133,  133,  133,
      133,  133,  640,  640,  640,  640, 1378, 1378, 1935, 1378,
     1378, 1378, 1378, 1378, 1378, 1378, 1378, 1378, 1378, 1378,
     1378, 1378, 1378, 1378,  719,  719,  719,  719,  719,  719,
     1935,  719,  719,  719,  719,  719,  719,  719,  719,  719,
      719,  719,  720,  720, 1935,  720,  720,  720,  720,  720,
      720,  720,  720,  720,  720,  720,  720,  720,  720,  720,
     1226, 1226, 1935, 1226, 1226, 1226, 1226, 1226, 1226, 1226,
     1226, 1226, 1226, 1226, 1226, 1226, 1226, 1226, 1225, 1225,

     1225, 1225, 1225, 1225, 1225, 1225, 1225, 1225, 1225, 1225,
     1225, 1225, 1225, 1225, 1225, 1225,  229,  229,  229,  229,
      229,  229,  229,  229,  229,  229,  229,  229,  229,  229,
      229,  229,  229,  229, 1445, 1445, 1445, 1445, 1445, 1445,
     1445, 1445, 1445, 1445, 1445, 1445, 1445, 1445, 1445, 1445,
     1445, 1445,  241,  241,  241,  241,  241,  241,  241,  241,
      241,  241,  241,  241,  241,  241,  241,  241,  241,  241,
      244,  244,  244,  244,  244,  244,  244,  244,  244,  244,
      244,  244,  244,  244,  244,  244,  244,  244, 1453, 1935,
     1453, 1935, 1935, 1935, 1935, 1453, 1935, 1935, 1453, 1453,

     1453, 1453, 1453, 1453, 1456, 1456, 1456, 1456, 1456, 1456,
     1456, 1456, 1456, 1456, 1456, 1456, 1456, 1456, 1456, 1456,
     1456, 1456, 1501, 1501, 1501, 1501, 1501, 1501, 1501, 1501,
     1501, 1501, 1501, 1501, 1501, 1501, 1501, 1501, 1501, 1501,
      324,  324,  324,  324,  324,  324,  324,  324,  324,  324,
      324,  324,  324,  324,  324,  324,  324,  324, 1503, 1503,
     1503, 1503, 1503, 1503, 1503, 1503, 1503, 1503, 1503, 1503,
     1503, 1503, 1503, 1503, 1503, 1503,  334,  334,  334,  334,
      334,  334,  334,  334,  334,  334,  334,  334,  334,  334,
      334,  334,  334,  334,  115,  115, 1935,  115,  115,  115,

      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124, 1363, 1935,
     1935, 1935, 1935, 1935, 1363, 1935, 1935, 1935, 1935, 1935,
     1363, 1363, 1363, 1363, 1363,  637, 1935, 1935, 1935, 1935,
     1935, 1935,  637, 1935,  637, 1935, 1935,  637,  637,  637,
      637,  133, 1935, 1935, 1935, 1935, 1935, 1935, 1935,  133,
     1935,  133, 1935,  133,  133,  133,  133,  133,  640,  640,

      640,  640, 1513, 1935, 1513, 1935, 1935, 1935, 1935, 1513,
     1935, 1935, 1513, 1513, 1513, 1513, 1513, 1513, 1567, 1935,
     1567, 1935, 1935, 1935, 1935, 1567, 1935, 1935, 1567, 1567,
     1567, 1567, 1567, 1567,  485,  485,  485,  485,  485,  485,
      485,  485,  485,  485,  485,  485,  485,  485,  485,  485,
      485,  485, 1654, 1654, 1654, 1654, 1654, 1682, 1682, 1935,
     1682, 1682, 1682, 1682, 1682, 1682, 1682, 1682, 1682, 1682,
     1682, 1682, 1682, 1682, 1682,  669,  669,  669,  669,  669,
      669,  669,  669,  669,  669,  669,  669,  669,  669,  669,
      669,  669,  669, 1724, 1724, 1724, 1724, 1724, 1724, 1724,

     1724, 1724, 1724, 1724, 1724, 1724, 1724, 1724, 1724, 1724,
     1724, 1766, 1766, 1766, 1766, 1766, 1766, 1766, 1766, 1766,
     1766, 1766, 1766, 1766, 1766, 1766, 1766, 1766, 1766, 1786,
     1786, 1786, 1786, 1786, 1786, 1786, 1786, 1786, 1786, 1786,
     1786, 1786, 1786, 1786, 1786, 1786, 1786, 1787, 1787, 1787,
     1787, 1787, 1787, 1787, 1787, 1787, 1787, 1787, 1787, 1787,
     1787, 1787, 1787, 1787, 1787, 1793, 1793, 1793, 1793, 1793,
     1793, 1793, 1793, 1793, 1793, 1793, 1793, 1793, 1793, 1793,
     1793, 1793, 1793, 1811, 1811, 1811, 1811, 1811, 1811, 1811,
     1811, 1811, 1811, 1811, 1811, 1811, 1811, 1811, 1811, 1811,

     1811, 1820, 1820, 1820, 1820, 1820, 1820, 1820, 1820, 1820,
     1820, 1820, 1820, 1820, 1820, 1820, 1820, 1820, 1820, 1823,
     1823, 1823, 1823, 1823, 1823, 1823, 1823, 1823, 1823, 1823,
     1823, 1823, 1823, 1823, 1823, 1823, 1823, 1864, 1864, 1864,
     1864, 1864, 1864, 1864, 1864, 1864, 1864, 1864, 1864, 1864,
     1864, 1864, 1864, 1864, 1864, 1885, 1885, 1885, 1885, 1885,
     1885, 1885, 1885, 1885, 1885, 1885, 1885, 1885, 1885, 1885,
     1885, 1885, 1885, 1914, 1914, 1914, 1914, 1914, 1914, 1914,
     1914, 1914, 1914, 1914, 1914, 1914, 1914, 1914, 1914, 1914,
     1914, 1917, 1917, 1917, 1917, 1917, 1917, 1917, 1917, 1917,

     1917, 1917, 1917, 1917, 1917, 1917, 1917, 1917, 1917, 1920,
     1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,
     1920, 1920, 1920, 1920, 1920, 1920, 1920, 1922, 1922, 1922,
     1922, 1922, 1922, 1922, 1922, 1922, 1922, 1922, 1922, 1922,
     1922, 1922, 1922, 1922, 1922, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1933, 1933, 1933, 1933, 1933, 1933, 1933,
     1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933,
     1933,   17, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,

     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935
    } ;

static yyconst flex_int16_t yy_chk[9763] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        2,    2,    2,    2,    8, 1933,    8,   10,   10,   10,
       11,   11,   11,   12,   12,   12,   71, 1958, 1958,   11,

       18,   71,   12,   19,   75,   19,    2,    2,   21,    8,
        2,   76,    2,   76,   18,   10,   23, 1931,   22,  106,
       24, 1930,   21,    2,   61,   61,   61,   61,  108,   18,
       23,  114,   19,  114,   24,    2,    2,   21,    8,    2,
       75,    2,   18,   22,   10,   23,   19,  322,  106,   24,
       21,    2,    6,    6,    6,    6,  108,   22,   23,   28,
       29,   19,   24,  322,   98,   34,   98,   30,   75,   98,
      113,   98,   22,   28,   19,   34,   35,   29,    6,    6,
     1927,   30,    6,   86,    6,   22,  112,   86,   28,   34,
       35,   29,   98,   86,   98,    6,   30,   98,  113,   98,

      249,   28,  249, 1922,   34,   35,   29,    6,    6,   30,
     1920,    6,   86,    6, 1917,  112,   86,   34,   35,   29,
      788,   86,  788,    6,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,

       13,   13,   13,   13,   14,   14,   14,   14,   36,   25,
       25,   25,   25,   26,  116,   36,   14,  102, 1914,  102,
       26,   26,   14,  117,  162,   37,   37,  121,  821,   36,
       14,   14,   37,  600,   14,  102,   14,  154,   25,  154,
      139,   26,  154,  116,   36,  821,   37,   14,  110,  600,
      110,  117,   25,  162, 1913,   26,   27,   36,   27,   14,
       14,   37,  140,   14,  121,   14,  110,   25,  139,   27,
       26,   27, 1910,   27,   37,   14,   16,   16,   16,   16,
       25,   40,   46,   26,   38,   27,   40,   32,   32,   32,
      140,   38,  121,  166,   16,   32,   46,  144,   32,   27,

       40,  145,   16,   16,   32,   38,   16, 1906,   16,   32,
       40,   46,   45,   89,   27,   40,   32,   89, 1903,   16,
       38,   45,  166,   89,   46,  144,   45,   27,   40,  145,
       32,   16,   16,   38, 1859,   16,  124,   16,  111, 1858,
      111,   45,   89,  124,  124,   32,   89,   16,   31,   45,
       31,   89, 1837,  120,   45,   59,  111,   33,   32,   33,
      122,  122,  122,  122, 1813, 1811,   31,  111,   33,   33,
       33,   39,   31,  120,   39,   59,   31,   31,   31, 1787,
       39,   33,  163,   31,   33,   31,   31,   39,   39,   59,
      164,   31,   74,   74,   74,   31,  111,   33,   33,   33,

       39,   31,  120,   39,   59,   31,   31,   31,   39,   33,
      163,   31,   33,   31,   31,   39,   39,   59,  164,   31,
       74, 1786,  165,   31,   41,   48,   42,   42,   48,   41,
       42,   41,   49,   49,  921,   41,   42,  921,   41,   48,
       62,   42,   49,   41,  126,   62, 1780,   49,   41,   74,
      165,  126,  126,   41,   48,   42,   42,   48,   41,   42,
       41,   49,   49,   41,   42,   47,   41,   48,   62,   42,
       49,   41,   47,   47,   47,   49,   41,   43, 1163,   47,
      167, 1163,   62,   43, 1761,   43,   43,   87,   43,   43,
      169,   43,  173,  168,   47,   43,  171,   62, 1760,   90,

       47,   47,   47,   90, 1444, 1755,   43,   47,  167,   90,
       62,   43,   50,   43,   43,   87,   43,   43,  169,   43,
      173,   50,  168,   43,   44,  171,   50,   44,   90,   87,
       44,   44,   90,   44,   57,   54,   54,   90,   44,   44,
     1752,   50,   57,  252,   87,  252,  252,   57,   57,   50,
       54, 1736, 1723,   44,   50, 1444,   44,   87,   44,   44,
     1722,   44,  177,   57,   54,   54,   44,   44,   51,   52,
       57,  141,   51,   51,  142,   57,   57,   52,   54,   52,
       51,   63,   51,   52,   52,  434,  141,   51,   51,  142,
      177,  170,  170,  176,  434,  434, 1717,   51,   52,   63,

      141,   51,   51,  142,   53,   52, 1716,   52,   51,   53,
       51,   52,   52,   63,  141,   51,   51,  142,   53,  170,
      170,   53,  176,   53,   53,   55, 1715,   91,   63,   55,
       55,   91,  193,   53,  125,  125,  125,   91,   53,   55,
      128,   63,  125,  125,   55,   55,   53,  128,  128,   53,
       56,   53,   53,  212,   55,   56,   91,   56,   55,   55,
       91,  193,   58,  128,   58,   91,  172,   55,  130,   56,
      130,  172,   55,   55,   58,  131,   58,  131,  323,   56,
      323,  130,  212,  130,   56, 1450,   56,  178,  131,  143,
      131,   58,  128,   58,  179,  172,  323,   56,   81,  143,

      172,  433,   58,  433,   58,   60,   60,   60,   60,   64,
       65,  433,  161,   64,   64,  178,   65,   64,  143,   65,
     1502,   81,  179,   64,   65,  181,   81,  143,   64,  161,
      182,   60,   60, 1710,   60,   60, 1450,   60,   64,   65,
       81,  161,   64,   64,   65, 1688,   64,   65,   60,  183,
       81,   64,   65,  181,  214,   81,   64,  161,  182, 1681,
       60,   60,   67,   60,   60,   66,   60,   67,   81,   67,
     1672, 1502,   66,   66,   66,  718,   60,  183,   80,   66,
       88,   67,  180,  214,  718,  718,   67,   79,   79,   79,
       79,   67, 1608,  180,   66, 1608,   67,   83,   67,   79,

       66,   66,   66,   80,  175, 1709,   80,   66,   88,   67,
      184,  180,   80,   79,   67,   88,   88,   88,  175,   80,
       80,  180,   88,   93,   83,   83, 1682,  185,   83, 1682,
     1671,  215,   80,  175,   83,   80,  188,   88,  184,   83,
       80, 1669,   79,   88,   88,   88,  175,   80,   80,   82,
       88,   93, 1667,   83,   83,  185, 1709,   83,  174,   93,
      215,   93,   83, 1665,  188,   93,   93,   83,  146,  146,
      146, 1662,   82,  216,   85,  218,  146,   82,   85,   82,
       93,   85,   85,   82,   85,  146,   82,   93,   95,   93,
       85,   82,   95,   93,   93,  191,   82, 1658,   95,  174,

      174,   82,  216,   85,  218,  192,   82,   85,   82,   85,
       85,   82,   85,  224,   82,  195,  231,   95,   85,   82,
     1657,   95,  100,  191,   82,   84,   95,  174,  174,   84,
      100,  196,   84,  192,   84,   84, 1592,   84,   84,  231,
      129,   92,  224,  195,   84,   92, 1591,  129,  129,   92,
      100, 1587, 1586,   92,   84,   92, 1728, 1585,   84,  196,
       84,   92,   84,   84,  100,   84,   84,  194,  231, 1583,
       92,   94,   84,  234,   92,  194,   97, 1575,   92,  100,
      129,   92,  197,   92,   94,   94,   94, 1684,   94,   92,
     1684,   94,  100,  186,  375,  186,  194,   99,  198,   97,

       94,   96,  234,  194,   97,   96,   97, 1728,  129,   96,
      197, 1569,   94,   94,   94,   96,   94,  199,   97,   94,
       96,   96,  138,  375, 1501,   99,  198,   99,   97,  103,
       96,  138,  138,   97,   96,   97,  186,   99,   96,   99,
      103, 1499,  200,   96,  138,  199,   97, 1750,   96,   96,
      101,  101,  101,  101,   99, 1494,   99,  103, 1493,  107,
      138,  138,  101,  107,  186,   99,  107,   99, 1491,  107,
      200,  103,  138,  137,  202,  137,  101,  790, 1488,  790,
      201,  147,  147,  147,  147,  205,  103,  208,  107,  109,
     1484,  205,  107,  147,  107,  201,  109,  107, 1750,  103,

      109,  137,  202,  109, 1481,  101,  104,  147,  104,  201,
      137, 1480,  209,  109,  205,  208, 1479,  152,  109,  205,
      137, 1473,  104,  201,  104,  109,  152,  152,  109,  203,
      137,  109,  203,  104,  104,  104,  147,  204,  137,  152,
      209,  109,  148,  148,  148,  148,  104,  204,  137,  104,
      148,  221,  221,  221,  221,  152,  152,  203, 1466,  148,
      203, 1461,  104,  104,  104,  359,  204,  152,  228,  228,
      228,  228,  359,  359,  104,  204, 1456,  104,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,

      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  133, 1446,
      133,  367,  210,  367,  134,  254,  134,  254,  254, 1445,
      213,  133,  133,  133,  133,  367,  133,  134,  134,  134,
      134,  135,  134,  135, 1369, 1362,  190,  190,  134,  153,
      210,  153, 1359,  190,  135,  135,  135,  135,  213,  135,

      153,  153,  187,  206,  187,  421,  189,  421,  232,  225,
      133, 1358,  187,  153,  190,  190,  134,  134,  189,  206,
      189,  190,  135,  189,  233,  211,  187,  421, 1342,  153,
      153,  232,  206,  135,  207,  189,  207,  225,  226,  211,
      217,  153,  211,  220,  217,  223,  189,  206,  189, 1341,
      135,  189,  207,  207,  211,  187,  207,  220, 1836,  223,
      232,  227,  237,  207,  233,  207,  226,  211,  227,  217,
      211,  238,  220,  217,  223,  230,  230,  230,  230,  235,
      207,  207,  236,  236,  207,  220,  235,  223,  239,  227,
      237,  240,  233,  243,  328,  247,  227,  248, 1337,  238,

      239,  351,  239, 1225,  352,  239,  377,  235, 1332, 1836,
      236,  236, 1225, 1225,  235,  353,  243,  239, 1838,  240,
      246,  246,  246,  248,  253,  253,  253,  253,  239,  351,
      239,  247,  352,  239,  328,  377,  253,  257,  257,  257,
      257,  262,  376,  353,  376,  243,  258,  376,  246,  257,
      253, 1331,  248,  258,  258,  258, 1330,  262,  262,  247,
      258,  259,  328,  257,  259,  260,  321,  260,  321, 1838,
      262,  259,  259,  259,  264,  258, 1328,  246,  259,  253,
      261,  258,  258,  258,  260,  262,  262,  263,  258,  261,
      259,  264,  257,  259,  260,  321,  260,  261,  265,  259,

      259,  259,  263,  264,  263,  269,  259,  265,  346,  261,
      346,  267,  260, 1312, 1308,  265,  263,  261,  266,  264,
      266, 1298,  269,  267,  321,  261,  346,  265,  267,  271,
      263,  268,  263, 1288,  269,  265,  327,  266,  272,  271,
      267,  268,  268,  265, 1285, 1269,  271,  266,  268,  266,
      269,  267,  270, 1268,  270,  399,  267,  270,  271,  327,
      268,  273,  272,  273,  329,  266,  329,  271,  354,  268,
      268,  270,  274,  274,  271, 1226,  268,  273,  273,  272,
      272,  270,  374,  270,  399, 1179,  270,  400,  327,  274,
      273,  272,  273,  329,  275,  276,  354,  281,  278,  270,

      384,  274,  274,  277,  278,  273,  273,  272,  272,  275,
      374,  275,  276,  276,  281,  278,  400,  274,  277, 1169,
      277,  279,  329,  275,  276, 1165,  281,  278,  384,  284,
      279,  284,  277,  278,  280,  403,  283,  275,  279,  275,
      276,  276,  281,  278,  282, 1164,  277,  283,  277,  413,
      279,  280,  280,  283,  299,  284,  299,  286,  279,  282,
      386,  282, 1142,  280,  403,  283,  279,  285, 1850,  285,
      286,  299,  284,  282,  286,  283,  414,  285,  413,  280,
      280,  283,  417,  299,  284,  299,  286,  282,  386,  282,
      333,  285,  287,  285,  317,  317,  317,  317,  286,  299,

      284,  289,  286, 1132,  287,  414,  287,  288, 1131,  287,
      285,  417,  289,  333,  290, 1118,  288,  288,  289, 1850,
      285,  287,  285,  288,  288,  290,  319,  319,  319,  319,
      289,  290,  287,  348,  287,  348,  288,  287,  285,  356,
      289, 1115,  333,  290,  288,  288,  289,  292,  292,  294,
      291,  288,  288,  290,  291, 1099,  292,  300,  293,  290,
     1094,  419,  348,  756,  292,  756,  294,  291,  293,  294,
      356,  295,  297,  756,  300,  293,  292,  292,  294,  291,
      295,  297,  296,  291,  292,  401,  300,  293,  295,  297,
      419,  348,  292,  296,  294,  291,  293,  294,  356,  296,

      295,  297,  300,  293, 1086,  298,  308, 1085,  295,  297,
     1077,  296,  302,  401,  298,  301,  295,  297,  302,  303,
      422,  296,  298,  308,  308,  303, 1072,  296,  302,  302,
      301,  303,  301,  301,  298,  308,  303,  304,  304,  357,
      425,  302,  298,  426,  301,  402,  306,  302,  303,  422,
      298,  308,  308,  304,  303,  304,  302,  302,  301,  303,
      301,  301, 1048,  306,  303,  306,  304,  304,  305,  425,
      305,  307,  426,  402,  941,  306,  360,  360,  360,  357,
      307,  304,  404,  304,  360,  360,  305,  305,  307,  934,
      305,  306,  309,  306,  309,  310,  310,  305,  928,  305,

      307,  884,  311,  342,  881,  877,  309,  357,  307,  309,
      404,  309,  405,  310,  305,  305,  307,  311,  305,  311,
      342,  309,  362,  309,  310,  310,  312,  313,  312,  362,
      362,  311,  342,  313,  309,  871,  314,  309,  314,  309,
      405,  310,  406,  312,  313,  311,  407,  311,  342,  387,
      869,  387,  387,  314,  408,  312,  313,  312,  315,  640,
      315,  640,  313,  316,  315,  314,  330,  314,  330,  316,
      406,  312,  313,  640,  407,  315,  318,  318,  318,  318,
      316,  314,  408,  331,  330,  331,  350,  315,  318,  315,
      411,  858,  316,  315,  389,  330,  389,  389,  316,  331,

      350,  331,  318,  315,  332,  332,  332,  332,  316,  336,
      331,  331,  410,  856,  338,  350,  332,  338,  411,  340,
      336,  338,  412,  331,  330,  340,  340,  336,  350,  410,
      332,  318,  911,  911,  911,  361,  361,  361,  336,  331,
      331,  410,  338,  361,  361,  338,  839,  340,  336,  338,
      412,  331,  833,  340,  340,  336, 1040,  410, 1040,  332,
      334,  334,  334,  334,  334,  334,  334,  334,  334,  334,
      334,  334,  334,  334,  334,  334,  334,  334,  334,  337,
      334,  334,  334,  334,  334,  339,  339,  339,  343, 1042,
      337, 1042,  339,  341,  870,  870,  870,  337,  409,  418,

      343,  409,  341,  427,  870,  343,  810,  429,  337,  341,
      432,  334,  334,  339,  339,  339,  366,  343,  337,  368,
      339,  368,  341,  366,  366,  337,  409,  418,  343,  409,
      341,  427,  368,  343,  368,  429,  802,  341,  432,  334,
      335,  335,  335,  335,  335,  335,  335,  335,  335,  335,
      335,  335,  335,  335,  335,  335,  335,  335,  335,  366,
      335,  335,  335,  335,  335,  344,  801,  345,  347,  378,
      347,  378,  601,  428,  601,  428,  335,  344,  436,  344,
      345,  365,  344,  335,  345,  378,  347,  366,  365,  365,
      601,  335,  335,  720,  344,  428,  345,  347,  719,  690,

      349,  383,  349,  383,  335,  344,  436,  344,  345, 1890,
      344,  335,  345,  689,  378,  365,  349,  383,  349,  335,
      369,  370,  369,  370,  669,  430,  347,  349,  349,  349,
      372,  373,  372,  373,  369,  370,  369,  370,  369,  370,
      349,  383,  378,  365,  372,  373,  372,  373,  372,  373,
      388,  388,  388,  388,  430,  415,  349,  349,  349,  431,
     1890,  431,  388,  424,  424,  372,  668,  664,  349,  383,
      416,  415,  369,  370,  663,  373,  388,  392,  392,  392,
      392,  437,  372,  373,  438,  392,  416,  393,  393,  393,
      393,  424,  424,  372,  392,  393,  415,  396,  423,  393,

      423,  435,  431,  373,  393,  388,  396,  396,  439,  437,
      435,  416,  438,  393,  440,  660,  441,  442,  446,  396,
      423,  439,  443,  445,  415,  447,  448,  658,  449,  435,
      431,  444,  451,  444,  423,  396,  396,  439,  435,  416,
      452,  453,  393,  440,  441,  442,  446,  396,  420,  439,
      420,  443,  445,  444,  447,  448,  449,  450,  454,  450,
      455,  451,  423,  457, 1050,  458, 1050,  459,  452,  453,
      420,  460,  462,  650,  461,  420,  647,  463,  464,  450,
      420,  466,  461,  466,  467,  468,  454,  472,  465,  455,
      420,  420,  457,  458,  420,  459,  465,  469,  470,  471,

      460,  462,  461,  466,  420,  463,  473,  464,  474,  420,
      461,  475,  467,  468,  476,  472,  466,  465,  420,  420,
      477,  478,  420,  480,  465,  466,  469,  470,  471,  481,
      482,  484,  486,  487,  473,  488,  474,  489,  490,  475,
      491,  492,  493,  476,  466,  494,  495,  646,  477,  478,
      496,  480,  497,  466,  498,  499,  501,  481,  482,  484,
      486,  487,  502,  488,  500,  489,  490,  645,  491,  497,
      492,  503,  503,  503,  632,  495,  493,  500,  516,  496,
      596,  497,  519,  498,  499,  501,  515,  505,  494,  505,
      505,  502,  506,  500,  506,  506,  514,  497,  509,  492,

      509,  509,  517,  503,  493,  500,  518,  510,  510,  510,
      510,  511,  516,  511,  511,  520,  494,  515,  522,  510,
      502,  521,  523,  514,  529,  519,  524,  528,  530,  530,
      590,  525,  503,  510,  526,  527,  532,  518,  567,  517,
      516,  531,  513,  512,  599,  530,  515,  533,  520,  541,
      528,  529,  514,  519,  539,  521,  525,  536,  522,  548,
      540,  524,  510,  523,  524,  531,  518,  517,  526,  527,
      530,  532,  599,  525,  533,  508,  520,  547,  545,  528,
      529,  539,  538,  521,  507,  525,  522,  540,  536,  524,
      541,  523,  524,  543,  531,  549,  526,  527,  530,  532,

      544,  525,  548,  533,  534,  534,  534,  535,  535,  535,
      539,  547,  537,  537,  537,  545,  540,  536,  541,  542,
      542,  542,  553,  538,  538,  544,  534,  546,  543,  535,
      548,  534,  550,  504,  537,  551,  534,  557,  549,  547,
      552,  542,  554,  545,  625,  563,  534,  534,  537,  485,
      534,  538,  538,  560,  544,  556,  543,  483,  559,  550,
      534,  546,  551,  562,  553,  534,  549,  558,  554,  564,
      546,  569,  550,  625,  534,  534,  537,  565,  534,  563,
      557,  552,  555,  555,  555,  559,  556,  566,  550,  546,
      562,  551,  553,  558,  568,  570,  560,  554,  546,  572,

      550,  561,  561,  561,  555,  571,  575,  563,  557,  552,
      573,  574,  569,  565,  559,  556,  564,  568,  566,  562,
      580,  583,  558,  561,  560,  584,  576,  587,  581,  578,
      582,  570,  579,  575,  586,  572,  571,  589,  585,  573,
      569,  565,  588,  572,  564,  574,  568,  566,  577,  577,
      577,  580,  576,  591,  593,  581,  587,  582,  627,  570,
      576,  583,  575,  572,  592,  571,  604,  584,  573,  398,
      577,  572,  578,  574,  586,  579,  585,  622,  588,  589,
      580,  576,  623,  577,  581,  587,  582,  627,  576,  583,
      397,  394,  577,  591,  624,  584,  593,  603,  592, 1052,

      578, 1052,  586,  579,  585,  622,  588,  589,  626,  604,
      623,  577,  594,  594,  594,  594,  595,  595,  595,  595,
      577,  591,  624,  602,  593,  602,  592,  597,  597,  597,
      597,  603,  605,  606,  605,  606,  626,  604,  607,  597,
      607,  602,  608,  391,  618,  657,  390,  612,  643,  671,
      643,  606,  602,  597,  607,  619,  607,  385,  616,  603,
      612,  605,  606,  611,  643,  607,  607,  618,  382,  672,
      611,  608,  621,  657,  616,  613,  612,  671,  607,  619,
      620,  602,  597,  598,  598,  598,  598,  616,  612,  613,
      605,  606,  611,  381,  607,  607,  618,  672,  611,  621,

      608,  615,  616,  615,  613,  380,  607,  620,  619,  598,
      598,  614,  598,  598,  617,  598,  630,  613,  615,  614,
      620,  617,  614,  630,  630,  673,  598,  629,  621,  379,
      615,  661,  615,  661,  661, 1138,  620, 1138,  598,  598,
      614,  598,  598,  617,  598,  628,  615,  614,  620,  617,
      614,  629,  371,  673,  598,  609,  609,  609,  609,  609,
      609,  609,  609,  609,  609,  609,  609,  609,  609,  609,
      609,  609,  609,  609,  631,  609,  609,  609,  609,  609,
      629,  631,  631,  628,  635,  633,  635,  636,  644,  636,
      644,  609,  633,  633,  674,  675,  639,  635,  639,  635,

      636,  676,  636,  677,  644,  634,  609,  609,  364,  363,
      639,  628,  634,  634,  639,  638,  358,  638,  662,  609,
      662,  662,  355,  674,  675,  797,  633,  797,  797,  638,
      676,  677,  634,  638,  609,  610,  610,  610,  610,  610,
      610,  610,  610,  610,  610,  610,  610,  610,  610,  610,
      610,  610,  610,  610,  633,  610,  610,  610,  610,  610,
      637,  634,  637,  641,  610,  641,  642,  638,  642,  679,
      326,  637,  648,  637,  648,  637,  637,  641,  637,  641,
      642,  641,  642,  655,  642,  655,  610,  610,  648,  665,
      665,  665,  665,  610,  642,  680,  324,  665,  679,  655,

     1145, 1145, 1145,  681,  682,  641,  665,  684,  666,  666,
      666,  666,  637,  683,  610,  641,  666,  685,  642,  686,
      666,  687,  688,  642,  680,  666,  692,  667,  667,  667,
      667,  681,  682,  641,  666,  667,  684,  670,  670,  670,
      670,  683,  691,  696,  667,  670,  685,  686,  697,  687,
      688,  693,  698,  693,  670,  692,  699,  700,  701,  703,
      701,  706,  707,  666,  709,  715,  704,  711,  704,  320,
      691,  696,  710,  712,  710,  712,  714,  697,  714,  716,
      701,  698,  256,  693,  699,  722,  700,  703,  704,  706,
      707,  723,  709,  715,  710,  712,  711,  693,  714,  724,

      721,  721,  721,  725,  726,  728,  730,  716,  729,  727,
      721,  721,  693,  721,  722,  721,  727,  731,  732,  723,
      732,  733,  721,  733,  736,  693,  694,  724,  694,  739,
      735,  725,  746,  726,  728,  730,  729,  743,  727,  737,
      732,  737,  747,  733,  727,  738,  731,  738,  694,  740,
      745,  740,  736,  694,  748,  255,  749,  739,  694,  735,
      746,  737,  750,  751,  752,  754,  743,  738,  694,  694,
      747,  740,  694,  753, 1272,  251, 1272,  766,  721,  745,
      738,  761,  694,  748,  749,  762,  763,  694,  764,  767,
      750,  751,  768,  752,  754,  770,  694,  694,  771,  774,

      694,  753,  758,  760,  758,  760,  766,  775,  738,  761,
      773,  776,  773,  762,  763,  778,  764,  779,  767,  780,
      768,  781,  782,  770,  758,  760,  783,  771,  774,  784,
      785,  786,  773,  787,  791,  789,  775,  794,  808,  804,
      776,  250,  796,  789,  778,  779,  792, 1276,  780, 1276,
      781,  782,  244,  792,  803,  783,  241,  792,  786,  805,
      793,  787,  793,  791,  789,  808,  795,  795,  795,  784,
      793,  785,  789,  807,  793,  792,  804,  794,  796,  793,
      798,  792,  798,  798,  806,  792,  799,  786,  799,  799,
      800,  809,  800,  800,  808,  803,  811,  784,  795,  785,

      805,  814,  807,  812,  804,  794,  796,  813,  815,  806,
      816,  818,  817,  819,  820,  822,  229,  828,  826,  811,
      823,  825,  834,  803,  831,  827,  923,  795,  805,  222,
      812,  807,  809,  923,  923,  814,  829,  842,  806,  817,
      832,  816,  127,  830,  813,  818,  826,  823,  811,  822,
      820,  815,  827,  824,  824,  824,  819,  840,  825,  812,
      809,  836,  834,  814,  828,  831,  829,  838,  817,  847,
      816,  841,  813,  818,  843,  826,  823,  822,  820,  815,
      842,  827,  832,  845,  819,  824,  825,  830,  840,  852,
      834,  836,  828,  831,  844,  829,  835,  835,  835,  824,

      837,  837,  837,  846,  851,  847,  848,  849,  842,  838,
      832,  841,  845,  859,  824,  830,  843,  840,  835,  845,
      836,  857,  837,  852, 1277,  844, 1277,  824,  860,  851,
      849,  861,  855,  847,  846,  848,  863,  838,  864,  841,
      859,  845,  865,  867,  843,  862,  878,  845,  850,  850,
      850,  852,  857,  866,  844,  853,  853,  853,  851,  849,
      854,  854,  854,  846,  848,  860,  861,  868,  855,  859,
      850,  878,  862,  873,  875,  874,  866,  853,  863,  876,
      864,  857,  854,  867,  865,  872,  872,  872,  880,  882,
      883,  879,  886,  860,  861,  854,  855,  891,  868,  123,

      878,  862,  885,  885,  885,  866,  863,  872,  864,  874,
      916,  867,  865,  873,  875,  876,  879,  883,  899,  886,
      887,  896,  888,  854,  885,  891,  882,  868,  902,  119,
      902,  880,  889,  889,  889,  889,  897,  874,  893,  916,
      893,  873,  875,  876,  118,  879,  883,  887,  886,  888,
      890,  890,  890,  890,  882,  894,  893,  894,  899,  880,
       78,  915,  896,  897,  903,  895,  903,  895,   72, 1370,
      902, 1370,  904,  894,  904,  898,  887,  898,  888,  892,
      892,  892,  892,  895,  894,  929,  899,  929,  903,  915,
      896,  898,  897,  898,  895,  912,  918,  892,  902,  929,

      904,   69,  898,  898,  945,  892,  892,  946,  892,  892,
      905,  892,  905,  894,  906,  898,  906,  903,  912,  924,
       17,  924,  892,  895,    9,  918,  907,    7,  907,  904,
      924,  898,  898,  945,  892,  892,  946,  892,  892,  937,
      892,  937,  937,  898, 1353, 1353, 1353,  912,  905,  906,
      892,  900,  900,  900,  900,  900,  900,  900,  900,  900,
      900,  900,  900,  900,  900,  900,  900,  900,  900,  900,
      907,  900,  900,  900,  900,  900,  905,  906, 1172,    0,
     1172,  914,  914,  914,  938,  950,  938,  938,  943, 1172,
      944,  914,  947,    0,  953,  914,  949,  908,  907,  908,

      914,    0,  900,  900,    0,  939,  939,  939,  939,  940,
      940,  940,  940,  939,  950,    0,  943,  940,  944,    0,
      947,  908,  939,  953,  949, 1060,  940, 1060, 1060,  908,
      900,  901,  901,  901,  901,  901,  901,  901,  901,  901,
      901,  901,  901,  901,  901,  901,  901,  901,  901,  901,
      908,  901,  901,  901,  901,  901,  920,  909,  908,  909,
      910,  913,  910,  926,  922,  926,    0,  952,  954,    0,
      955,  922,  922,    0,  926,  957,  927,  955,  927,  926,
      920,  926,  901,  901,  930,  909,  930,  927,  910,  960,
      913,  922,  927,  958,  927,  952,  954,  913,  930,  955,

      930,  913,  930,  957,  959,  955,  961,  965,    0,  920,
      901,  963,  930,    0,  909,  926,  968,  910,  960,  913,
      922,  958,    0, 1166, 1061,  913, 1061, 1061,  927,  913,
     1166, 1166,  959,    0,  961,  965,  930,  962,  964,  962,
      963,  930,  931,  931,  968,  931,  931,  931,  931,  931,
      931,  931,  931,  931,  931,  931,  931,  931,  931,  931,
      931,  931,  931,  931,  931,  931,  931,  964,  966,  962,
      942,  942,  942,  942,  967,  969,    0,  972,  942,  973,
      974,  975,  942,  962,  976,  984,  977,  942,  977,  978,
      979,  990,  979,  931,  931,  931,  942,  966,  962,  991,

      982,    0,  982,  967,  969,  972,    0,  973,  977,  974,
      975,  962,  979,  976,  984,    0,  992,  978,    0,    0,
      990,  931,  982,    0,    0,  942,  948,  948,  991,  948,
      948,  948,  948,  948,  948,  948,  948,  948,  948,  948,
      948,  948,  948,  948,  948,  992,  948,  948,  948,  948,
      948,  986,  987,  986,  987,  994,    0, 1062,  995, 1062,
     1062,  996,  986,  987,  986,  987,  986,  987,  997,  988,
      988,  988,  998,  986,  987, 1000,  989,  948,  948,  988,
      988, 1003,  988,  994,  988,  989,  989,  995,  989,  996,
      989,  988,  999, 1001,  999, 1001,  997,  989, 1004, 1009,

     1005,  998, 1005, 1000, 1007,  948, 1007, 1010, 1011, 1012,
     1003, 1013, 1014, 1016,  999, 1001, 1017,    0, 1019, 1020,
     1021, 1022, 1005, 1022, 1026,    0, 1007, 1004, 1009,  986,
      987, 1022, 1024, 1027, 1024, 1028, 1010, 1011, 1012, 1013,
     1029, 1014, 1016, 1030, 1031, 1017, 1019,  988, 1020, 1021,
     1032, 1033, 1026,  989, 1024, 1034, 1041, 1039, 1035, 1039,
     1035, 1027, 1043, 1028, 1044, 1046, 1053, 1039, 1047, 1029,
     1049, 1030, 1051, 1031, 1045, 1054, 1045, 1059, 1032, 1033,
     1035, 1064, 1058, 1034, 1045, 1041, 1071, 1063, 1045, 1063,
     1063, 1043, 1044, 1045, 1053, 1047, 1046, 1065, 1055, 1049,

     1055, 1051, 1066, 1068, 1054, 1067, 1136, 1057, 1055, 1057,
     1074, 1071, 1055, 1058, 1092, 1092, 1092, 1055, 1188, 1059,
     1148, 1057, 1064, 1073, 1047, 1046, 1057, 1070, 1067, 1066,
     1105, 1105, 1105, 1065, 1136,    0, 1092, 1074,    0, 1068,
     1071,    0, 1058, 1133, 1133, 1133, 1188, 1059, 1167, 1148,
     1064,    0, 1105, 1133, 1075, 1167, 1167, 1067, 1066, 1073,
     1078, 1065,    0, 1080, 1079, 1070, 1074, 1068, 1069, 1069,
     1069, 1069, 1069, 1069, 1069, 1069, 1069, 1069, 1069, 1069,
     1069, 1069, 1069, 1069, 1069, 1069, 1069, 1073, 1069, 1069,
     1069, 1069, 1069, 1070, 1075, 1076, 1081, 1078, 1079, 1082,

     1083, 1088, 1087, 1084, 1080, 1676, 1090, 1093, 1100, 1102,
     1089, 1091, 1676, 1095, 1103, 1676, 1096, 1104, 1097, 1069,
     1069,    0, 1075, 1081, 1076, 1078, 1079, 1101, 1083, 1090,
     1098, 1076, 1080, 1084, 1108, 1082, 1087, 1089, 1106, 1088,
     1091, 1096, 1107, 1093, 1095, 1097, 1104, 1069, 1100, 1111,
     1103, 1102, 1081, 1076, 1101, 1109, 1098, 1083, 1090, 1076,
     1110, 1108, 1084, 1082, 1087, 1107, 1089, 1088, 1113, 1091,
     1096, 1093, 1112, 1095, 1097, 1104, 1100, 1111, 1103, 1102,
     1106, 1117, 1114, 1101, 1116, 1098, 1109, 1119, 1121, 1122,
     1108, 1110, 1123, 1120, 1107, 1124, 1125, 1126, 1112, 1127,

     1189, 1129, 1128,    0, 1113, 1130, 1111, 1149, 1106, 1114,
        0, 1116, 1117, 1121,    0, 1109, 1120,    0, 1149, 1150,
     1110, 1150, 1125, 1155, 1158, 1144, 1127, 1112, 1189, 1119,
     1122, 1124, 1113, 1141, 1123, 1174, 1149, 1174, 1114, 1126,
     1116, 1117, 1121, 1128, 1129, 1120, 1149, 1130, 1150, 1174,
     1150, 1125, 1155, 1158,    0, 1127, 1144, 1119, 1122, 1124,
     1141,    0, 1123, 1134, 1134, 1134, 1134, 1126, 1139,    0,
     1139, 1128, 1129, 1157,    0, 1130, 1135, 1135, 1135, 1135,
     1137, 1137, 1137, 1137, 1140, 1144, 1140, 1156, 1151, 1141,
     1152, 1168, 1181, 1191, 1181, 1181, 1152, 1139, 1168, 1168,

     1143, 1157, 1143, 1151, 1153,    0, 1137, 1137, 1153, 1137,
     1137,    0, 1137, 1140,    0, 1153, 1143, 1151, 1143, 1152,
        0, 1156, 1191, 1137, 1192, 1152, 1139, 1143, 1143,    0,
     1157, 1151, 1175, 1153, 1175, 1137, 1137, 1153, 1137, 1137,
     1143, 1137, 1140, 1153, 1170, 1175, 1170, 1175, 1171, 1156,
     1171, 1137, 1192,    0,    0, 1170, 1143, 1143, 1182, 1171,
     1182, 1182, 1170,    0,    0, 1198, 1171,    0, 1143, 1146,
     1146, 1146, 1146, 1146, 1146, 1146, 1146, 1146, 1146, 1146,
     1146, 1146, 1146, 1146, 1146, 1146, 1146, 1146,    0, 1146,
     1146, 1146, 1146, 1146, 1198, 1176, 1170, 1176, 1146, 1200,

     1183, 1183, 1183, 1183, 1185, 1194, 1185, 1195, 1183, 1176,
     1203, 1176, 1204, 1176, 1196, 1197, 1177, 1183, 1177,    0,
     1146, 1146, 1178, 1186, 1178, 1186, 1185, 1146, 1200, 1371,
     1177, 1371, 1177, 1194, 1177, 1195, 1178,    0, 1178, 1203,
     1178, 1204, 1196, 1197, 1442, 1186, 1442, 1176, 1146, 1147,
     1147, 1147, 1147, 1147, 1147, 1147, 1147, 1147, 1147, 1147,
     1147, 1147, 1147, 1147, 1147, 1147, 1147, 1147, 1177, 1147,
     1147, 1147, 1147, 1147, 1178, 1199, 1147, 1184, 1184, 1184,
     1184, 1202, 1206,    0, 1209, 1184, 1210, 1211, 1212, 1213,
     1360, 1214, 1215, 1216, 1184, 1216, 1219, 1360, 1360, 1220,

     1147, 1147,    0, 1199, 1217, 1147, 1217,    0, 1365, 1202,
     1365, 1206, 1209,    0, 1210, 1216, 1211, 1212, 1213, 1214,
     1215, 1365, 1201, 1365, 1201, 1219, 1217, 1220, 1147, 1154,
     1154, 1154, 1154, 1154, 1154, 1154, 1154, 1154, 1154, 1154,
     1154, 1154, 1154, 1154, 1154, 1154, 1154, 1154, 1201, 1154,
     1154, 1154, 1154, 1154, 1233, 1262, 1221, 1262, 1221,    0,
        0, 1201, 1229, 1231, 1232, 1262,    0, 1221, 1222, 1221,
     1222, 1221, 1222, 1223, 1237, 1223, 1237, 1201, 1221, 1222,
     1154, 1154, 1233, 1224, 1223, 1224, 1223, 1224, 1223, 1201,
     1228, 1229, 1231, 1232, 1224, 1223, 1237,    0,    0, 1228,

     1228,    0, 1228, 1278, 1228, 1278, 1278, 1246, 1154, 1173,
     1173, 1228, 1173, 1173, 1173, 1173, 1173, 1173, 1173, 1173,
     1173, 1173, 1173, 1173, 1173, 1173, 1173, 1173, 1173, 1173,
     1173, 1173, 1173, 1173, 1221, 1222, 1246, 1235, 1236,    0,
        0, 1240, 1227, 1227, 1227, 1242, 1239, 1241, 1239, 1241,
     1224, 1223, 1227, 1227, 1243, 1227, 1244, 1227,    0, 1247,
     1173, 1173, 1173, 1248, 1227, 1235, 1236, 1228, 1239, 1240,
        0,    0, 1279, 1242, 1279, 1279, 1338, 1338, 1338, 1338,
     1249, 1448, 1243, 1448, 1244,    0, 1241, 1247, 1173, 1187,
     1187, 1248, 1187, 1187, 1187, 1187, 1187, 1187, 1187, 1187,

     1187, 1187, 1187, 1187, 1187, 1187, 1187, 1187, 1249, 1187,
     1187, 1187, 1187, 1187, 1241, 1245, 1250, 1245, 1251, 1253,
     1227, 1257, 1258, 1257, 1258, 1260, 1264, 1259, 1265, 1259,
     1266, 1275, 1267, 1270, 1299, 1271, 1273, 1245, 1274, 1293,
     1187, 1187,    0,    0, 1258, 1250, 1251, 1257, 1253, 1259,
     1280, 1280, 1280, 1260, 1347, 1264, 1265,    0, 1275, 1266,
     1267, 1299, 1270, 1271, 1293, 1273, 1274, 1361, 1187, 1281,
     1281, 1281, 1280, 1283, 1361, 1361, 1257, 1452, 1289, 1452,
     1452, 1287, 1347, 1291, 1292, 1290, 1284, 1275, 1294, 1267,
     1299, 1281, 1297, 1293,    0, 1274, 1282, 1282, 1282, 1282,

     1282, 1282, 1282, 1282, 1282, 1282, 1282, 1282, 1282, 1282,
     1282, 1282, 1282, 1282, 1282, 1283, 1282, 1282, 1282, 1282,
     1282, 1284, 1286, 1287, 1289, 1290, 1291, 1292, 1295, 1300,
     1294, 1296, 1296, 1296, 1297, 1303, 1301, 1302, 1304, 1304,
     1304, 1305, 1306, 1283, 1309, 1286, 1310, 1282, 1282, 1284,
     1307, 1287, 1289, 1290, 1291, 1292, 1300, 1296, 1294, 1295,
     1304, 1311, 1297, 1301, 1305, 1313, 1314,    0, 1318, 1303,
     1296, 1309, 1319, 1310, 1286, 1282, 1320, 1302, 1316, 1306,
     1322, 1307, 1315, 1315, 1315, 1300, 1296, 1324, 1295, 1317,
     1317, 1317, 1301, 1305, 1321, 1321, 1321, 1303, 1296, 1323,

     1309, 1311, 1310, 1322, 1315, 1302, 1313, 1306, 1314, 1318,
     1307, 1325, 1316,    0, 1319, 1327, 1321, 1326,    0,    0,
     1340, 1329, 1336, 1320,    0, 1348, 1343, 1324, 1317, 1311,
        0, 1454, 1322, 1454, 1313, 1349, 1314, 1318,    0,    0,
     1316, 1323, 1319, 1380, 1326, 1333, 1333, 1333, 1325, 1340,
     1327, 1320, 1329, 1348, 1343, 1324, 1317, 1334, 1334, 1334,
     1335, 1335, 1335, 1349, 1336, 1339, 1339, 1339, 1339, 1323,
     1350, 1333, 1380, 1326, 1354, 1363, 1325, 1363, 1327, 1334,
        0, 1329, 1335, 1343,    0, 1455, 1363, 1455, 1498, 1498,
     1498, 1498, 1336, 1512,    0, 1512, 1512, 1354, 1350, 1351,

     1333, 1344, 1344, 1344, 1344, 1344, 1344, 1344, 1344, 1344,
     1344, 1344, 1344, 1344, 1344, 1344, 1344, 1344, 1344, 1344,
     1355, 1344, 1344, 1344, 1344, 1344, 1354, 1351, 1364, 1374,
     1364, 1374, 1372, 1372, 1372, 1372, 1376, 1381, 1376, 1385,
     1372, 1404, 1364, 1404,    0, 1860, 1379, 1860, 1355, 1372,
     1379, 1374, 1344, 1344, 1366, 1367, 1366, 1367, 1376,    0,
     1368,    0, 1368, 1404,    0,    0, 1381, 1385, 1366, 1367,
     1366, 1367, 1366, 1367, 1368, 1379, 1368,    0, 1368, 1379,
     1344, 1345, 1345, 1345, 1345, 1345, 1345, 1345, 1345, 1345,
     1345, 1345, 1345, 1345, 1345, 1345, 1345, 1345, 1345, 1345,

     1382, 1345, 1345, 1345, 1345, 1345, 1366, 1367, 1384, 1386,
     1387, 1393, 1368, 1373, 1373, 1373, 1373, 1389, 1391, 1392,
     1395, 1373, 1396, 1397,    0, 1345, 1398,    0, 1400, 1382,
     1373, 1399, 1345, 1345, 1511,    0, 1511, 1384, 1386, 1387,
     1393, 1599,    0, 1599, 1511, 1389, 1391, 1392,    0, 1395,
     1396, 1599, 1397, 1345, 1390, 1398, 1390, 1400, 1402, 1399,
     1345, 1346, 1346, 1346, 1346, 1346, 1346, 1346, 1346, 1346,
     1346, 1346, 1346, 1346, 1346, 1346, 1346, 1346, 1346, 1346,
     1390, 1346, 1346, 1346, 1346, 1346, 1403, 1402, 1406, 1407,
     1411, 1415, 1408, 1390, 1408, 1409, 1416, 1409, 1421, 1409,

     1420, 1422, 1420, 1408,    0, 1408, 1409, 1408, 1417, 1390,
     1417,    0, 1346, 1346, 1408, 1403, 1406, 1407, 1411, 1427,
     1415, 1390, 1410, 1410, 1410, 1416, 1421, 1430, 1428, 1422,
     1417, 1433, 1410, 1410, 1423, 1410, 1423, 1410, 1441, 1420,
     1346, 1425, 1435, 1425, 1410, 1451, 1434, 1427, 1434, 1440,
     1443, 1436, 1438, 1436, 1438, 1430, 1423, 1428, 1447, 1433,
     1449, 1458, 1409, 1425, 1457, 1459, 1441, 1420, 1460, 1462,
     1408, 1435, 1434, 1436, 1438, 1463, 1464, 1467, 1440, 1443,
     1474, 1465, 1469, 1470, 1468, 1451, 1447, 1457, 1458, 1449,
     1459, 1457, 1477, 1472, 1471, 1460, 1462, 1476, 1482, 1475,

     1410, 1434, 1478, 1464, 1465, 1485, 1470, 1474, 1483, 1486,
     1490,    0, 1463, 1451, 1467, 1497, 1457, 1458, 1472, 1459,
     1457, 1468, 1469, 1489, 1460, 1462, 1475, 1492, 1477, 1482,
        0, 1476, 1464, 1465, 1471, 1470, 1474, 1490, 1483, 1495,
     1463, 1496, 1467, 1500, 1478, 1485, 1497, 1472, 1504, 1468,
     1469, 1486, 1487, 1487, 1487, 1475, 1477, 1489, 1482, 1476,
     1506, 1509, 1471, 1492, 1496,    0, 1490, 1483, 1507, 1508,
     1510, 1500, 1478, 1485, 1487, 1497, 1516, 1504, 1519, 1486,
     1518, 1520, 1521, 1495, 1522, 1489, 1513, 1513, 1513, 1506,
     1509, 1492, 1529, 1496, 1513, 1510, 1507, 1508, 1514, 1514,

     1514, 1514, 1524, 1513, 1525, 1516, 1514, 1519, 1518, 1520,
     1521, 1495, 1522, 1526, 1527, 1514, 1528, 1515, 1515, 1515,
     1515, 1529, 1530, 1531, 1510, 1515, 1532, 1533, 1535,    0,
     1524, 1536, 1525, 1538, 1515, 1543,    0,    0, 1545, 1546,
        0, 1526, 1527, 1544, 1528, 1544, 1548, 1541, 1548, 1541,
     1530, 1531, 1551, 1553, 1554, 1532, 1533, 1535, 1541, 1536,
     1541, 1538, 1541, 1555, 1543, 1544, 1545, 1546, 1548, 1541,
     1552, 1556, 1552, 1557, 1558, 1560, 1559, 1562, 1563, 1564,
     1551, 1570, 1553, 1554, 1565, 1568, 1565, 1565, 1571, 1572,
     1573, 1555, 1552, 1566, 1574, 1566, 1566, 1576, 1578, 1556,

     1579, 1557, 1559, 1558, 1560, 1563, 1562, 1564, 1580, 1568,
     1577, 1582, 1584, 1581, 1590, 1571,    0, 1570, 1588, 1589,
     1589, 1589, 1593, 1596, 1601, 1541, 1573, 1579, 1595, 1572,
     1597, 1559,    0, 1578, 1563, 1602, 1564, 1574, 1568, 1576,
     1581, 1589, 1580, 1588, 1571, 1570, 1577, 1600, 1584, 1604,
     1615, 1590, 1601, 1582, 1573, 1595, 1579, 1572, 1594, 1594,
     1594, 1578, 1593, 1602, 1596, 1574, 1597, 1576, 1606, 1581,
     1580,    0, 1588, 1600, 1577, 1621, 1584, 1604, 1615, 1590,
     1594, 1582,    0,    0, 1595, 1598, 1598, 1598, 1598, 1607,
     1593, 1607, 1596, 1616, 1597, 1606,    0, 1619, 1609, 1607,

     1609, 1609, 1600, 1610, 1621, 1610, 1611, 1611, 1611, 1611,
     1612, 1612, 1612, 1620, 1611, 1623, 1625, 1624, 1612, 1626,
     1610, 1616, 1628, 1611, 1606, 1619, 1630, 1612, 1613, 1613,
     1613, 1613, 1614, 1614, 1614, 1633, 1613, 1629, 1637, 1629,
     1614, 1620, 1638, 1623, 1625, 1613, 1624, 1629, 1626, 1614,
     1635, 1628, 1635, 1640, 1630, 1640, 1643, 1644, 1645, 1647,
     1648, 1649, 1651, 1656, 1633, 1655, 1637, 1652, 1659, 1663,
     1660, 1638, 1635, 1661, 1653, 1640, 1653, 1653, 1664, 1677,
     1679, 1666, 1666, 1666, 1643, 1673, 1644, 1645, 1647, 1649,
     1652, 1666, 1668, 1670, 1674, 1680, 1694, 1648, 1651, 1655,

     1661, 1689, 1695, 1664, 1675, 1656, 1675, 1663, 1679, 1696,
     1659, 1660, 1673, 1697, 1675, 1740, 1677, 1668, 1670, 1652,
     1702, 1674,    0, 1680, 1694, 1648, 1651, 1655,    0, 1661,
     1689, 1695, 1664, 1656,    0, 1663,    0, 1696, 1659, 1660,
        0, 1673, 1697, 1740, 1677, 1703, 1668, 1670,    0, 1702,
     1674, 1683, 1683, 1683, 1683, 1685, 1685, 1685, 1685, 1683,
     1704, 1713, 1683, 1685, 1686, 1686, 1686, 1687, 1687, 1687,
     1687, 1705, 1685, 1711, 1703, 1687, 1690, 1698, 1690, 1698,
     1699, 1686, 1699, 1712, 1687, 1718, 1690, 1698, 1713, 1704,
     1699, 1714, 1714, 1714, 1719, 1720, 1721, 1724, 1741, 1711,

     1705, 1714,    0, 1800, 1724, 1800, 1727, 1724, 1712, 1725,
     1725, 1725, 1725, 1727, 1800,    0, 1727, 1713, 1743, 1718,
     1742, 1725, 1720, 1721, 1738, 1719, 1738, 1741, 1711, 1733,
     1726, 1726, 1726, 1726, 1738, 1725, 1733, 1712, 1726, 1733,
        0, 1726, 1731, 1731, 1731, 1731, 1743, 1718, 1748, 1742,
     1731, 1720, 1721, 1731, 1719, 1732, 1732, 1732, 1732, 1734,
     1734, 1734, 1734, 1732, 1725, 1745, 1732, 1734, 1746, 1735,
     1735, 1735, 1735, 1749, 1754, 1751, 1734, 1735, 1739, 1753,
     1739, 1735, 1756,    0, 1748, 1757, 1735, 1757, 1757,    0,
     1765, 1739, 1771, 1739, 1745, 1735, 1759, 1746, 1759, 1759,

     1739, 1749, 1751, 1764, 1766, 1772, 1753, 1774, 1756,    0,
     1764, 1766, 1748, 1764, 1766, 1754, 1758, 1758, 1758, 1758,
     1768, 1771, 1777, 1781, 1735,    0, 1765, 1768, 1758,    0,
     1768, 1751, 1773, 1772, 1775, 1753, 1774, 1756, 1762, 1762,
     1762, 1762, 1758, 1754,    0, 1776, 1762, 1783, 1779, 1762,
     1763, 1763, 1763, 1763, 1765, 1769, 1778, 1769, 1763, 1777,
     1773, 1763, 1763, 1775, 1767, 1767, 1767, 1767, 1769, 1781,
     1769, 1758, 1767, 1776, 1779, 1767, 1763, 1769, 1770, 1782,
     1770,    0, 1803, 1784, 1778, 1784, 1784, 1777,    0, 1770,
     1783, 1770, 1785, 1770, 1785, 1785, 1802, 1781,    0, 1796,

     1770, 1805,    0, 1779,    0, 1763, 1788, 1788, 1788, 1788,
     1782, 1803,    0,    0, 1788,    0, 1791, 1788, 1783, 1789,
     1789, 1789, 1789, 1791, 1802, 1810, 1791, 1789,    0, 1805,
     1789, 1789, 1790, 1790, 1790, 1790, 1796, 1792,    0, 1782,
     1790, 1793, 1808, 1790, 1792, 1789, 1814, 1792, 1793, 1795,
     1806, 1793, 1794, 1794, 1794, 1794, 1795, 1815, 1807, 1795,
     1794, 1809, 1810, 1794, 1796, 1797, 1797, 1797, 1797, 1798,
     1798, 1798, 1798, 1797, 1789, 1799, 1797, 1798, 1806,    0,
     1798,    0, 1799, 1814, 1808, 1799, 1807, 1832,    0, 1809,
     1810, 1816,    0, 1816, 1816,    0, 1817, 1815, 1817, 1817,

        0, 1833, 1818, 1818, 1818, 1818, 1819, 1819, 1819, 1819,
     1818, 1814, 1808, 1818, 1819, 1832, 1820, 1819,    0, 1821,
     1821, 1821, 1821, 1820, 1822, 1815, 1820, 1821, 1823, 1833,
     1821, 1822, 1830, 1839, 1822, 1823,    0,    0, 1823, 1824,
     1824, 1824, 1824, 1826, 1825, 1825, 1825, 1825, 1840, 1834,
     1826, 1824, 1825, 1826, 1835, 1825, 1827, 1827, 1827, 1827,
     1828, 1828, 1828, 1828, 1827, 1824, 1829, 1827, 1828, 1839,
     1853, 1828, 1828, 1829, 1830, 1840, 1829, 1834, 1841,    0,
     1841, 1841, 1842, 1835, 1842, 1842, 1828, 1843, 1843, 1843,
     1843, 1851,    0, 1847, 1824, 1843, 1855, 1839, 1843, 1853,

     1847,    0, 1830, 1847, 1840, 1844, 1844, 1844, 1844, 1845,
     1845, 1845, 1845, 1844, 1857, 1828, 1844, 1845, 1849, 1851,
     1845, 1846, 1846, 1846, 1846, 1849, 1856, 1867, 1849, 1846,
     1870,    0, 1846, 1846, 1848, 1848, 1848, 1848, 1855, 1864,
        0, 1866, 1848,    0, 1871, 1848, 1864, 1846, 1866, 1864,
     1857, 1866,    0,    0, 1856, 1872, 1862, 1862, 1862, 1862,
     1863, 1863, 1863, 1863, 1862,    0, 1855, 1862, 1863, 1867,
     1870, 1863, 1871, 1878,    0,    0, 1846, 1888, 1857, 1861,
     1861, 1861, 1861, 1873, 1861, 1873, 1873, 1861, 1880, 1861,
     1861, 1861, 1872, 1889, 1861, 1861, 1879, 1867, 1870, 1861,

     1888, 1861, 1861, 1861, 1865, 1865, 1865, 1865, 1874, 1874,
     1874, 1874, 1865, 1878, 1877, 1865, 1874, 1880,    0, 1874,
     1872, 1877, 1889, 1879, 1877, 1876, 1876, 1876, 1876, 1888,
     1861, 1861, 1861, 1876, 1881, 1882, 1876, 1882, 1882, 1884,
        0, 1878, 1883, 1883, 1883, 1883, 1884, 1885,    0, 1884,
     1883, 1887, 1879, 1883, 1885, 1893, 1895, 1885, 1861, 1875,
     1875, 1875, 1875, 1875, 1875, 1894, 1896, 1875, 1875, 1875,
     1875, 1875,    0,    0, 1875, 1875, 1881, 1892, 1887, 1875,
        0, 1875, 1875, 1875, 1886, 1886, 1886, 1886, 1891, 1891,
     1891, 1891, 1886, 1894, 1899, 1886, 1891, 1893, 1895, 1891,

     1892, 1907,    0,    0, 1881,    0,    0, 1887, 1896,    0,
     1875, 1875, 1875, 1897, 1897, 1897, 1897, 1898, 1898, 1898,
     1898, 1900, 1900, 1900, 1900, 1893, 1895,    0, 1907, 1892,
     1901, 1901, 1901, 1901, 1899,    0, 1896,    0, 1875, 1902,
     1902, 1902, 1902, 1904, 1904, 1904, 1904, 1905, 1905, 1905,
     1905, 1908, 1908, 1908, 1908, 1923,    0, 1907, 1909, 1909,
     1909, 1909, 1899, 1911, 1911, 1911, 1911, 1912, 1912, 1912,
     1912, 1915, 1915, 1915, 1915, 1916, 1916, 1916, 1916, 1918,
     1918, 1918, 1918, 1919, 1921, 1921, 1921, 1921, 1924, 1924,
     1924, 1924, 1925, 1925, 1925, 1925,    0, 1923, 1926, 1926,

     1926, 1926, 1928, 1928, 1928, 1928, 1919, 1929, 1929, 1929,
     1929, 1932, 1932, 1932, 1932, 1934, 1934, 1934, 1934,    0,
        0,    0,    0,    0,    0, 1923,    0,    0,    0,    0,
        0,    0,    0,    0,    0, 1919, 1936, 1936, 1936, 1936,
     1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936,
     1936, 1936, 1936, 1936, 1937, 1937, 1937, 1937, 1937, 1937,
     1937, 1937, 1937, 1937, 1937, 1937, 1937, 1937, 1937, 1937,
     1937, 1937, 1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938,
     1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938,
     1939, 1939,    0, 1939, 1939, 1939, 1939, 1939, 1939, 1939,

     1939, 1939, 1939, 1939, 1939, 1939, 1939, 1939, 1940, 1940,
     1940, 1940, 1940, 1940, 1940, 1940, 1940, 1940, 1940, 1940,
     1940, 1940, 1940, 1940, 1940, 1940, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1942,    0,    0,    0,    0,    0,
        0, 1942,    0, 1942,    0, 1942, 1942, 1942, 1942, 1942,
     1943, 1943, 1943, 1943, 1943, 1944, 1944, 1944, 1944, 1944,
     1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944,
     1944, 1944, 1944, 1945, 1945, 1945, 1945, 1945, 1945, 1945,
     1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945,

     1945, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946,
     1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1948,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0, 1948, 1948,
     1948, 1948, 1948, 1949, 1949, 1949, 1949, 1949, 1949, 1949,
     1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949,
     1949, 1950, 1950,    0, 1950, 1950, 1950, 1950, 1950, 1950,
     1950, 1950, 1950, 1950, 1950, 1950, 1950, 1950, 1950, 1951,
     1951, 1951, 1951, 1951, 1951, 1951, 1951, 1951, 1951, 1951,

     1951, 1951, 1951, 1951, 1951, 1951, 1951, 1952, 1952, 1952,
     1952, 1952, 1952, 1952, 1952, 1952, 1952, 1952, 1952, 1952,
     1952, 1952, 1952, 1952, 1952, 1953, 1953, 1953, 1953, 1953,
     1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953,
     1953, 1953, 1953, 1954, 1954, 1954, 1954, 1954, 1954, 1954,
     1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954,
     1954, 1955,    0,    0,    0,    0,    0,    0, 1955,    0,
     1955,    0,    0, 1955, 1955, 1955, 1955, 1956, 1956, 1956,
     1956,    0, 1956, 1956, 1956, 1956, 1956, 1956,    0, 1956,
     1956,    0,    0, 1956, 1956, 1957, 1957, 1957, 1957, 1957,

     1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959,
     1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959, 1960, 1960,
     1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960,
     1960, 1960, 1960, 1960, 1960, 1960, 1961, 1961, 1961, 1961,
     1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961,
     1961, 1961, 1961, 1961, 1962, 1962, 1962, 1962, 1962, 1962,
     1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962,
     1962, 1962, 1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963,
     1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963,
     1964, 1964, 1964, 1964, 1964, 1964, 1964, 1964, 1964, 1964,

     1964, 1964, 1964, 1964, 1964, 1964, 1964, 1964, 1965, 1965,
     1965, 1965, 1965, 1965, 1965, 1965, 1965, 1965, 1965, 1965,
     1965, 1965, 1965, 1965, 1965, 1965, 1966, 1966, 1966, 1966,
     1966, 1966, 1966, 1966, 1966, 1966, 1966, 1966, 1966, 1966,
     1966, 1966, 1966, 1966, 1967, 1967, 1967, 1967, 1967, 1967,
     1967, 1967, 1967, 1967, 1967, 1967, 1967, 1967, 1967, 1967,
     1967, 1967, 1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968,
     1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968,
     1969, 1969,    0, 1969, 1969, 1969, 1969, 1969, 1969, 1969,
     1969, 1969, 1969, 1969, 1969, 1969, 1969, 1969, 1970, 1970,

     1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970,
     1970, 1970, 1970, 1970, 1970, 1970, 1971, 1971, 1971, 1971,
     1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971,
     1971, 1971, 1971, 1971, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973,
     1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973,
     1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974,
     1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1975,    0,
        0,    0,    0,    0,    0, 1975,    0, 1975,    0,    0,

     1975, 1975, 1975, 1975, 1976,    0,    0,    0,    0,    0,
        0,    0, 1976,    0, 1976,    0, 1976, 1976, 1976, 1976,
     1976, 1977, 1977, 1977, 1977, 1978, 1978, 1978, 1978, 1978,
     1978, 1978, 1978, 1978, 1978, 1978, 1978, 1978, 1978, 1978,
     1978, 1978, 1978, 1979, 1979, 1979, 1979, 1979, 1979, 1979,
     1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979,
     1979, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,
     1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1981,
     1981, 1981, 1981,    0, 1981, 1981, 1981, 1981, 1981, 1981,
        0, 1981, 1981,    0,    0, 1981, 1981, 1982, 1982, 1982,

     1982, 1982, 1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983,
     1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983,
     1984,    0,    0,    0,    0,    0,    0,    0, 1984, 1984,
     1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985,
     1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985, 1986, 1986,
     1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986,
     1986, 1986, 1986, 1986, 1986, 1986, 1987, 1987, 1987, 1987,
     1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987,
     1987, 1987, 1987, 1987, 1988, 1988, 1988, 1988, 1988, 1988,
     1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988,

     1988, 1988, 1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989,
     1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989,
     1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990,
     1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1991, 1991,
     1991, 1991, 1991, 1991, 1991, 1991, 1991, 1991, 1991, 1991,
     1991, 1991, 1991, 1991, 1991, 1991, 1992, 1992, 1992, 1992,
     1992, 1992, 1992, 1992, 1992, 1992, 1992, 1992, 1992, 1992,
     1992, 1992, 1992, 1992, 1993, 1993, 1993, 1993, 1993, 1993,
     1993, 1993, 1993, 1993, 1993, 1993, 1993, 1993, 1993, 1993,
     1993, 1993, 1994,    0,    0,    0,    0,    0,    0,    0,

        0,    0,    0, 1994, 1994, 1994, 1994, 1994, 1995, 1995,
     1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995,
     1995, 1995, 1995, 1995, 1995, 1995, 1996, 1996, 1996, 1996,
     1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996,
     1996, 1996, 1996, 1996, 1997, 1997, 1997, 1997, 1997, 1997,
     1997, 1997, 1997, 1997, 1997, 1997, 1997, 1997, 1997, 1997,
     1997, 1997, 1998, 1998,    0, 1998, 1998, 1998, 1998, 1998,
     1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998,
     1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999,
     1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 2000, 2000,

     2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000,
     2000, 2000, 2000, 2000, 2000, 2000, 2001, 2001, 2001, 2001,
     2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001,
     2001, 2001, 2001, 2001, 2002, 2002, 2002, 2002, 2002, 2002,
     2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002,
     2002, 2002, 2003,    0,    0,    0,    0,    0,    0, 2003,
        0, 2003,    0,    0, 2003, 2003, 2003, 2003, 2004,    0,
        0,    0,    0,    0,    0,    0, 2004,    0,    0,    0,
     2004, 2004, 2004, 2004, 2004, 2005,    0,    0,    0,    0,
        0,    0,    0, 2005,    0, 2005,    0, 2005, 2005, 2005,

     2005, 2005, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006,
     2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006,
     2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007,
     2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2008, 2008,
     2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008,
     2008, 2008, 2008, 2008, 2008, 2008, 2009, 2009, 2009, 2009,
     2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009,
     2009, 2009, 2009, 2009, 2010, 2010, 2010, 2010, 2010, 2011,
     2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011,
     2011, 2011, 2011, 2011, 2011, 2011, 2011, 2012, 2012, 2012,

     2012, 2012, 2012,    0, 2012, 2012, 2012, 2012, 2012, 2012,
     2012, 2012, 2012, 2012, 2012, 2013, 2013,    0, 2013, 2013,
     2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013,
     2013, 2013, 2013, 2014, 2014, 2014, 2014, 2014, 2014, 2014,
     2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014,
     2014, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015,
     2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2016,
     2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016,
     2016, 2016, 2016, 2016, 2016, 2016, 2016, 2017, 2017, 2017,
     2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017,

     2017, 2017, 2017, 2017, 2017, 2018, 2018, 2018, 2018, 2018,
     2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
     2018, 2018, 2018, 2019, 2019, 2019, 2019, 2019, 2019, 2019,
     2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019,
     2019, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020,
     2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2021,
     2021,    0, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021,
     2021, 2021, 2021, 2021, 2021, 2021, 2021, 2022, 2022,    0,
     2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022,
     2022, 2022, 2022, 2022, 2022, 2023, 2023,    0, 2023, 2023,

     2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023,
     2023, 2023, 2023, 2024, 2024, 2024, 2024, 2024, 2024, 2024,
     2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024,
     2024, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025,
     2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2026,
     2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026,
     2026, 2026, 2026, 2026, 2026, 2026, 2026, 2027, 2027, 2027,
     2027, 2027, 2027, 2027, 2027, 2027, 2027, 2027, 2027, 2027,
     2027, 2027, 2027, 2027, 2027, 2028,    0,    0,    0,    0,
        0, 2028,    0,    0,    0, 2028,    0, 2028, 2028, 2028,

     2028, 2028, 2029, 2029, 2029, 2029, 2030,    0,    0,    0,
        0,    0,    0,    0, 2030,    0,    0,    0, 2030, 2030,
     2030, 2030, 2030, 2031,    0,    0,    0,    0,    0,    0,
        0, 2031,    0, 2031,    0, 2031, 2031, 2031, 2031, 2031,
     2032, 2032,    0, 2032, 2032, 2032, 2032, 2032, 2032, 2032,
     2032, 2032, 2032, 2032, 2032, 2032, 2032, 2032, 2033, 2033,
     2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033,
     2033, 2033, 2033, 2033, 2033, 2033, 2034, 2034,    0, 2034,
     2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034,
     2034, 2034, 2034, 2034, 2035, 2035, 2035, 2035, 2035, 2035,

        0, 2035, 2035, 2035, 2035, 2035, 2035, 2035, 2035, 2035,
     2035, 2035, 2036, 2036,    0, 2036, 2036, 2036, 2036, 2036,
     2036, 2036, 2036, 2036, 2036, 2036, 2036, 2036, 2036, 2036,
     2037, 2037, 2037, 2037, 2037, 2037, 2037, 2037, 2037, 2037,
     2037, 2037, 2037, 2037, 2037, 2037, 2037, 2037, 2038, 2038,
     2038, 2038, 2038, 2038, 2038, 2038, 2038, 2038, 2038, 2038,
     2038, 2038, 2038, 2038, 2038, 2038, 2039, 2039, 2039, 2039,
     2039, 2039, 2039, 2039, 2039, 2039, 2039, 2039, 2039, 2039,
     2039, 2039, 2039, 2039, 2040, 2040, 2040, 2040, 2040, 2040,
     2040, 2040, 2040, 2040, 2040, 2040, 2040, 2040, 2040, 2040,

     2040, 2040, 2041, 2041, 2041, 2041, 2041, 2041, 2041, 2041,
     2041, 2041, 2041, 2041, 2041, 2041, 2041, 2041, 2041, 2041,
     2042, 2042, 2042, 2042, 2042, 2042, 2042, 2042, 2042, 2042,
     2042, 2042, 2042, 2042, 2042, 2042, 2042, 2042, 2043, 2043,
     2043, 2043, 2043, 2043, 2043, 2043, 2043, 2043, 2043, 2043,
     2043, 2043, 2043, 2043, 2043, 2043, 2044, 2044, 2044, 2044,
     2044, 2044, 2044, 2044, 2044, 2044, 2044, 2044, 2044, 2044,
     2044, 2044, 2044, 2044, 2045, 2045, 2045, 2045, 2045, 2045,
     2045, 2045, 2045, 2045, 2045, 2045, 2045, 2045, 2045, 2045,
     2045, 2045, 2046, 2046, 2046, 2046, 2046, 2046, 2046, 2046,

     2046, 2046, 2046, 2046, 2046, 2046, 2046, 2046, 2046, 2046,
     2047, 2047, 2047, 2047, 2047, 2047, 2047, 2047, 2047, 2047,
     2047, 2047, 2047, 2047, 2047, 2047, 2047, 2047, 2048, 2048,
        0, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048,
     2048, 2048, 2048, 2048, 2048, 2048, 2049, 2049,    0, 2049,
     2049, 2049, 2049, 2049, 2049, 2049, 2049, 2049, 2049, 2049,
     2049, 2049, 2049, 2049, 2050, 2050,    0, 2050, 2050, 2050,
     2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050,
     2050, 2050, 2051, 2051, 2051, 2051, 2051, 2051, 2051, 2051,
     2051, 2051, 2051, 2051, 2051, 2051, 2051, 2051, 2051, 2051,

     2052, 2052, 2052, 2052, 2052, 2052, 2052, 2052, 2052, 2052,
     2052, 2052, 2052, 2052, 2052, 2052, 2052, 2052, 2053, 2053,
     2053, 2053, 2053, 2053, 2053, 2053, 2053, 2053, 2053, 2053,
     2053, 2053, 2053, 2053, 2053, 2053, 2054, 2054, 2054, 2054,
     2054, 2054, 2054, 2054, 2054, 2054, 2054, 2054, 2054, 2054,
     2054, 2054, 2054, 2054, 2055,    0,    0,    0,    0,    0,
     2055,    0,    0,    0,    0,    0, 2055, 2055, 2055, 2055,
     2055, 2056, 2056,    0, 2056, 2056, 2056, 2056, 2056, 2056,
     2056, 2056, 2056, 2056, 2056, 2056, 2056, 2056, 2056, 2057,
        0,    0,    0,    0,    0,    0, 2057,    0, 2057,    0,

        0, 2057, 2057, 2057, 2057, 2058,    0,    0,    0,    0,
        0,    0,    0, 2058,    0, 2058,    0, 2058, 2058, 2058,
     2058, 2058, 2059, 2059, 2059, 2059, 2060, 2060,    0, 2060,
     2060, 2060, 2060, 2060, 2060, 2060, 2060, 2060, 2060, 2060,
     2060, 2060, 2060, 2060, 2061, 2061, 2061, 2061, 2061, 2061,
        0, 2061, 2061, 2061, 2061, 2061, 2061, 2061, 2061, 2061,
     2061, 2061, 2062, 2062,    0, 2062, 2062, 2062, 2062, 2062,
     2062, 2062, 2062, 2062, 2062, 2062, 2062, 2062, 2062, 2062,
     2063, 2063,    0, 2063, 2063, 2063, 2063, 2063, 2063, 2063,
     2063, 2063, 2063, 2063, 2063, 2063, 2063, 2063, 2064, 2064,

     2064, 2064, 2064, 2064, 2064, 2064, 2064, 2064, 2064, 2064,
     2064, 2064, 2064, 2064, 2064, 2064, 2065, 2065, 2065, 2065,
     2065, 2065, 2065, 2065, 2065, 2065, 2065, 2065, 2065, 2065,
     2065, 2065, 2065, 2065, 2066, 2066, 2066, 2066, 2066, 2066,
     2066, 2066, 2066, 2066, 2066, 2066, 2066, 2066, 2066, 2066,
     2066, 2066, 2067, 2067, 2067, 2067, 2067, 2067, 2067, 2067,
     2067, 2067, 2067, 2067, 2067, 2067, 2067, 2067, 2067, 2067,
     2068, 2068, 2068, 2068, 2068, 2068, 2068, 2068, 2068, 2068,
     2068, 2068, 2068, 2068, 2068, 2068, 2068, 2068, 2069,    0,
     2069,    0,    0,    0,    0, 2069,    0,    0, 2069, 2069,

     2069, 2069, 2069, 2069, 2070, 2070, 2070, 2070, 2070, 2070,
     2070, 2070, 2070, 2070, 2070, 2070, 2070, 2070, 2070, 2070,
     2070, 2070, 2071, 2071, 2071, 2071, 2071, 2071, 2071, 2071,
     2071, 2071, 2071, 2071, 2071, 2071, 2071, 2071, 2071, 2071,
     2072, 2072, 2072, 2072, 2072, 2072, 2072, 2072, 2072, 2072,
     2072, 2072, 2072, 2072, 2072, 2072, 2072, 2072, 2073, 2073,
     2073, 2073, 2073, 2073, 2073, 2073, 2073, 2073, 2073, 2073,
     2073, 2073, 2073, 2073, 2073, 2073, 2074, 2074, 2074, 2074,
     2074, 2074, 2074, 2074, 2074, 2074, 2074, 2074, 2074, 2074,
     2074, 2074, 2074, 2074, 2075, 2075,    0, 2075, 2075, 2075,

     2075, 2075, 2075, 2075, 2075, 2075, 2075, 2075, 2075, 2075,
     2075, 2075, 2076, 2076, 2076, 2076, 2076, 2076, 2076, 2076,
     2076, 2076, 2076, 2076, 2076, 2076, 2076, 2076, 2076, 2076,
     2077, 2077, 2077, 2077, 2077, 2077, 2077, 2077, 2077, 2077,
     2077, 2077, 2077, 2077, 2077, 2077, 2077, 2077, 2078,    0,
        0,    0,    0,    0, 2078,    0,    0,    0,    0,    0,
     2078, 2078, 2078, 2078, 2078, 2079,    0,    0,    0,    0,
        0,    0, 2079,    0, 2079,    0,    0, 2079, 2079, 2079,
     2079, 2080,    0,    0,    0,    0,    0,    0,    0, 2080,
        0, 2080,    0, 2080, 2080, 2080, 2080, 2080, 2081, 2081,

     2081, 2081, 2082,    0, 2082,    0,    0,    0,    0, 2082,
        0,    0, 2082, 2082, 2082, 2082, 2082, 2082, 2083,    0,
     2083,    0,    0,    0,    0, 2083,    0,    0, 2083, 2083,
     2083, 2083, 2083, 2083, 2084, 2084, 2084, 2084, 2084, 2084,
     2084, 2084, 2084, 2084, 2084, 2084, 2084, 2084, 2084, 2084,
     2084, 2084, 2085, 2085, 2085, 2085, 2085, 2086, 2086,    0,
     2086, 2086, 2086, 2086, 2086, 2086, 2086, 2086, 2086, 2086,
     2086, 2086, 2086, 2086, 2086, 2087, 2087, 2087, 2087, 2087,
     2087, 2087, 2087, 2087, 2087, 2087, 2087, 2087, 2087, 2087,
     2087, 2087, 2087, 2088, 2088, 2088, 2088, 2088, 2088, 2088,

     2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088, 2088,
     2088, 2089, 2089, 2089, 2089, 2089, 2089, 2089, 2089, 2089,
     2089, 2089, 2089, 2089, 2089, 2089, 2089, 2089, 2089, 2090,
     2090, 2090, 2090, 2090, 2090, 2090, 2090, 2090, 2090, 2090,
     2090, 2090, 2090, 2090, 2090, 2090, 2090, 2091, 2091, 2091,
     2091, 2091, 2091, 2091, 2091, 2091, 2091, 2091, 2091, 2091,
     2091, 2091, 2091, 2091, 2091, 2092, 2092, 2092, 2092, 2092,
     2092, 2092, 2092, 2092, 2092, 2092, 2092, 2092, 2092, 2092,
     2092, 2092, 2092, 2093, 2093, 2093, 2093, 2093, 2093, 2093,
     2093, 2093, 2093, 2093, 2093, 2093, 2093, 2093, 2093, 2093,

     2093, 2094, 2094, 2094, 2094, 2094, 2094, 2094, 2094, 2094,
     2094, 2094, 2094, 2094, 2094, 2094, 2094, 2094, 2094, 2095,
     2095, 2095, 2095, 2095, 2095, 2095, 2095, 2095, 2095, 2095,
     2095, 2095, 2095, 2095, 2095, 2095, 2095, 2096, 2096, 2096,
     2096, 2096, 2096, 2096, 2096, 2096, 2096, 2096, 2096, 2096,
     2096, 2096, 2096, 2096, 2096, 2097, 2097, 2097, 2097, 2097,
     2097, 2097, 2097, 2097, 2097, 2097, 2097, 2097, 2097, 2097,
     2097, 2097, 2097, 2098, 2098, 2098, 2098, 2098, 2098, 2098,
     2098, 2098, 2098, 2098, 2098, 2098, 2098, 2098, 2098, 2098,
     2098, 2099, 2099, 2099, 2099, 2099, 2099, 2099, 2099, 2099,

     2099, 2099, 2099, 2099, 2099, 2099, 2099, 2099, 2099, 2100,
     2100, 2100, 2100, 2100, 2100, 2100, 2100, 2100, 2100, 2100,
     2100, 2100, 2100, 2100, 2100, 2100, 2100, 2101, 2101, 2101,
     2101, 2101, 2101, 2101, 2101, 2101, 2101, 2101, 2101, 2101,
     2101, 2101, 2101, 2101, 2101, 2102, 2102, 2102, 2102, 2102,
     2102, 2102, 2102, 2102, 2102, 2102, 2102, 2102, 2102, 2102,
     2102, 2102, 2102, 2103, 2103, 2103, 2103, 2103, 2103, 2103,
     2103, 2103, 2103, 2103, 2103, 2103, 2103, 2103, 2103, 2103,
     2103, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,

     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935
    } ;

extern int fortran__flex_debug;
int fortran__flex_debug = 0;

static yy_state_type *yy_state_buf=0, *yy_state_ptr=0;
static char *yy_full_match;
static int yy_lp;
static int yy_looking_for_trail_begin = 0;
static int yy_full_lp;
static int *yy_full_state;
#define YY_TRAILING_MASK 0x2000
#define YY_TRAILING_HEAD_MASK 0x4000
#define REJECT \
{ \
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */ \
yy_cp = (yy_full_match); /* restore poss. backed-over text */ \
(yy_lp) = (yy_full_lp); /* restore orig. accepting pos. */ \
(yy_state_ptr) = (yy_full_state); /* restore orig. state */ \
yy_current_state = *(yy_state_ptr); /* restore curr. state */ \
++(yy_lp); \
goto find_rule; \
}

#define yymore() yymore_used_but_not_detected
#define YY_MORE_ADJ 0
#define YY_RESTORE_YY_MORE_OFFSET
char *fortran_text;
#line 1 "fortran.lex"
/******************************************************************************/
/*                                                                            */
/*     CONV (converter) for Agrif (Adaptive Grid Refinement In Fortran)       */
/*                                                                            */
/* Copyright or   or Copr. Laurent Debreu (Laurent.Debreu@imag.fr)            */
/*                        Cyril Mazauric (Cyril_Mazauric@yahoo.fr)            */
/* This software is governed by the CeCILL-C license under French law and     */
/* abiding by the rules of distribution of free software.  You can  use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-C   */
/* license as circulated by CEA, CNRS and INRIA at the following URL          */
/* "http://www.cecill.info".                                                  */
/*                                                                            */
/* As a counterpart to the access to the source code and  rights to copy,     */
/* modify and redistribute granted by the license, users are provided only    */
/* with a limited warranty  and the software's author,  the holder of the     */
/* economic rights,  and the successive licensors  have only  limited         */
/* liability.                                                                 */
/*                                                                            */
/* In this respect, the user's attention is drawn to the risks associated     */
/* with loading,  using,  modifying and/or developing or reproducing the      */
/* software by the user in light of its specific status of free software,     */
/* that may mean  that it is complicated to manipulate,  and  that  also      */
/* therefore means  that it is reserved for developers  and  experienced      */
/* professionals having in-depth computer knowledge. Users are therefore      */
/* encouraged to load and test the software's suitability as regards their    */
/* requirements in conditions enabling the security of their systems and/or   */
/* data to be ensured and,  more generally, to use and operate it in the      */
/* same conditions as regards security.                                       */
/*                                                                            */
/* The fact that you are presently reading this means that you have had       */
/* knowledge of the CeCILL-C license and that you accept its terms.           */
/******************************************************************************/
/* version 1.7                                                                */
/******************************************************************************/







#line 46 "fortran.lex"
#include <math.h>
#include <stdlib.h>
#include <string.h>
extern FILE * fortran_in;
#define MAX_INCLUDE_DEPTH 30
#define YY_BUF_SIZE 64000
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int line_num_input = 0;
int newlinef90 = 0;
int tmpc;

int lastwasendofstmt = 1;

extern char linebuf1[1024];
extern char linebuf2[1024];

int count_newlines(const char* str_in)
{
    int k, i = 0;
    for( k=0 ; k<strlen(str_in) ; k++)
        if (str_in[k] == '\n') i++;
    return i;
}

#define PRINT_LINE_NUM()   //  { fprintf(stderr,"== Parsing l.%4d...\n", line_num_input); }
#define INCREMENT_LINE_NUM() { line_num_input+=count_newlines(fortran_text) ; PRINT_LINE_NUM(); }
#define YY_USER_ACTION       { if (increment_nbtokens !=0) token_since_endofstmt++; increment_nbtokens = 1; if (token_since_endofstmt>=1) lastwasendofstmt=0; /*printf("VALLIJSDFLSD = %d %d %s \n",lastwasendofstmt,token_since_endofstmt,fortran_text); */ if (firstpass) { strcpy(linebuf1, linebuf2); strncpy(linebuf2, fortran_text,80);} \
                               else {my_position_before=setposcur();/*printf("muposition = %d\n",my_position_before);*/ECHO;} }
#define YY_BREAK {/*printf("VALL = %d %d\n",lastwasendofstmt,token_since_endofstmt);*/if (token_since_endofstmt>=1) lastwasendofstmt=0; break;}

void out_of_donottreat(void);

#line 3571 "fortran.yy.c"

#define INITIAL 0
#define parameter 1
#define character 2
#define donottreat 3
#define donottreat_interface 4
#define includestate 5
#define fortran77style 6
#define fortran90style 7

#ifndef YY_NO_UNISTD_H
/* Special case for "unistd.h", since it is non-ANSI. We include it way
 * down here because we want the user's section 1 to have been scanned first.
 * The user has a chance to override it with an option.
 */
#include <unistd.h>
#endif

#ifndef YY_EXTRA_TYPE
#define YY_EXTRA_TYPE void *
#endif

static int yy_init_globals (void );

/* Accessor methods to globals.
   These are made visible to non-reentrant scanners for convenience. */

int fortran_lex_destroy (void );

int fortran_get_debug (void );

void fortran_set_debug (int debug_flag  );

YY_EXTRA_TYPE fortran_get_extra (void );

void fortran_set_extra (YY_EXTRA_TYPE user_defined  );

FILE *fortran_get_in (void );

void fortran_set_in  (FILE * _in_str  );

FILE *fortran_get_out (void );

void fortran_set_out  (FILE * _out_str  );

			int fortran_get_leng (void );

char *fortran_get_text (void );

int fortran_get_lineno (void );

void fortran_set_lineno (int _line_number  );

/* Macros after this point can all be overridden by user definitions in
 * section 1.
 */

#ifndef YY_SKIP_YYWRAP
#ifdef __cplusplus
extern "C" int fortran_wrap (void );
#else
extern int fortran_wrap (void );
#endif
#endif

#ifndef YY_NO_UNPUT
    
    static void yyunput (int c,char *buf_ptr  );
    
#endif

#ifndef yytext_ptr
static void yy_flex_strncpy (char *,yyconst char *,int );
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (yyconst char * );
#endif

#ifndef YY_NO_INPUT

#ifdef __cplusplus
static int yyinput (void );
#else
static int input (void );
#endif

#endif

/* Amount of stuff to slurp up with each read. */
#ifndef YY_READ_BUF_SIZE
#ifdef __ia64__
/* On IA-64, the buffer size is 16k, not 8k */
#define YY_READ_BUF_SIZE 16384
#else
#define YY_READ_BUF_SIZE 8192
#endif /* __ia64__ */
#endif

/* Copy whatever the last rule matched to the standard output. */
#ifndef ECHO
/* This used to be an fputs(), but since the string might contain NUL's,
 * we now use fwrite().
 */
#define ECHO do { if (fwrite( fortran_text, (size_t) fortran_leng, 1, fortran_out )) {} } while (0)
#endif

/* Gets input and stuffs it into "buf".  number of characters read, or YY_NULL,
 * is returned in "result".
 */
#ifndef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( YY_CURRENT_BUFFER_LVALUE->yy_is_interactive ) \
		{ \
		int c = '*'; \
		int n; \
		for ( n = 0; n < max_size && \
			     (c = getc( fortran_in )) != EOF && c != '\n'; ++n ) \
			buf[n] = (char) c; \
		if ( c == '\n' ) \
			buf[n++] = (char) c; \
		if ( c == EOF && ferror( fortran_in ) ) \
			YY_FATAL_ERROR( "input in flex scanner failed" ); \
		result = n; \
		} \
	else \
		{ \
		errno=0; \
		while ( (result = (int) fread(buf, 1, (yy_size_t) max_size, fortran_in)) == 0 && ferror(fortran_in)) \
			{ \
			if( errno != EINTR) \
				{ \
				YY_FATAL_ERROR( "input in flex scanner failed" ); \
				break; \
				} \
			errno=0; \
			clearerr(fortran_in); \
			} \
		}\
\

#endif

/* No semi-colon after return; correct usage is to write "yyterminate();" -
 * we don't want an extra ';' after the "return" because that will cause
 * some compilers to complain about unreachable statements.
 */
#ifndef yyterminate
#define yyterminate() return YY_NULL
#endif

/* Number of entries by which start-condition stack grows. */
#ifndef YY_START_STACK_INCR
#define YY_START_STACK_INCR 25
#endif

/* Report a fatal error. */
#ifndef YY_FATAL_ERROR
#define YY_FATAL_ERROR(msg) yy_fatal_error( msg )
#endif

/* end tables serialization structures and prototypes */

/* Default declaration of generated scanner - a define so the user can
 * easily add parameters.
 */
#ifndef YY_DECL
#define YY_DECL_IS_OURS 1

extern int fortran_lex (void);

#define YY_DECL int fortran_lex (void)
#endif /* !YY_DECL */

/* Code executed at the beginning of each rule, after fortran_text and fortran_leng
 * have been set up.
 */
#ifndef YY_USER_ACTION
#define YY_USER_ACTION
#endif

/* Code executed at the end of each rule. */
#ifndef YY_BREAK
#define YY_BREAK /*LINTED*/break;
#endif

#define YY_RULE_SETUP \
	if ( fortran_leng > 0 ) \
		YY_CURRENT_BUFFER_LVALUE->yy_at_bol = \
				(fortran_text[fortran_leng - 1] == '\n'); \
	YY_USER_ACTION

/** The main scanner function which does all the work.
 */
YY_DECL
{
	yy_state_type yy_current_state;
	char *yy_cp, *yy_bp;
	int yy_act;
    
	if ( !(yy_init) )
		{
		(yy_init) = 1;

#ifdef YY_USER_INIT
		YY_USER_INIT;
#endif

        /* Create the reject buffer large enough to save one state per allowed character. */
        if ( ! (yy_state_buf) )
            (yy_state_buf) = (yy_state_type *)fortran_alloc(YY_STATE_BUF_SIZE  );
            if ( ! (yy_state_buf) )
                YY_FATAL_ERROR( "out of dynamic memory in fortran_lex()" );

		if ( ! (yy_start) )
			(yy_start) = 1;	/* first start state */

		if ( ! fortran_in )
			fortran_in = stdin;

		if ( ! fortran_out )
			fortran_out = stdout;

		if ( ! YY_CURRENT_BUFFER ) {
			fortran_ensure_buffer_stack ();
			YY_CURRENT_BUFFER_LVALUE =
				fortran__create_buffer(fortran_in,YY_BUF_SIZE );
		}

		fortran__load_buffer_state( );
		}

	{
#line 101 "fortran.lex"

  if (infixed) BEGIN(fortran77style) ;
  if (infree)  BEGIN(fortran90style) ;

#line 3810 "fortran.yy.c"

	while ( /*CONSTCOND*/1 )		/* loops until end-of-file is reached */
		{
		yy_cp = (yy_c_buf_p);

		/* Support of fortran_text. */
		*yy_cp = (yy_hold_char);

		/* yy_bp points to the position in yy_ch_buf of the start of
		 * the current run.
		 */
		yy_bp = yy_cp;

		yy_current_state = (yy_start);
		yy_current_state += YY_AT_BOL();

		(yy_state_ptr) = (yy_state_buf);
		*(yy_state_ptr)++ = yy_current_state;

yy_match:
		do
			{
			YY_CHAR yy_c = yy_ec[YY_SC_TO_UI(*yy_cp)] ;
			while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
				{
				yy_current_state = (int) yy_def[yy_current_state];
				if ( yy_current_state >= 1936 )
					yy_c = yy_meta[(unsigned int) yy_c];
				}
			yy_current_state = yy_nxt[yy_base[yy_current_state] + (flex_int16_t) yy_c];
			*(yy_state_ptr)++ = yy_current_state;
			++yy_cp;
			}
		while ( yy_base[yy_current_state] != 9682 );

yy_find_action:
		yy_current_state = *--(yy_state_ptr);
		(yy_lp) = yy_accept[yy_current_state];
find_rule: /* we branch to this label when backing up */
		for ( ; ; ) /* until we find what rule we matched */
			{
			if ( (yy_lp) && (yy_lp) < yy_accept[yy_current_state + 1] )
				{
				yy_act = yy_acclist[(yy_lp)];
				if ( yy_act & YY_TRAILING_HEAD_MASK ||
				     (yy_looking_for_trail_begin) )
					{
					if ( yy_act == (yy_looking_for_trail_begin) )
						{
						(yy_looking_for_trail_begin) = 0;
						yy_act &= ~YY_TRAILING_HEAD_MASK;
						break;
						}
					}
				else if ( yy_act & YY_TRAILING_MASK )
					{
					(yy_looking_for_trail_begin) = yy_act & ~YY_TRAILING_MASK;
					(yy_looking_for_trail_begin) |= YY_TRAILING_HEAD_MASK;
					}
				else
					{
					(yy_full_match) = yy_cp;
					(yy_full_state) = (yy_state_ptr);
					(yy_full_lp) = (yy_lp);
					break;
					}
				++(yy_lp);
				goto find_rule;
				}
			--yy_cp;
			yy_current_state = *--(yy_state_ptr);
			(yy_lp) = yy_accept[yy_current_state];
			}

		YY_DO_BEFORE_ACTION;

do_action:	/* This label is used only to access EOF actions. */

		switch ( yy_act )
	{ /* beginning of action switch */
case 1:
YY_RULE_SETUP
#line 105 "fortran.lex"
{ return TOK_SUBROUTINE; }
	YY_BREAK
case 2:
YY_RULE_SETUP
#line 106 "fortran.lex"
{ return TOK_PROGRAM; }
	YY_BREAK
case 3:
YY_RULE_SETUP
#line 107 "fortran.lex"
{ inallocate = 1; return TOK_ALLOCATE; }
	YY_BREAK
case 4:
YY_RULE_SETUP
#line 108 "fortran.lex"
{ return TOK_CONTINUE; }
	YY_BREAK
case 5:
YY_RULE_SETUP
#line 109 "fortran.lex"
{ return TOK_NULLIFY; }
	YY_BREAK
case 6:
YY_RULE_SETUP
#line 110 "fortran.lex"
{ inallocate = 1; return TOK_DEALLOCATE; }
	YY_BREAK
case 7:
YY_RULE_SETUP
#line 111 "fortran.lex"
{ return TOK_RESULT; }
	YY_BREAK
case 8:
YY_RULE_SETUP
#line 112 "fortran.lex"
{ return TOK_FUNCTION; }
	YY_BREAK
case 9:
YY_RULE_SETUP
#line 113 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDUNIT;}
	YY_BREAK
case 10:
YY_RULE_SETUP
#line 114 "fortran.lex"
{ pos_curinclude = setposcur()-9; BEGIN(includestate); }
	YY_BREAK
case 11:
YY_RULE_SETUP
#line 115 "fortran.lex"
{ return TOK_USE;}
	YY_BREAK
case 12:
YY_RULE_SETUP
#line 116 "fortran.lex"
{ return TOK_REWIND; }
	YY_BREAK
case 13:
YY_RULE_SETUP
#line 117 "fortran.lex"
{ return TOK_IMPLICIT; }
	YY_BREAK
case 14:
YY_RULE_SETUP
#line 118 "fortran.lex"
{ return TOK_NONE; }
	YY_BREAK
case 15:
YY_RULE_SETUP
#line 119 "fortran.lex"
{ return TOK_CALL; }
	YY_BREAK
case 16:
YY_RULE_SETUP
#line 120 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_TRUE; }
	YY_BREAK
case 17:
YY_RULE_SETUP
#line 121 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_FALSE; }
	YY_BREAK
case 18:
YY_RULE_SETUP
#line 122 "fortran.lex"
{ return TOK_POINT_TO; }
	YY_BREAK
case 19:
YY_RULE_SETUP
#line 123 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ASSIGNTYPE;}
	YY_BREAK
case 20:
YY_RULE_SETUP
#line 124 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DASTER; }
	YY_BREAK
case 21:
YY_RULE_SETUP
#line 125 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQV; }
	YY_BREAK
case 22:
YY_RULE_SETUP
#line 126 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQ;  }
	YY_BREAK
case 23:
YY_RULE_SETUP
#line 127 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_GT;  }
	YY_BREAK
case 24:
YY_RULE_SETUP
#line 128 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_GE;  }
	YY_BREAK
case 25:
YY_RULE_SETUP
#line 129 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LT;  }
	YY_BREAK
case 26:
YY_RULE_SETUP
#line 130 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LE;  }
	YY_BREAK
case 27:
YY_RULE_SETUP
#line 131 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NEQV;}
	YY_BREAK
case 28:
YY_RULE_SETUP
#line 132 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NE;  }
	YY_BREAK
case 29:
YY_RULE_SETUP
#line 133 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NOT; }
	YY_BREAK
case 30:
YY_RULE_SETUP
#line 134 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_OR;  }
	YY_BREAK
case 31:
YY_RULE_SETUP
#line 135 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_XOR; }
	YY_BREAK
case 32:
YY_RULE_SETUP
#line 136 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_AND; }
	YY_BREAK
case 33:
YY_RULE_SETUP
#line 137 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQUALEQUAL; }
	YY_BREAK
case 34:
YY_RULE_SETUP
#line 138 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_SLASHEQUAL; }
	YY_BREAK
case 35:
YY_RULE_SETUP
#line 139 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_INFEQUAL; }
	YY_BREAK
case 36:
YY_RULE_SETUP
#line 140 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_SUPEQUAL; }
	YY_BREAK
case 37:
YY_RULE_SETUP
#line 141 "fortran.lex"
{ return TOK_MODULE; }
	YY_BREAK
case 38:
YY_RULE_SETUP
#line 142 "fortran.lex"
{ return TOK_WHILE; }
	YY_BREAK
case 39:
YY_RULE_SETUP
#line 143 "fortran.lex"
{ return TOK_CONCURRENT; }
	YY_BREAK
case 40:
YY_RULE_SETUP
#line 144 "fortran.lex"
{ return TOK_ENDDO; }
	YY_BREAK
case 41:
YY_RULE_SETUP
#line 145 "fortran.lex"
{ strcpy(yylval.na,&fortran_text[2]);
                              if (testandextractfromlist(&List_Do_labels,&fortran_text[2]) == 1)
                              {
                              return TOK_PLAINDO_LABEL_DJVIEW;
                              }
                              else
                              {
                              List_Do_labels=Insertname(List_Do_labels,yylval.na,1);
                              return TOK_PLAINDO_LABEL;
                             }
                             }
	YY_BREAK
case 42:
YY_RULE_SETUP
#line 156 "fortran.lex"
{ increment_nbtokens = 0; return TOK_PLAINDO;}
	YY_BREAK
case 43:
YY_RULE_SETUP
#line 157 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_REAL; }
	YY_BREAK
case 44:
YY_RULE_SETUP
#line 158 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_INTEGER; }
	YY_BREAK
case 45:
YY_RULE_SETUP
#line 159 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LOGICAL; }
	YY_BREAK
case 46:
YY_RULE_SETUP
#line 160 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_CHARACTER; }
	YY_BREAK
case 47:
YY_RULE_SETUP
#line 161 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_HEXA;}
	YY_BREAK
case 48:
YY_RULE_SETUP
#line 162 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DOUBLEPRECISION; }
	YY_BREAK
case 49:
YY_RULE_SETUP
#line 163 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DOUBLECOMPLEX; }
	YY_BREAK
case 50:
YY_RULE_SETUP
#line 164 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_COMPLEX; }
	YY_BREAK
case 51:
YY_RULE_SETUP
#line 165 "fortran.lex"
{ return TOK_ALLOCATABLE; }
	YY_BREAK
case 52:
YY_RULE_SETUP
#line 166 "fortran.lex"
{ return TOK_CONTIGUOUS; }
	YY_BREAK
case 53:
YY_RULE_SETUP
#line 167 "fortran.lex"
{ return TOK_CLOSE; }
	YY_BREAK
case 54:
YY_RULE_SETUP
#line 168 "fortran.lex"
{ return TOK_INQUIRE; }
	YY_BREAK
case 55:
YY_RULE_SETUP
#line 169 "fortran.lex"
{ return TOK_DIMENSION; }
	YY_BREAK
case 56:
YY_RULE_SETUP
#line 170 "fortran.lex"
{ return TOK_PAUSE; }
	YY_BREAK
case 57:
YY_RULE_SETUP
#line 171 "fortran.lex"
{ return TOK_EQUIVALENCE; }
	YY_BREAK
case 58:
YY_RULE_SETUP
#line 172 "fortran.lex"
{ return TOK_STOP; }
	YY_BREAK
case 59:
YY_RULE_SETUP
#line 173 "fortran.lex"
{ return TOK_WHERE; }
	YY_BREAK
case 60:
YY_RULE_SETUP
#line 174 "fortran.lex"
{ return TOK_ENDWHERE; }
	YY_BREAK
case 61:
YY_RULE_SETUP
#line 175 "fortran.lex"
{ return TOK_ELSEWHEREPAR; }
	YY_BREAK
case 62:
YY_RULE_SETUP
#line 176 "fortran.lex"
{ return TOK_ELSEWHERE; }
	YY_BREAK
case 63:
YY_RULE_SETUP
#line 177 "fortran.lex"
{ return TOK_CONTAINS; }
	YY_BREAK
case 64:
YY_RULE_SETUP
#line 178 "fortran.lex"
{ return TOK_ONLY; }
	YY_BREAK
case 65:
YY_RULE_SETUP
#line 179 "fortran.lex"
{ return TOK_PARAMETER; }
	YY_BREAK
case 66:
YY_RULE_SETUP
#line 180 "fortran.lex"
{ return TOK_RECURSIVE; }
	YY_BREAK
case 67:
YY_RULE_SETUP
#line 181 "fortran.lex"
{ return TOK_PURE; }
	YY_BREAK
case 68:
YY_RULE_SETUP
#line 182 "fortran.lex"
{ return TOK_IMPURE; }
	YY_BREAK
case 69:
YY_RULE_SETUP
#line 183 "fortran.lex"
{ return TOK_ELEMENTAL; }
	YY_BREAK
case 70:
YY_RULE_SETUP
#line 184 "fortran.lex"
{ return TOK_COMMON; }
	YY_BREAK
case 71:
YY_RULE_SETUP
#line 185 "fortran.lex"
{ return TOK_GLOBAL; }
	YY_BREAK
case 72:
YY_RULE_SETUP
#line 186 "fortran.lex"
{ return TOK_EXTERNAL; }
	YY_BREAK
case 73:
YY_RULE_SETUP
#line 187 "fortran.lex"
{ intent_spec = 1; return TOK_INTENT; }
	YY_BREAK
case 74:
YY_RULE_SETUP
#line 188 "fortran.lex"
{ return TOK_POINTER; }
	YY_BREAK
case 75:
YY_RULE_SETUP
#line 189 "fortran.lex"
{ return TOK_OPTIONAL; }
	YY_BREAK
case 76:
YY_RULE_SETUP
#line 190 "fortran.lex"
{ return TOK_SAVE; }
	YY_BREAK
case 77:
YY_RULE_SETUP
#line 191 "fortran.lex"
{ pos_cur_decl = setposcur()-strlen(fortran_text); return TOK_TYPEPAR; }
	YY_BREAK
case 78:
YY_RULE_SETUP
#line 192 "fortran.lex"
{ return TOK_TYPE; }
	YY_BREAK
case 79:
YY_RULE_SETUP
#line 193 "fortran.lex"
{ return TOK_ENDTYPE; }
	YY_BREAK
case 80:
YY_RULE_SETUP
#line 194 "fortran.lex"
{ if (inallocate == 1) return TOK_STAT; else { strcpy(yylval.na,fortran_text); return TOK_NAME; } }
	YY_BREAK
case 81:
YY_RULE_SETUP
#line 195 "fortran.lex"
{ return TOK_OPEN; }
	YY_BREAK
case 82:
YY_RULE_SETUP
#line 196 "fortran.lex"
{ return TOK_RETURN; }
	YY_BREAK
case 83:
YY_RULE_SETUP
#line 197 "fortran.lex"
{ return TOK_EXIT; }
	YY_BREAK
case 84:
YY_RULE_SETUP
#line 198 "fortran.lex"
{ return TOK_PRINT; }
	YY_BREAK
case 85:
YY_RULE_SETUP
#line 199 "fortran.lex"
{ return TOK_PROCEDURE; }
	YY_BREAK
case 86:
YY_RULE_SETUP
#line 200 "fortran.lex"
{ in_io_control_spec = 1; return TOK_READ_PAR; }
	YY_BREAK
case 87:
YY_RULE_SETUP
#line 201 "fortran.lex"
{ return TOK_READ; }
	YY_BREAK
case 88:
YY_RULE_SETUP
#line 202 "fortran.lex"
{ return TOK_NAMELIST; }
	YY_BREAK
case 89:
YY_RULE_SETUP
#line 203 "fortran.lex"
{ in_io_control_spec = 1; return TOK_WRITE_PAR; }
	YY_BREAK
case 90:
YY_RULE_SETUP
#line 204 "fortran.lex"
{ return TOK_WRITE; }
	YY_BREAK
case 91:
YY_RULE_SETUP
#line 205 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_FLUSH; }
	YY_BREAK
case 92:
YY_RULE_SETUP
#line 206 "fortran.lex"
{ return TOK_TARGET; }
	YY_BREAK
case 93:
YY_RULE_SETUP
#line 207 "fortran.lex"
{ return TOK_PUBLIC; }
	YY_BREAK
case 94:
YY_RULE_SETUP
#line 208 "fortran.lex"
{ return TOK_PRIVATE; }
	YY_BREAK
case 95:
YY_RULE_SETUP
#line 209 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_IN; }
                              else
                              {
                              return TOK_NAME;
                              }
                            }
	YY_BREAK
case 96:
YY_RULE_SETUP
#line 217 "fortran.lex"
{ pos_curdata = setposcur()-strlen(fortran_text); /*Init_List_Data_Var();*/ return TOK_DATA; }
	YY_BREAK
case 97:
YY_RULE_SETUP
#line 218 "fortran.lex"
{ return TOK_PLAINGOTO; }
	YY_BREAK
case 98:
YY_RULE_SETUP
#line 219 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_OUT; }
                              else
                              {
                              return TOK_NAME;
                              }
                            }
	YY_BREAK
case 99:
YY_RULE_SETUP
#line 227 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_IN; }
                              else
                              {
                              return TOK_INOUT;
                              }
                            }
	YY_BREAK
case 100:
YY_RULE_SETUP
#line 235 "fortran.lex"
{ return TOK_INTRINSIC; }
	YY_BREAK
case 101:
YY_RULE_SETUP
#line 236 "fortran.lex"
{ return TOK_THEN; }
	YY_BREAK
case 102:
YY_RULE_SETUP
#line 237 "fortran.lex"
{ return TOK_ELSEIF; }
	YY_BREAK
case 103:
YY_RULE_SETUP
#line 238 "fortran.lex"
{ return TOK_ELSE; }
	YY_BREAK
case 104:
YY_RULE_SETUP
#line 239 "fortran.lex"
{ return TOK_ENDIF; }
	YY_BREAK
case 105:
YY_RULE_SETUP
#line 240 "fortran.lex"
{strcpy(yylval.na,fortran_text);
                            return TOK_LOGICALIF_PAR;
                            }
	YY_BREAK
case 106:
/* rule 106 can match eol */
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
YY_LINENO_REWIND_TO(yy_bp + 2);
(yy_c_buf_p) = yy_cp = yy_bp + 2;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 243 "fortran.lex"
{strcpy(yylval.na,fortran_text);
                            return TOK_NAME;
                            }
	YY_BREAK
case 107:
YY_RULE_SETUP
#line 246 "fortran.lex"
{strcpy(yylval.na,fortran_text);
                            return TOK_LOGICALIF_PAR;
                            }
	YY_BREAK
case 108:
YY_RULE_SETUP
#line 249 "fortran.lex"
{ return TOK_SELECTCASE; }
	YY_BREAK
case 109:
YY_RULE_SETUP
#line 250 "fortran.lex"
{ if (in_select_case_stmt > 0) return TOK_CASE ; else return TOK_NAME;}
	YY_BREAK
case 110:
YY_RULE_SETUP
#line 251 "fortran.lex"
{ return TOK_DEFAULT; }
	YY_BREAK
case 111:
YY_RULE_SETUP
#line 252 "fortran.lex"
{ return TOK_ENDSELECT; }
	YY_BREAK
case 112:
YY_RULE_SETUP
#line 253 "fortran.lex"
{ return TOK_FILE; }
	YY_BREAK
case 113:
YY_RULE_SETUP
#line 254 "fortran.lex"
{ return TOK_ACCESS; }
	YY_BREAK
case 114:
YY_RULE_SETUP
#line 255 "fortran.lex"
{ return TOK_ACTION; }
	YY_BREAK
case 115:
YY_RULE_SETUP
#line 256 "fortran.lex"
{ return TOK_IOLENGTH; }
	YY_BREAK
case 116:
YY_RULE_SETUP
#line 257 "fortran.lex"
{ return TOK_UNIT; }
	YY_BREAK
case 117:
YY_RULE_SETUP
#line 258 "fortran.lex"
{ return TOK_NEWUNIT; }
	YY_BREAK
case 118:
YY_RULE_SETUP
#line 259 "fortran.lex"
{ return TOK_OPENED; }
	YY_BREAK
case 119:
YY_RULE_SETUP
#line 260 "fortran.lex"
{ return TOK_FMT; }
	YY_BREAK
case 120:
YY_RULE_SETUP
#line 261 "fortran.lex"
{ return TOK_NML; }
	YY_BREAK
case 121:
YY_RULE_SETUP
#line 262 "fortran.lex"
{ return TOK_END; }
	YY_BREAK
case 122:
YY_RULE_SETUP
#line 263 "fortran.lex"
{ return TOK_EOR; }
	YY_BREAK
case 123:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 3;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 264 "fortran.lex"
{
                            if (in_char_selector ==1)
                               return TOK_LEN;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
	YY_BREAK
case 124:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 4;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 272 "fortran.lex"
{
                            if ((in_char_selector==1) || (in_kind_selector == 1))
                               return TOK_KIND;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
	YY_BREAK
case 125:
YY_RULE_SETUP
#line 280 "fortran.lex"
{ return TOK_ERRMSG; }
	YY_BREAK
case 126:
YY_RULE_SETUP
#line 281 "fortran.lex"
{ return TOK_MOLD; }
	YY_BREAK
case 127:
YY_RULE_SETUP
#line 282 "fortran.lex"
{ return TOK_SOURCE; }
	YY_BREAK
case 128:
YY_RULE_SETUP
#line 283 "fortran.lex"
{ return TOK_POSITION; }
	YY_BREAK
case 129:
YY_RULE_SETUP
#line 284 "fortran.lex"
{ return TOK_IOMSG; }
	YY_BREAK
case 130:
YY_RULE_SETUP
#line 285 "fortran.lex"
{ return TOK_IOSTAT; }
	YY_BREAK
case 131:
YY_RULE_SETUP
#line 286 "fortran.lex"
{ return TOK_ERR; }
	YY_BREAK
case 132:
YY_RULE_SETUP
#line 287 "fortran.lex"
{ return TOK_FORM; }
	YY_BREAK
case 133:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 4;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 288 "fortran.lex"
{
                            if (in_inquire==1)
                               return TOK_NAME_EQ;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
	YY_BREAK
case 134:
YY_RULE_SETUP
#line 296 "fortran.lex"
{ return TOK_RECL; }
	YY_BREAK
case 135:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 3;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 297 "fortran.lex"
{ if (in_io_control_spec == 1)
                              return TOK_REC;
                             else
                             {
                             strcpy(yylval.na,fortran_text); return TOK_NAME;
                             }
                             }
	YY_BREAK
case 136:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 6;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 304 "fortran.lex"
{ if (close_or_connect == 1)
                              return TOK_STATUS;
                             else
                             {
                             strcpy(yylval.na,fortran_text); return TOK_NAME;
                             }
                             }
	YY_BREAK
case 137:
YY_RULE_SETUP
#line 311 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NAME;}
	YY_BREAK
case 138:
YY_RULE_SETUP
#line 312 "fortran.lex"
{ return TOK_EXIST; }
	YY_BREAK
case 139:
YY_RULE_SETUP
#line 313 "fortran.lex"
{ return TOK_CYCLE; }
	YY_BREAK
case 140:
YY_RULE_SETUP
#line 314 "fortran.lex"
{ return TOK_BACKSPACE; }
	YY_BREAK
case 141:
YY_RULE_SETUP
#line 315 "fortran.lex"
{ return TOK_FOURDOTS;  }
	YY_BREAK
case 142:
/* rule 142 can match eol */
YY_RULE_SETUP
#line 316 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DSLASH; }
	YY_BREAK
case 143:
YY_RULE_SETUP
#line 317 "fortran.lex"
{ return TOK_LEFTAB; }
	YY_BREAK
case 144:
YY_RULE_SETUP
#line 318 "fortran.lex"
{ return TOK_RIGHTAB; }
	YY_BREAK
case 145:
YY_RULE_SETUP
#line 319 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_SLASH; }
	YY_BREAK
case 146:
/* rule 146 can match eol */
YY_RULE_SETUP
#line 320 "fortran.lex"
{
                              INCREMENT_LINE_NUM() ; strcpy(yylval.na,fortran_text); return TOK_CHAR_CUT; }
	YY_BREAK
case 147:
/* rule 147 can match eol */
YY_RULE_SETUP
#line 322 "fortran.lex"
{Add_Include_1(fortran_text);}
	YY_BREAK
case 148:
YY_RULE_SETUP
#line 323 "fortran.lex"
{}
	YY_BREAK
case 149:
/* rule 149 can match eol */
YY_RULE_SETUP
#line 324 "fortran.lex"
{
                  if (inmoduledeclare == 0 )
                  {
                  pos_end=setposcur();
                  RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
                  }
                  out_of_donottreat();
                  }
	YY_BREAK
case 150:
/* rule 150 can match eol */
YY_RULE_SETUP
#line 332 "fortran.lex"
{ strcpy(yylval.na,fortran_text);return TOK_CHAR_CONSTANT; }
	YY_BREAK
case 151:
/* rule 151 can match eol */
YY_RULE_SETUP
#line 333 "fortran.lex"
{ strcpy(yylval.na,fortran_text);return TOK_CHAR_MESSAGE; }
	YY_BREAK
case 152:
YY_RULE_SETUP
#line 334 "fortran.lex"
{ BEGIN(donottreat_interface); }
	YY_BREAK
case 153:
/* rule 153 can match eol */
YY_RULE_SETUP
#line 335 "fortran.lex"
{ out_of_donottreat(); return '\n'; }
	YY_BREAK
case 154:
/* rule 154 can match eol */
YY_RULE_SETUP
#line 336 "fortran.lex"
{INCREMENT_LINE_NUM() ; }
	YY_BREAK
case 155:
/* rule 155 can match eol */
YY_RULE_SETUP
#line 337 "fortran.lex"
{strcpy(yylval.na,fortran_text); removenewline(yylval.na);
                            return TOK_NAME; }
	YY_BREAK
case 156:
YY_RULE_SETUP
#line 339 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NAME; }
	YY_BREAK
case 157:
YY_RULE_SETUP
#line 340 "fortran.lex"
{strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 158:
/* rule 158 can match eol */
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
YY_LINENO_REWIND_TO(yy_cp - 1);
(yy_c_buf_p) = yy_cp -= 1;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 341 "fortran.lex"
{  // REAL1
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 159:
YY_RULE_SETUP
#line 343 "fortran.lex"
{  // REAL2
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 160:
YY_RULE_SETUP
#line 345 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                             if (lastwasendofstmt == 0)
                              return TOK_CSTINT;
                             else
                              if (testandextractfromlist(&List_Do_labels,fortran_text) == 1)
                              {
                              removefromlist(&List_Do_labels,yylval.na);
                              return TOK_LABEL_DJVIEW;
                              }
                              else
                              {
                              return TOK_LABEL;
                              }
                             }
	YY_BREAK
case 161:
YY_RULE_SETUP
#line 359 "fortran.lex"
{}
	YY_BREAK
case 162:
YY_RULE_SETUP
#line 360 "fortran.lex"
{}
	YY_BREAK
case 163:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 1;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 361 "fortran.lex"
{
                            in_complex_literal = -1;
                            return (int) *fortran_text;
                            }
	YY_BREAK
case 164:
YY_RULE_SETUP
#line 365 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 165:
YY_RULE_SETUP
#line 366 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 166:
YY_RULE_SETUP
#line 367 "fortran.lex"
{ lastwasendofstmt=1; token_since_endofstmt = 0; return TOK_SEMICOLON; }
	YY_BREAK
case 167:
YY_RULE_SETUP
#line 368 "fortran.lex"
{ if (in_complex_literal==-1) {return TOK_COMMACOMPLEX; in_complex_literal=0;} else; return (int) *fortran_text; }
	YY_BREAK
case 168:
YY_RULE_SETUP
#line 369 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 169:
YY_RULE_SETUP
#line 370 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 170:
YY_RULE_SETUP
#line 371 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 171:
/* rule 171 can match eol */
YY_RULE_SETUP
#line 372 "fortran.lex"
{ INCREMENT_LINE_NUM() ; lastwasendofstmt=1; token_since_endofstmt = 0; increment_nbtokens = 0; return '\n'; }
	YY_BREAK
case 172:
YY_RULE_SETUP
#line 373 "fortran.lex"
{increment_nbtokens = 0;}
	YY_BREAK
case 173:
/* rule 173 can match eol */
YY_RULE_SETUP
#line 374 "fortran.lex"
{
                              return TOK_LABEL_FORMAT; }
	YY_BREAK
case 174:
/* rule 174 can match eol */
YY_RULE_SETUP
#line 376 "fortran.lex"
{return TOK_LABEL_FORMAT; }
	YY_BREAK
case 175:
/* rule 175 can match eol */
YY_RULE_SETUP
#line 377 "fortran.lex"
{ INCREMENT_LINE_NUM() ; newlinef90=1; }
	YY_BREAK
case 176:
/* rule 176 can match eol */
YY_RULE_SETUP
#line 378 "fortran.lex"
{ INCREMENT_LINE_NUM() ;}
	YY_BREAK
case 177:
/* rule 177 can match eol */
YY_RULE_SETUP
#line 380 "fortran.lex"
{INCREMENT_LINE_NUM() ; BEGIN(donottreat); }
	YY_BREAK
case 178:
/* rule 178 can match eol */
YY_RULE_SETUP
#line 381 "fortran.lex"
{out_of_donottreat(); return '\n'; }
	YY_BREAK
case 179:
/* rule 179 can match eol */
YY_RULE_SETUP
#line 382 "fortran.lex"
{INCREMENT_LINE_NUM() ; }
	YY_BREAK
case 180:
/* rule 180 can match eol */
YY_RULE_SETUP
#line 383 "fortran.lex"
{INCREMENT_LINE_NUM() ; increment_nbtokens = 0;}
	YY_BREAK
case 181:
/* rule 181 can match eol */
YY_RULE_SETUP
#line 384 "fortran.lex"
{INCREMENT_LINE_NUM() ; increment_nbtokens = 0;}
	YY_BREAK
case 182:
YY_RULE_SETUP
#line 385 "fortran.lex"
{increment_nbtokens = 0;}
	YY_BREAK
case YY_STATE_EOF(INITIAL):
case YY_STATE_EOF(parameter):
case YY_STATE_EOF(character):
case YY_STATE_EOF(donottreat):
case YY_STATE_EOF(donottreat_interface):
case YY_STATE_EOF(includestate):
case YY_STATE_EOF(fortran77style):
case YY_STATE_EOF(fortran90style):
#line 386 "fortran.lex"
{endoffile = 1; yyterminate();}
	YY_BREAK
case 183:
YY_RULE_SETUP
#line 387 "fortran.lex"
ECHO;
	YY_BREAK
#line 4962 "fortran.yy.c"

	case YY_END_OF_BUFFER:
		{
		/* Amount of text matched not including the EOB char. */
		int yy_amount_of_matched_text = (int) (yy_cp - (yytext_ptr)) - 1;

		/* Undo the effects of YY_DO_BEFORE_ACTION. */
		*yy_cp = (yy_hold_char);
		YY_RESTORE_YY_MORE_OFFSET

		if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_NEW )
			{
			/* We're scanning a new file or input source.  It's
			 * possible that this happened because the user
			 * just pointed fortran_in at a new source and called
			 * fortran_lex().  If so, then we have to assure
			 * consistency between YY_CURRENT_BUFFER and our
			 * globals.  Here is the right place to do so, because
			 * this is the first action (other than possibly a
			 * back-up) that will match for the new input source.
			 */
			(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
			YY_CURRENT_BUFFER_LVALUE->yy_input_file = fortran_in;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status = YY_BUFFER_NORMAL;
			}

		/* Note that here we test for yy_c_buf_p "<=" to the position
		 * of the first EOB in the buffer, since yy_c_buf_p will
		 * already have been incremented past the NUL character
		 * (since all states make transitions on EOB to the
		 * end-of-buffer state).  Contrast this with the test
		 * in input().
		 */
		if ( (yy_c_buf_p) <= &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			{ /* This was really a NUL. */
			yy_state_type yy_next_state;

			(yy_c_buf_p) = (yytext_ptr) + yy_amount_of_matched_text;

			yy_current_state = yy_get_previous_state(  );

			/* Okay, we're now positioned to make the NUL
			 * transition.  We couldn't have
			 * yy_get_previous_state() go ahead and do it
			 * for us because it doesn't know how to deal
			 * with the possibility of jamming (and we don't
			 * want to build jamming into it because then it
			 * will run more slowly).
			 */

			yy_next_state = yy_try_NUL_trans( yy_current_state );

			yy_bp = (yytext_ptr) + YY_MORE_ADJ;

			if ( yy_next_state )
				{
				/* Consume the NUL. */
				yy_cp = ++(yy_c_buf_p);
				yy_current_state = yy_next_state;
				goto yy_match;
				}

			else
				{
				yy_cp = (yy_c_buf_p);
				goto yy_find_action;
				}
			}

		else switch ( yy_get_next_buffer(  ) )
			{
			case EOB_ACT_END_OF_FILE:
				{
				(yy_did_buffer_switch_on_eof) = 0;

				if ( fortran_wrap( ) )
					{
					/* Note: because we've taken care in
					 * yy_get_next_buffer() to have set up
					 * fortran_text, we can now set up
					 * yy_c_buf_p so that if some total
					 * hoser (like flex itself) wants to
					 * call the scanner after we return the
					 * YY_NULL, it'll still work - another
					 * YY_NULL will get returned.
					 */
					(yy_c_buf_p) = (yytext_ptr) + YY_MORE_ADJ;

					yy_act = YY_STATE_EOF(YY_START);
					goto do_action;
					}

				else
					{
					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
					}
				break;
				}

			case EOB_ACT_CONTINUE_SCAN:
				(yy_c_buf_p) =
					(yytext_ptr) + yy_amount_of_matched_text;

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_match;

			case EOB_ACT_LAST_MATCH:
				(yy_c_buf_p) =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)];

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_find_action;
			}
		break;
		}

	default:
		YY_FATAL_ERROR(
			"fatal flex scanner internal error--no action found" );
	} /* end of action switch */
		} /* end of scanning one token */
	} /* end of user's declarations */
} /* end of fortran_lex */

/* yy_get_next_buffer - try to read in a new buffer
 *
 * Returns a code representing an action:
 *	EOB_ACT_LAST_MATCH -
 *	EOB_ACT_CONTINUE_SCAN - continue scanning from current position
 *	EOB_ACT_END_OF_FILE - end of file
 */
static int yy_get_next_buffer (void)
{
    	char *dest = YY_CURRENT_BUFFER_LVALUE->yy_ch_buf;
	char *source = (yytext_ptr);
	yy_size_t number_to_move, i;
	int ret_val;

	if ( (yy_c_buf_p) > &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] )
		YY_FATAL_ERROR(
		"fatal flex scanner internal error--end of buffer missed" );

	if ( YY_CURRENT_BUFFER_LVALUE->yy_fill_buffer == 0 )
		{ /* Don't try to fill the buffer, so this is an EOF. */
		if ( (yy_c_buf_p) - (yytext_ptr) - YY_MORE_ADJ == 1 )
			{
			/* We matched a single character, the EOB, so
			 * treat this as a final EOF.
			 */
			return EOB_ACT_END_OF_FILE;
			}

		else
			{
			/* We matched some text prior to the EOB, first
			 * process it.
			 */
			return EOB_ACT_LAST_MATCH;
			}
		}

	/* Try to read more data. */

	/* First move last chars to start of buffer. */
	number_to_move = (yy_size_t) ((yy_c_buf_p) - (yytext_ptr)) - 1;

	for ( i = 0; i < number_to_move; ++i )
		*(dest++) = *(source++);

	if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_EOF_PENDING )
		/* don't do the read, it's not guaranteed to return an EOF,
		 * just force an EOF
		 */
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars) = 0;

	else
		{
			int num_to_read =
			YY_CURRENT_BUFFER_LVALUE->yy_buf_size - number_to_move - 1;

		while ( num_to_read <= 0 )
			{ /* Not enough room in the buffer - grow it. */

			YY_FATAL_ERROR(
"input buffer overflow, can't enlarge buffer because scanner uses REJECT" );

			}

		if ( num_to_read > YY_READ_BUF_SIZE )
			num_to_read = YY_READ_BUF_SIZE;

		/* Read in more data. */
		YY_INPUT( (&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move]),
			(yy_n_chars), num_to_read );

		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	if ( (yy_n_chars) == 0 )
		{
		if ( number_to_move == YY_MORE_ADJ )
			{
			ret_val = EOB_ACT_END_OF_FILE;
			fortran_restart(fortran_in  );
			}

		else
			{
			ret_val = EOB_ACT_LAST_MATCH;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status =
				YY_BUFFER_EOF_PENDING;
			}
		}

	else
		ret_val = EOB_ACT_CONTINUE_SCAN;

	if ((int) ((yy_n_chars) + number_to_move) > YY_CURRENT_BUFFER_LVALUE->yy_buf_size) {
		/* Extend the array by 50%, plus the number we really need. */
		int new_size = (yy_n_chars) + number_to_move + ((yy_n_chars) >> 1);
		YY_CURRENT_BUFFER_LVALUE->yy_ch_buf = (char *) fortran_realloc((void *) YY_CURRENT_BUFFER_LVALUE->yy_ch_buf,(yy_size_t) new_size  );
		if ( ! YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			YY_FATAL_ERROR( "out of dynamic memory in yy_get_next_buffer()" );
	}

	(yy_n_chars) += number_to_move;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] = YY_END_OF_BUFFER_CHAR;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] = YY_END_OF_BUFFER_CHAR;

	(yytext_ptr) = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[0];

	return ret_val;
}

/* yy_get_previous_state - get the state just before the EOB char was reached */

    static yy_state_type yy_get_previous_state (void)
{
	yy_state_type yy_current_state;
	char *yy_cp;
    
	yy_current_state = (yy_start);
	yy_current_state += YY_AT_BOL();

	(yy_state_ptr) = (yy_state_buf);
	*(yy_state_ptr)++ = yy_current_state;

	for ( yy_cp = (yytext_ptr) + YY_MORE_ADJ; yy_cp < (yy_c_buf_p); ++yy_cp )
		{
		YY_CHAR yy_c = (*yy_cp ? yy_ec[YY_SC_TO_UI(*yy_cp)] : 1);
		while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
			{
			yy_current_state = (int) yy_def[yy_current_state];
			if ( yy_current_state >= 1936 )
				yy_c = yy_meta[(unsigned int) yy_c];
			}
		yy_current_state = yy_nxt[yy_base[yy_current_state] + (flex_int16_t) yy_c];
		*(yy_state_ptr)++ = yy_current_state;
		}

	return yy_current_state;
}

/* yy_try_NUL_trans - try to make a transition on the NUL character
 *
 * synopsis
 *	next_state = yy_try_NUL_trans( current_state );
 */
    static yy_state_type yy_try_NUL_trans  (yy_state_type yy_current_state )
{
	int yy_is_jam;
    
	YY_CHAR yy_c = 1;
	while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
		{
		yy_current_state = (int) yy_def[yy_current_state];
		if ( yy_current_state >= 1936 )
			yy_c = yy_meta[(unsigned int) yy_c];
		}
	yy_current_state = yy_nxt[yy_base[yy_current_state] + (flex_int16_t) yy_c];
	yy_is_jam = (yy_current_state == 1935);
	if ( ! yy_is_jam )
		*(yy_state_ptr)++ = yy_current_state;

		return yy_is_jam ? 0 : yy_current_state;
}

#ifndef YY_NO_UNPUT

    static void yyunput (int c, char * yy_bp )
{
	char *yy_cp;
    
    yy_cp = (yy_c_buf_p);

	/* undo effects of setting up fortran_text */
	*yy_cp = (yy_hold_char);

	if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
		{ /* need to shift things up to make room */
		/* +2 for EOB chars. */
		int number_to_move = (yy_n_chars) + 2;
		char *dest = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[
					YY_CURRENT_BUFFER_LVALUE->yy_buf_size + 2];
		char *source =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move];

		while ( source > YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			*--dest = *--source;

		yy_cp += (int) (dest - source);
		yy_bp += (int) (dest - source);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars =
			(yy_n_chars) = (int) YY_CURRENT_BUFFER_LVALUE->yy_buf_size;

		if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
			YY_FATAL_ERROR( "flex scanner push-back overflow" );
		}

	*--yy_cp = (char) c;

	(yytext_ptr) = yy_bp;
	(yy_hold_char) = *yy_cp;
	(yy_c_buf_p) = yy_cp;
}

#endif

#ifndef YY_NO_INPUT
#ifdef __cplusplus
    static int yyinput (void)
#else
    static int input  (void)
#endif

{
	int c;
    
	*(yy_c_buf_p) = (yy_hold_char);

	if ( *(yy_c_buf_p) == YY_END_OF_BUFFER_CHAR )
		{
		/* yy_c_buf_p now points to the character we want to return.
		 * If this occurs *before* the EOB characters, then it's a
		 * valid NUL; if not, then we've hit the end of the buffer.
		 */
		if ( (yy_c_buf_p) < &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			/* This was really a NUL. */
			*(yy_c_buf_p) = '\0';

		else
			{ /* need more input */
			int offset = (yy_c_buf_p) - (yytext_ptr);
			++(yy_c_buf_p);

			switch ( yy_get_next_buffer(  ) )
				{
				case EOB_ACT_LAST_MATCH:
					/* This happens because yy_g_n_b()
					 * sees that we've accumulated a
					 * token and flags that we need to
					 * try matching the token before
					 * proceeding.  But for input(),
					 * there's no matching to consider.
					 * So convert the EOB_ACT_LAST_MATCH
					 * to EOB_ACT_END_OF_FILE.
					 */

					/* Reset buffer status. */
					fortran_restart(fortran_in );

					/*FALLTHROUGH*/

				case EOB_ACT_END_OF_FILE:
					{
					if ( fortran_wrap( ) )
						return 0;

					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
#ifdef __cplusplus
					return yyinput();
#else
					return input();
#endif
					}

				case EOB_ACT_CONTINUE_SCAN:
					(yy_c_buf_p) = (yytext_ptr) + offset;
					break;
				}
			}
		}

	c = *(unsigned char *) (yy_c_buf_p);	/* cast for 8-bit char's */
	*(yy_c_buf_p) = '\0';	/* preserve fortran_text */
	(yy_hold_char) = *++(yy_c_buf_p);

	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = (c == '\n');

	return c;
}
#endif	/* ifndef YY_NO_INPUT */

/** Immediately switch to a different input stream.
 * @param input_file A readable stream.
 * 
 * @note This function does not reset the start condition to @c INITIAL .
 */
    void fortran_restart  (FILE * input_file )
{
    
	if ( ! YY_CURRENT_BUFFER ){
        fortran_ensure_buffer_stack ();
		YY_CURRENT_BUFFER_LVALUE =
            fortran__create_buffer(fortran_in,YY_BUF_SIZE );
	}

	fortran__init_buffer(YY_CURRENT_BUFFER,input_file );
	fortran__load_buffer_state( );
}

/** Switch to a different input buffer.
 * @param new_buffer The new input buffer.
 * 
 */
    void fortran__switch_to_buffer  (YY_BUFFER_STATE  new_buffer )
{
    
	/* TODO. We should be able to replace this entire function body
	 * with
	 *		fortran_pop_buffer_state();
	 *		fortran_push_buffer_state(new_buffer);
     */
	fortran_ensure_buffer_stack ();
	if ( YY_CURRENT_BUFFER == new_buffer )
		return;

	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	YY_CURRENT_BUFFER_LVALUE = new_buffer;
	fortran__load_buffer_state( );

	/* We don't actually know whether we did this switch during
	 * EOF (fortran_wrap()) processing, but the only time this flag
	 * is looked at is after fortran_wrap() is called, so it's safe
	 * to go ahead and always set it.
	 */
	(yy_did_buffer_switch_on_eof) = 1;
}

static void fortran__load_buffer_state  (void)
{
    	(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
	(yytext_ptr) = (yy_c_buf_p) = YY_CURRENT_BUFFER_LVALUE->yy_buf_pos;
	fortran_in = YY_CURRENT_BUFFER_LVALUE->yy_input_file;
	(yy_hold_char) = *(yy_c_buf_p);
}

/** Allocate and initialize an input buffer state.
 * @param file A readable stream.
 * @param size The character buffer size in bytes. When in doubt, use @c YY_BUF_SIZE.
 * 
 * @return the allocated buffer state.
 */
    YY_BUFFER_STATE fortran__create_buffer  (FILE * file, int  size )
{
	YY_BUFFER_STATE b;
    
	b = (YY_BUFFER_STATE) fortran_alloc(sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__create_buffer()" );

	b->yy_buf_size = size;

	/* yy_ch_buf has to be 2 characters longer than the size given because
	 * we need to put in 2 end-of-buffer characters.
	 */
	b->yy_ch_buf = (char *) fortran_alloc((yy_size_t) (b->yy_buf_size + 2)  );
	if ( ! b->yy_ch_buf )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__create_buffer()" );

	b->yy_is_our_buffer = 1;

	fortran__init_buffer(b,file );

	return b;
}

/** Destroy the buffer.
 * @param b a buffer created with fortran__create_buffer()
 * 
 */
    void fortran__delete_buffer (YY_BUFFER_STATE  b )
{
    
	if ( ! b )
		return;

	if ( b == YY_CURRENT_BUFFER ) /* Not sure if we should pop here. */
		YY_CURRENT_BUFFER_LVALUE = (YY_BUFFER_STATE) 0;

	if ( b->yy_is_our_buffer )
		fortran_free((void *) b->yy_ch_buf  );

	fortran_free((void *) b  );
}

/* Initializes or reinitializes a buffer.
 * This function is sometimes called more than once on the same buffer,
 * such as during a fortran_restart() or at EOF.
 */
    static void fortran__init_buffer  (YY_BUFFER_STATE  b, FILE * file )

{
	int oerrno = errno;
    
	fortran__flush_buffer(b );

	b->yy_input_file = file;
	b->yy_fill_buffer = 1;

    /* If b is the current buffer, then fortran__init_buffer was _probably_
     * called from fortran_restart() or through yy_get_next_buffer.
     * In that case, we don't want to reset the lineno or column.
     */
    if (b != YY_CURRENT_BUFFER){
        b->yy_bs_lineno = 1;
        b->yy_bs_column = 0;
    }

        b->yy_is_interactive = file ? (isatty( fileno(file) ) > 0) : 0;
    
	errno = oerrno;
}

/** Discard all buffered characters. On the next scan, YY_INPUT will be called.
 * @param b the buffer state to be flushed, usually @c YY_CURRENT_BUFFER.
 * 
 */
    void fortran__flush_buffer (YY_BUFFER_STATE  b )
{
    	if ( ! b )
		return;

	b->yy_n_chars = 0;

	/* We always need two end-of-buffer characters.  The first causes
	 * a transition to the end-of-buffer state.  The second causes
	 * a jam in that state.
	 */
	b->yy_ch_buf[0] = YY_END_OF_BUFFER_CHAR;
	b->yy_ch_buf[1] = YY_END_OF_BUFFER_CHAR;

	b->yy_buf_pos = &b->yy_ch_buf[0];

	b->yy_at_bol = 1;
	b->yy_buffer_status = YY_BUFFER_NEW;

	if ( b == YY_CURRENT_BUFFER )
		fortran__load_buffer_state( );
}

/** Pushes the new state onto the stack. The new state becomes
 *  the current state. This function will allocate the stack
 *  if necessary.
 *  @param new_buffer The new state.
 *  
 */
void fortran_push_buffer_state (YY_BUFFER_STATE new_buffer )
{
    	if (new_buffer == NULL)
		return;

	fortran_ensure_buffer_stack();

	/* This block is copied from fortran__switch_to_buffer. */
	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	/* Only push if top exists. Otherwise, replace top. */
	if (YY_CURRENT_BUFFER)
		(yy_buffer_stack_top)++;
	YY_CURRENT_BUFFER_LVALUE = new_buffer;

	/* copied from fortran__switch_to_buffer. */
	fortran__load_buffer_state( );
	(yy_did_buffer_switch_on_eof) = 1;
}

/** Removes and deletes the top of the stack, if present.
 *  The next element becomes the new top.
 *  
 */
void fortran_pop_buffer_state (void)
{
    	if (!YY_CURRENT_BUFFER)
		return;

	fortran__delete_buffer(YY_CURRENT_BUFFER );
	YY_CURRENT_BUFFER_LVALUE = NULL;
	if ((yy_buffer_stack_top) > 0)
		--(yy_buffer_stack_top);

	if (YY_CURRENT_BUFFER) {
		fortran__load_buffer_state( );
		(yy_did_buffer_switch_on_eof) = 1;
	}
}

/* Allocates the stack if it does not exist.
 *  Guarantees space for at least one push.
 */
static void fortran_ensure_buffer_stack (void)
{
	int num_to_alloc;
    
	if (!(yy_buffer_stack)) {

		/* First allocation is just for 2 elements, since we don't know if this
		 * scanner will even need a stack. We use 2 instead of 1 to avoid an
		 * immediate realloc on the next call.
         */
      num_to_alloc = 1; /* After all that talk, this was set to 1 anyways... */
		(yy_buffer_stack) = (struct yy_buffer_state**)fortran_alloc
								(num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in fortran_ensure_buffer_stack()" );
								  
		memset((yy_buffer_stack), 0, num_to_alloc * sizeof(struct yy_buffer_state*));
				
		(yy_buffer_stack_max) = num_to_alloc;
		(yy_buffer_stack_top) = 0;
		return;
	}

	if ((yy_buffer_stack_top) >= ((yy_buffer_stack_max)) - 1){

		/* Increase the buffer to prepare for a possible push. */
		yy_size_t grow_size = 8 /* arbitrary grow size */;

		num_to_alloc = (yy_buffer_stack_max) + grow_size;
		(yy_buffer_stack) = (struct yy_buffer_state**)fortran_realloc
								((yy_buffer_stack),
								num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in fortran_ensure_buffer_stack()" );

		/* zero only the new slots.*/
		memset((yy_buffer_stack) + (yy_buffer_stack_max), 0, grow_size * sizeof(struct yy_buffer_state*));
		(yy_buffer_stack_max) = num_to_alloc;
	}
}

/** Setup the input buffer state to scan directly from a user-specified character buffer.
 * @param base the character buffer
 * @param size the size in bytes of the character buffer
 * 
 * @return the newly allocated buffer state object. 
 */
YY_BUFFER_STATE fortran__scan_buffer  (char * base, yy_size_t  size )
{
	YY_BUFFER_STATE b;
    
	if ( size < 2 ||
	     base[size-2] != YY_END_OF_BUFFER_CHAR ||
	     base[size-1] != YY_END_OF_BUFFER_CHAR )
		/* They forgot to leave room for the EOB's. */
		return NULL;

	b = (YY_BUFFER_STATE) fortran_alloc(sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__scan_buffer()" );

	b->yy_buf_size = (int) (size - 2);	/* "- 2" to take care of EOB's */
	b->yy_buf_pos = b->yy_ch_buf = base;
	b->yy_is_our_buffer = 0;
	b->yy_input_file = NULL;
	b->yy_n_chars = b->yy_buf_size;
	b->yy_is_interactive = 0;
	b->yy_at_bol = 1;
	b->yy_fill_buffer = 0;
	b->yy_buffer_status = YY_BUFFER_NEW;

	fortran__switch_to_buffer(b  );

	return b;
}

/** Setup the input buffer state to scan a string. The next call to fortran_lex() will
 * scan from a @e copy of @a str.
 * @param yystr a NUL-terminated string to scan
 * 
 * @return the newly allocated buffer state object.
 * @note If you want to scan bytes that may contain NUL values, then use
 *       fortran__scan_bytes() instead.
 */
YY_BUFFER_STATE fortran__scan_string (yyconst char * yystr )
{
    
	return fortran__scan_bytes(yystr,(int) strlen(yystr) );
}

/** Setup the input buffer state to scan the given bytes. The next call to fortran_lex() will
 * scan from a @e copy of @a bytes.
 * @param yybytes the byte buffer to scan
 * @param _yybytes_len the number of bytes in the buffer pointed to by @a bytes.
 * 
 * @return the newly allocated buffer state object.
 */
YY_BUFFER_STATE fortran__scan_bytes  (yyconst char * yybytes, int  _yybytes_len )
{
	YY_BUFFER_STATE b;
	char *buf;
	yy_size_t n;
	int i;
    
	/* Get memory for full buffer, including space for trailing EOB's. */
	n = (yy_size_t) (_yybytes_len + 2);
	buf = (char *) fortran_alloc(n  );
	if ( ! buf )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__scan_bytes()" );

	for ( i = 0; i < _yybytes_len; ++i )
		buf[i] = yybytes[i];

	buf[_yybytes_len] = buf[_yybytes_len+1] = YY_END_OF_BUFFER_CHAR;

	b = fortran__scan_buffer(buf,n );
	if ( ! b )
		YY_FATAL_ERROR( "bad buffer in fortran__scan_bytes()" );

	/* It's okay to grow etc. this buffer, and we should throw it
	 * away when we're done.
	 */
	b->yy_is_our_buffer = 1;

	return b;
}

#ifndef YY_EXIT_FAILURE
#define YY_EXIT_FAILURE 2
#endif

static void yynoreturn yy_fatal_error (yyconst char* msg )
{
			(void) fprintf( stderr, "%s\n", msg );
	exit( YY_EXIT_FAILURE );
}

/* Redefine yyless() so it works in section 3 code. */

#undef yyless
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up fortran_text. */ \
        yy_size_t yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		fortran_text[fortran_leng] = (yy_hold_char); \
		(yy_c_buf_p) = fortran_text + yyless_macro_arg; \
		(yy_hold_char) = *(yy_c_buf_p); \
		*(yy_c_buf_p) = '\0'; \
		fortran_leng = yyless_macro_arg; \
		} \
	while ( 0 )

/* Accessor  methods (get/set functions) to struct members. */

/** Get the current line number.
 * 
 */
int fortran_get_lineno  (void)
{
        
    return fortran_lineno;
}

/** Get the input stream.
 * 
 */
FILE *fortran_get_in  (void)
{
        return fortran_in;
}

/** Get the output stream.
 * 
 */
FILE *fortran_get_out  (void)
{
        return fortran_out;
}

/** Get the length of the current token.
 * 
 */
int fortran_get_leng  (void)
{
        return fortran_leng;
}

/** Get the current token.
 * 
 */

char *fortran_get_text  (void)
{
        return fortran_text;
}

/** Set the current line number.
 * @param _line_number line number
 * 
 */
void fortran_set_lineno (int  _line_number )
{
    
    fortran_lineno = _line_number;
}

/** Set the input stream. This does not discard the current
 * input buffer.
 * @param _in_str A readable stream.
 * 
 * @see fortran__switch_to_buffer
 */
void fortran_set_in (FILE *  _in_str )
{
        fortran_in = _in_str ;
}

void fortran_set_out (FILE *  _out_str )
{
        fortran_out = _out_str ;
}

int fortran_get_debug  (void)
{
        return fortran__flex_debug;
}

void fortran_set_debug (int  _bdebug )
{
        fortran__flex_debug = _bdebug ;
}

static int yy_init_globals (void)
{
        /* Initialization is the same as for the non-reentrant scanner.
     * This function is called from fortran_lex_destroy(), so don't allocate here.
     */

    (yy_buffer_stack) = NULL;
    (yy_buffer_stack_top) = 0;
    (yy_buffer_stack_max) = 0;
    (yy_c_buf_p) = NULL;
    (yy_init) = 0;
    (yy_start) = 0;

    (yy_state_buf) = 0;
    (yy_state_ptr) = 0;
    (yy_full_match) = 0;
    (yy_lp) = 0;

/* Defined in main.c */
#ifdef YY_STDINIT
    fortran_in = stdin;
    fortran_out = stdout;
#else
    fortran_in = NULL;
    fortran_out = NULL;
#endif

    /* For future reference: Set errno on error, since we are called by
     * fortran_lex_init()
     */
    return 0;
}

/* fortran_lex_destroy is for both reentrant and non-reentrant scanners. */
int fortran_lex_destroy  (void)
{
    
    /* Pop the buffer stack, destroying each element. */
	while(YY_CURRENT_BUFFER){
		fortran__delete_buffer(YY_CURRENT_BUFFER  );
		YY_CURRENT_BUFFER_LVALUE = NULL;
		fortran_pop_buffer_state();
	}

	/* Destroy the stack itself. */
	fortran_free((yy_buffer_stack) );
	(yy_buffer_stack) = NULL;

    fortran_free ( (yy_state_buf) );
    (yy_state_buf)  = NULL;

    /* Reset the globals. This is important in a non-reentrant scanner so the next time
     * fortran_lex() is called, initialization will occur. */
    yy_init_globals( );

    return 0;
}

/*
 * Internal utility routines.
 */

#ifndef yytext_ptr
static void yy_flex_strncpy (char* s1, yyconst char * s2, int n )
{
		
	int i;
	for ( i = 0; i < n; ++i )
		s1[i] = s2[i];
}
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (yyconst char * s )
{
	int n;
	for ( n = 0; s[n]; ++n )
		;

	return n;
}
#endif

void *fortran_alloc (yy_size_t  size )
{
			return malloc(size);
}

void *fortran_realloc  (void * ptr, yy_size_t  size )
{
		
	/* The cast to (char *) in the following accommodates both
	 * implementations that use char* generic pointers, and those
	 * that use void* generic pointers.  It works with the latter
	 * because both ANSI C and C++ allow castless assignment from
	 * any pointer type to void*, and deal with argument conversions
	 * as though doing an assignment.
	 */
	return realloc(ptr, size);
}

void fortran_free (void * ptr )
{
			free( (char *) ptr );	/* see fortran_realloc() for (char *) cast */
}

#define YYTABLES_NAME "yytables"

#line 387 "fortran.lex"



void out_of_donottreat ( void )
{
    BEGIN(INITIAL);
    if (infixed) BEGIN(fortran77style) ;
    if (infree)  BEGIN(fortran90style) ;
    INCREMENT_LINE_NUM() ;
}

