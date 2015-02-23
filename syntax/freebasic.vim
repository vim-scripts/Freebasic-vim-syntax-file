" Vim syntax file
" Language:    FreeBasic
" Maintainer:  Mark Manning <markem@airmail.net>
" Updated:     10/22/2006
"
" Description:
"
"	Based originally on the work done by Allan Kelly <Allan.Kelly@ed.ac.uk>
"	Updated by Mark Manning <markem@airmail.net>
"	Applied FreeBasic support to the already excellent support
"	for standard basic syntax (like QB).
"
"	First version based on Micro$soft QBASIC circa
"	1989, as documented in 'Learn BASIC Now' by
"	Halvorson&Rygmyr. Microsoft Press 1989.  This syntax file
"	not a complete implementation yet.  Send suggestions to
"	the maintainer.
"
"	Quit when a (custom) syntax file was already loaded (Taken from c.vim)
"
"	Updated Feb 2015 by Michael Torrie <torriem@gmail.com>
"
"	fixed highlighting for numbers, changed dim, redim, and as to
"	freebasicArrays so they look better
"
"	Fixed comments, added multi-line comments
"
"	Fixed preprocessor highlighting (wasn't working at all), mostly
"	working now, though occasionally colors the next line after a #define.
"	Added support for include once
"
"	Fixed error highlighting a bit (for some reason clustering isn't
"	working.  
"
"	Instead of defaulting to "Identifier" which marks every variable and
"	symbol, leave those the default text color.  Makes it more consistent
"	with C, Python, and other syntax highlighting in VIM.
"
"	Other things probably still missing and not working.  the ON something 
"	GOTO something syntax is not highlighted.  Nor is OPEN Cons. Select
"	Case is not highlighted either.
"
"	Update:	Michael Torrie (torriem@gmail.com)	2/20/2015 11:22pm
"		I've made a number of changes to the file to fix a lot of things
"		that just weren't working such as preprocessor directives (#ifdef,
"		#define, etc).  Fixed up comment handling a bit, and added multi-line
"		comments (/' '/).  Changed a few other classifications too to make
"		the code highlight more like how code is highlighted in other languages
"		such as C, Python, etc.  User-defined identifiers are not colored at
"		all, which makes code a lot more readable.  Numbers also were not being
"		highlighted at all before; not sure why but I fixed that.  There
"		are still some broken things I encounter once in a while, but overall
"		the highlighting is much improved.
"
if exists("b:current_syntax")
  finish
endif
"
"	Be sure to turn on the "case ignore" since current versions
"	of freebasic support both upper as well as lowercase
"	letters. - MEM 10/1/2006
"
syn case ignore
"
"	This list of keywords is taken directly from the FreeBasic
"	user's guide as presented by the FreeBasic online site.
"
syn keyword	freebasicArrays			ERASE LBOUND REDIM PRESERVE UBOUND DIM AS SHARED

syn keyword	freebasicBitManipulation	BIT BITRESET BITSET HIBYTE HIWORD LOBYTE LOWORD SHL SHR

syn keyword	freebasicCompilerSwitches	DEFBYTE DEFDBL DEFINT DEFLNG DEFLNGINT DEFSHORT DEFSNG DEFSTR
syn keyword	freebasicCompilerSwitches	DEFUBYTE DEFUINT DEFULNGINT DEFUSHORT
syn match	freebasicCompilerSwitches	"\<option\s+\(BASE\|BYVAL\|DYNAMIC\|ESCAPE\|EXPLICIT\|NOKEYWORD\)\>"
syn match	freebasicCompilerSwitches	"\<option\s+\(PRIVATE\|STATIC\)\>"

syn region	freebasicConditional		start="\son\s+" skip=".*" end="gosub"
syn region	freebasicConditional		start="\son\s+" skip=".*" end="goto"
syn match	freebasicConditional		"\<select\s+case\>"
syn keyword	freebasicConditional		if iif then case else elseif with

syn match	freebasicConsole		"\<open\s+\(CONS\|ERR\|PIPE\|SCRN\)\>"
syn keyword	freebasicConsole		BEEP CLS CSRLIN LOCATE PRINT POS SPC TAB VIEW WIDTH

syn keyword	freebasicDataTypes		BYTE CONST DOUBLE ENUM INTEGER LONG LONGINT SHORT STRING
syn keyword	freebasicDataTypes		SINGLE TYPE UBYTE UINTEGER ULONGINT UNION UNSIGNED USHORT WSTRING ZSTRING

syn keyword	freebasicDateTime		DATE DATEADD DATEDIFF DATEPART DATESERIAL DATEVALUE DAY HOUR MINUTE
syn keyword	freebasicDateTime		MONTH MONTHNAME NOW SECOND SETDATE SETTIME TIME TIMESERIAL TIMEVALUE
syn keyword	freebasicDateTime		TIMER YEAR WEEKDAY WEEKDAYNAME

syn keyword	freebasicDebug			ASSERT STOP

syn keyword	freebasicErrorHandling		ERR ERL ERROR LOCAL RESUME
syn match	freebasicErrorHandling		"\<resume\s+next\>"
syn match	freebasicErrorHandling		"\<on\s+error\>"

syn match	freebasicFiles			"\<get\s+#\>"
syn match	freebasicFiles			"\<input\s+#\>"
syn match	freebasicFiles			"\<line\s+input\s+#\>"
syn match	freebasicFiles			"\<put\s+#\>"
syn keyword	freebasicFiles			ACCESS APPEND BINARY BLOAD BSAVE CLOSE EOF FREEFILE INPUT LOC
syn keyword	freebasicFiles			LOCK LOF OPEN OUTPUT RANDOM RESET SEEK UNLOCK WRITE

syn keyword	freebasicFunctions		ALIAS ANY BYREF BYVAL CALL CDECL CONSTRUCTOR DESTRUCTOR
syn keyword	freebasicFunctions		DECLARE FUNCTION LIB OVERLOAD PASCAL STATIC SUB STDCALL
syn keyword	freebasicFunctions		VA_ARG VA_FIRST VA_NEXT

syn match	freebasicGraphics		"\<palette\s+get\>"
syn keyword	freebasicGraphics		ALPHA CIRCLE CLS COLOR CUSTOM DRAW FLIP GET
syn keyword	freebasicGraphics		IMAGECREATE IMAGEDESTROY LINE PAINT PALETTE PCOPY PMAP POINT
syn keyword	freebasicGraphics		PRESET PSET PUT RGB RGBA SCREEN SCREENCOPY SCREENINFO SCREENLIST
syn keyword	freebasicGraphics		SCREENLOCK SCREENPTR SCREENRES SCREENSET SCREENSYNC SCREENUNLOCK
syn keyword	freebasicGraphics		TRANS USING VIEW WINDOW

syn match	freebasicHardware		"\<open\s+com\>"
syn keyword	freebasicHardware		INP OUT WAIT LPT LPOS LPRINT

syn keyword	freebasicLogical		AND EQV IMP OR NOT XOR

syn keyword	freebasicMath			ABS ACOS ASIN ATAN2 ATN COS EXP FIX INT LOG MOD RANDOMIZE
syn keyword	freebasicMath			RND SGN SIN SQR TAN

syn keyword	freebasicMemory			ALLOCATE CALLOCATE CLEAR DEALLOCATE FIELD FRE PEEK POKE REALLOCATE

syn keyword	freebasicMisc			ASM DATA LET TO READ RESTORE SIZEOF SWAP OFFSETOF

syn keyword	freebasicModularizing		CHAIN COMMON EXPORT EXTERN DYLIBFREE DYLIBLOAD DYLIBSYMBOL
syn keyword	freebasicModularizing		PRIVATE PUBLIC

syn keyword	freebasicMultithreading		MUTEXCREATE MUTEXDESTROY MUTEXLOCK MUTEXUNLOCK THREADCREATE THREADWAIT

syn keyword	freebasicShell			CHDIR DIR COMMAND ENVIRON EXEC EXEPATH KILL NAME MKDIR RMDIR RUN

syn keyword	freebasicEnviron		SHELL SYSTEM WINDOWTITLE POINTERS

syn keyword	freebasicLoops			FOR LOOP WHILE WEND DO CONTINUE STEP UNTIL next

syn keyword	freebasicPointer		PROCPTR PTR SADD STRPTR VARPTR

syn keyword	freebasicPredefined		__DATE__ __FB_DOS__ __FB_LINUX__ __FB_MAIN__ __FB_MIN_VERSION__
syn keyword	freebasicPredefined		__FB_SIGNATURE__ __FB_VERSION__ __FB_WIN32__ __FB_VER_MAJOR__
syn keyword	freebasicPredefined		__FB_VER_MINOR__ __FB_VER_PATCH__ __FILE__ __FUNCTION__
syn keyword	freebasicPredefined		__LINE__ __TIME__

" Preprocessor
syn keyword	freebasicPreProcIncludeWords		DEFINED ONCE
syn region	freebasicPreProcIncluded	display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match	freebasicPreProcInclude	display "^\s*\(%:\|#\)\s*lang\>\s*\"" contains=freebasicPreProcIncluded
syn match	freebasicPreProcInclude	display "^\s*\(%:\|#\)\s*include\>\s*\"" contains=freebasicPreProcIncluded
syn match	freebasicPreProcInclude	display "^\s*\(%:\|#\)\s*include\sonce\>\s*\"" contains=freebasicPreProcIncluded
syn region	freebasicPreProcDefine	start="^\s*\(%:\|#\)\s*\(define\|undef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=freebasicNumber,freebasicComment
syn region	freebasicPreProcCondit	start="^\s*\(%:\|#\)\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=freebasicComment
syn match	freebasicPreProcCondit	display "^\s*\(%:\|#\)\s*\(else\|endif\)\>"
syn region	freebasicOut		start="^\s*\(%:\|#\)\s*if\s\+0\+\>" end=".\@=\|$" contains=freebasicOut2 fold
syn region	freebasicOut2		contained start="0" end="^\s*\(%:\|#\)\s*\(endif\>\|else\>\|elif\>\)" contains=freebasicSkip
syn region	freebasicSkip		contained start="^\s*\(%:\|#\)\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*\(%:\|#\)\s*endif\>" contains=freebasicSkip

syn keyword	freebasicProgramFlow		END EXIT GOSUB GOTO
syn keyword	freebasicProgramFlow		IS RETURN SCOPE SLEEP

syn keyword	freebasicString			INSTR LCASE LEFT LEN LSET LTRIM MID RIGHT RSET RTRIM
syn keyword	freebasicString			SPACE STRING TRIM UCASE ASC BIN CHR CVD CVI CVL CVLONGINT
syn keyword	freebasicString			CVS CVSHORT FORMAT HEX MKD MKI MKL MKLONGINT MKS MKSHORT
syn keyword	freebasicString			OCT STR VAL VALLNG VALINT VALUINT VALULNG

syn keyword	freebasicTypeCasting		CAST CBYTE CDBL CINT CLNG CLNGINT CPTR CSHORT CSIGN CSNG
syn keyword	freebasicTypeCasting		CUBYTE CUINT CULNGINT CUNSG CURDIR CUSHORT

syn match	freebasicUserInput		"\<line\s+input\>"
syn keyword	freebasicUserInput		GETJOYSTICK GETKEY GETMOUSE INKEY INPUT MULTIKEY SETMOUSE
"
"	Do the Basic variables names first.  This is because it
"	is the most inclusive of the tests.  Later on we change
"	this so the identifiers are split up into the various
"	types of identifiers like functions, basic commands and
"	such. MEM 9/9/2006
"
syn match	freebasicIdentifier		"\<[a-zA-Z_][a-zA-Z0-9_]*\>"
syn match	freebasicGenericFunction	"\<[a-zA-Z_][a-zA-Z0-9_]*\>\s*("me=e-1,he=e-1
"
"	Function list
"
syn keyword	freebasicTodo		contained TODO
"
"	Catch errors caused by wrong parenthesis
"
syn region	freebasicParen		transparent start='(' end=')' contains=ALLBUT,@freebasicParenGroup
syn match	freebasicParenError	")"
syn match	freebasicInParen	contained "[{}]"
syn cluster	freebasicParenGroup	contains=freebasicParenError,freebasicSpecial,freebasicTodo,freebasicUserCont,freebasicUserLabel,freebasicBitField
"
"	Integer number, or floating point number without a dot and with "f".
"	Hex end marking isn't quite right. Requires a space. Not sure how to
"	make that better
"
syn region	freebasicHex		start="&h" end="\W"
syn region	freebasicHexError	start="&h\x*[g-zG-Z]" end="\W"
syn match	freebasicInteger	"\<\d\+\(u\=l\=\|lu\|f\)\>"
"
"	Floating point number, with dot, optional exponent
"
syn match	freebasicFloat		"\<\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\=\>"
"
"	Floating point number, starting with a dot, optional exponent
"
syn match	freebasicFloat		"\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"
"	Floating point number, without dot, with exponent
"
syn match	freebasicFloat		"\<\d\+e[-+]\=\d\+[fl]\=\>"
"
"	Hex number
"
syn case match
syn match	freebasicOctal		"\<0\o*\>"
syn match	freebasicOctalError	"\<0\o*[89]"
"
"	String and Character contstants
"
syn region	freebasicString		start='"' end='"' contains=freebasicSpecial,freebasicTodo
syn region	freebasicString		start="'" end="'" contains=freebasicSpecial,freebasicTodo
"
"	Now do the comments and labels
"
syn match	freebasicLabel		"^\d"
syn match	freebasicLabel		"\<^\w+:\>"
syn region	freebasicLineNumber	start="^\d" end="\s"
"
"	Create the clusters
"
syn cluster	freebasicNumber		contains=freebasicHex,freebasicOctal,freebasicInteger,freebasicFloat
syn cluster	freebasicError		contains=freebasicHexError,freebasicOctalError
"
"	Used with OPEN statement
"
syn match	freebasicFilenumber	"#\d\+"
syn match	freebasicMathOperator	"[\+\-\=\|\*\/\>\<\%\()[\]]" contains=freebasicParen


"
"	Comments
"
syn match	freebasicSpecial	contained "\\."
syn region	freebasicComment	start="^rem" end="$" contains=freebasicSpecial,freebasicTodo
syn region	freebasicComment	start=":\s*rem" end="$" contains=freebasicSpecial,freebasicTodo
syn region	freebasicComment	start="\(^\|\s\)rem\s" end="$" contains=freebasicSpecial,freebasicTodo
syn region	freebasicComment	start="\s*'" end="$" contains=freebasicSpecial,freebasicTodo
syn region	freebasicComment	start="^'" end="$" contains=freebasicSpecial,freebasicTodo

syn region	freebasicComment	start="^'" end="$" contains=freebasicSpecial,freebasicTodo
syn region	freebasicComment	start="/'" end="'/" contains=freebasicSpecial,freebasicTodo


"
"	The default methods for highlighting.  Can be overridden later
"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_fbasic_syntax_inits")
  if version < 508
    let did_fbasic_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink freebasicArrays		Statement
  HiLink freebasicBitManipulation	Operator
  HiLink freebasicCompilerSwitches	PreCondit
  HiLink freebasicConsole		Identifier
  HiLink freebasicDataTypes		Type
  HiLink freebasicDateTime		Type
  HiLink freebasicDebug		Special
  HiLink freebasicErrorHandling	Special
  HiLink freebasicFiles		Identifier
  HiLink freebasicFunctions		Function
  HiLink freebasicGraphics		Function
  HiLink freebasicHardware		Identifier
  HiLink freebasicLogical		Conditional
  HiLink freebasicLoops 		Statement
  HiLink freebasicMath		Function
  HiLink freebasicMemory		Function
  HiLink freebasicMisc		Special
  HiLink freebasicModularizing	Special
  HiLink freebasicMultithreading	Special
  HiLink freebasicShell		Special
  HiLink freebasicEnviron		Special
  HiLink freebasicPointer		Special

  HiLink freebasicPredefined		PreProc
  HiLink freebasicPreProcIncluded	String
  HiLink freebasicPreProcCondit	PreProc
  HiLink freebasicPreProcDefine	PreProc

  HiLink freebasicProgramFlow	Statement
  HiLink freebasicString		String
  HiLink freebasicTypeCasting	Type
  HiLink freebasicUserInput		Statement
  HiLink freebasicComment		Comment
  HiLink freebasicOut 	Comment
  HiLink freebasicOut2 	Comment
  HiLink freebasicSkip 	Comment

  HiLink freebasicConditional	Conditional
  HiLink freebasicHexError		Error
  HiLink freebasicOctalError		Error

  HiLink freebasicPreProcInclude		Include
  HiLink freebasicLabel		Label
  HiLink freebasicLineNumber		Label
  HiLink freebasicMathOperator	Operator
  HiLink freebasicInteger		Number
  HiLink freebasicFloat		Number
  HiLink freebasicHex		Number
  HiLink freebasicOctal		Number
  HiLink freebasicSpecial		Special
  HiLink freebasicTodo		Todo

  delcommand HiLink
endif




let b:current_syntax = "freebasic"

" vim: ts=8
