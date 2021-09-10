import strutils

import npeg


type
  Token* = object
    data*: string
    kind*: TokenKind

  TokenKind* = enum
    # Whitespace and Comments
    Space
    Comment

    # Numbers
    IntegerConstant
    FloatConstant
    BooleanConstant
    HexadecimalConstant

    # Strings
    StringConstant

    # Identifiers
    Identifier
    Keyword
    Operator
    Parameter


proc `==`*(a: Token, b: Token): bool =
  result = (a.kind == b.kind)
  if result == false:
    return result

  case a.kind
    of Keyword:
      result = (cmpIgnoreCase(a.data, b.data) == 0)
    else:
      result = (a.data == b.data)


proc `==`*(left: Token, right: TokenKind): bool =
  result = (left.kind == right)


proc `==`*(left: TokenKind, right: Token): bool =
  result = (left == right.kind)


# Basic Tokens
grammar "sql":
  IDENT <- [Identifier]
  PARAM <- [Parameter]
  Op <- [Operator]

  ICONST <- [IntegerConstant]
  FCONST <- [FloatConstant]
  BCONST <- [BooleanConstant]
  SCONST <- [StringConstant]
  XCONST <- [HexadecimalConstant]



# grammar "sqltokens": 
#   # In order to make the world safe for Windows and Mac clients as well as
#   # Unix ones, we accept either \n or \r as a newline.  A DOS-style \r\n
#   # sequence will be seen as two successive newlines, but that doesn't cause
#   # any problems.  Comments that start with -- and extend to the next
#   # newline are treated as equivalent to a single whitespace character.
#   # 
#   # NOTE a fine point: if there is no newline following --, we will absorb
#   # everything to the end of the input as a comment.  This is correct.  Older
#   # versions of Postgres failed to recognize -- as a comment if the input
#   # did not end with a newline.
#   # 
#   # XXX perhaps \f (formfeed) should be treated as a newline as well?
#   # 
#   # XXX if you change the set of whitespace characters, fix scanner_isspace()
#   # to agree.
#   # 

#   space            <- {' ', '\t', '\n', '\r', '\f'}
#   horizontal_space <- {' ', '\t', '\f'}
#   newline          <- {'\n', '\r'}
#   non_newline      <- {'\n', '\r'} * 1

#   comment          <- "--" * *non_newline

#   whitespace       <- +space | comment

#   # 
#   # SQL requires at least one newline in the whitespace separating
#   # string literals that are to be concatenated.  Silly, but who are we
#   # to argue?  Note that {whitespace_with_newline} should not have * after
#   # it, whereas {whitespace} should generally have a * after it...
#   # 

#   special_whitespace      <- +space | comment * newline
#   horizontal_whitespace   <- horizontal_space | comment
#   whitespace_with_newline <- *horizontal_whitespace * newline * *special_whitespace

#   quote           <- "'"
#   # If we see {quote} then {quotecontinue}, the quoted string continues */
#   quotecontinue   <- whitespace_with_newline * quote

#   # 
#   # {quotecontinuefail} is needed to avoid lexer backup when we fail to match
#   # {quotecontinue}.  It might seem that this could just be {whitespace}*,
#   # but if there's a dash after {whitespace_with_newline}, it must be consumed
#   # to see if there's another dash --- which would start a {comment} and thus
#   # allow continuation of the {quotecontinue} token.
#   # 
#   quotecontinuefail   <- *whitespace * ?"-"

#   # Bit string
#   # It is tempting to scan the string for only those characters
#   # which are allowed. However, this leads to silently swallowed
#   # characters if illegal characters are included in the string.
#   # For example, if xbinside is [01] then B'ABCD' is interpreted
#   # as a zero-length string, and the ABCD' is lost!
#   # Better to pass the string forward and let the input routines
#   # validate the contents.
#   # 
#   xbstart         <- {'b', 'B'} * quote
#   xbinside        <- *(!"'" * 1)

#   # Hexadecimal number */
#   xhstart         <- {'x', 'X'} * quote
#   xhinside        <- *(!"'" * 1)

#   # National character */
#   xnstart         <- {'n', 'N'} * quote

#   # Quoted string that allows backslash escapes */
#   HEX_CHAR <- {'0'..'9', 'A'..'F', 'a'..'f'}
#   OCT_CHAR <- {'0'..'7'}

#   xestart         <- {'e', 'E'} * quote
#   xeinside        <- +(!{'\\', '\''} * 1)
#   xeescape        <- '\\' * (!OCT_CHAR * 1)
#   xeoctesc        <- '\\' * OCT_CHAR[1..3]
#   xehexesc        <- '\\' * 'x' * HEX_CHAR[1..2]
#   xeunicode       <- '\\' * (
#     'u' * HEX_CHAR[4] |
#     'U' * HEX_CHAR[8]
#   )
#   xeunicodefail   <- '\\' * (
#     'u' * HEX_CHAR[0..3] |
#     'U' * HEX_CHAR[0..7]
#   )

#   # Extended quote
#   # xqdouble implements embedded quote, ''''
#   # 
#   xqstart         <- quote
#   xqdouble        <- quote * quote
#   xqinside        <- +(!"'" * 1)

#   # $foo$ style quotes ("dollar quoting")
#   # The quoted string starts with $foo$ where "foo" is an optional string
#   # in the form of an identifier, except that it may not contain "$",
#   # and extends to the first occurrence of an identical string.
#   # There is *no* processing of the quoted text.
#   # 
#   # {dolqfailed} is an error rule to avoid scanner backup when {dolqdelim}
#   # fails to match its trailing "$".
#   # 
#   # dolq_start      <- {'A'..'Z', 'a'..'z', 200..377, '_'}
#   # dolq_cont       <- {'A'..'Z', 'a'..'z', 200..377, '_', '0'..'9'}
#   dolq_start      <- {'A'..'Z', 'a'..'z', '_'}
#   dolq_cont       <- {'A'..'Z', 'a'..'z', '_', '0'..'9'}
#   dolqdelim       <- '$' * ?(dolq_start * *dolq_cont) * '$'
#   dolqfailed      <- '$' *   dolq_start * *dolq_cont
#   dolqinside      <- +(!'$' * 1)

#   # Double quote
#   # Allows embedded spaces and other special characters into identifiers.
#   # 
#   dquote          <- '"'
#   xdstart         <- dquote
#   xdstop          <- dquote
#   xddouble        <- dquote * dquote
#   xdinside        <- +(!'"' * 1)

#   # Quoted identifier with Unicode escapes */
#   xuistart        <- &{'u', 'U'} * dquote

#   # Quoted string with Unicode escapes */
#   xusstart        <- &{'u', 'U'} * quote

#   # error rule to avoid backup */
#   xufailed        <- &{'u', 'U'}


#   # C-style comments
#   # 
#   # The "extended comment" syntax closely resembles allowable operator syntax.
#   # The tricky part here is to get lex to recognize a string starting with
#   # slash-star as a comment, when interpreting it as an operator would produce
#   # a longer match --- remember lex will prefer a longer match!  Also, if we
#   # have something like plus-slash-star, lex will think this is a 3-character
#   # operator whereas we want to see it as a + operator and a comment start.
#   # The solution is two-fold:
#   # 1. append {op_chars}* to xcstart so that it matches as much text as
#   #    {operator} would. Then the tie-breaker (first matching rule of same
#   #    length) ensures xcstart wins.  We put back the extra stuff with yyless()
#   #    in case it contains a star-slash that should terminate the comment.
#   # 2. In the operator rule, check for slash-star within the operator, and
#   #    if found throw it back with yyless().  This handles the plus-slash-star
#   #    problem.
#   # Dash-dash comments have similar interactions with the operator rule.
#   # 
#   xcstart         <- '/' * '*' * *op_chars
#   xcstop          <- +'*' * '/'
#   xcinside        <- +(!{'*', '/'} * 1)

#   digit           <- {'0'..'9'}
#   # ident_start     <- {'A'..'Z', 'a'..'z', 200..377, '_'}
#   # ident_cont      <- {'A'..'Z', 'a'..'z', 200..377, '_', '0'..'9', '$'}
#   ident_start     <- {'A'..'Z', 'a'..'z', '_'}
#   ident_cont      <- {'A'..'Z', 'a'..'z', '_', '0'..'9', '$'}

#   identifier      <- ident_start * *ident_cont

#   # Assorted special-case operators and operator-like tokens */
#   typecast        <- "::"
#   dot_dot         <- ".."
#   colon_equals    <- ":="

#   # 
#   # These operator-like tokens (unlike the above ones) also match the {operator}
#   # rule, which means that they might be overridden by a longer match if they
#   # are followed by a comment start or a + or - character. Accordingly, if you
#   # add to this list, you must also add corresponding code to the {operator}
#   # block to return the correct token in such cases. (This is not needed in
#   # psqlscan.l since the token value is ignored there.)
#   # 
#   equals_greater <- "=>"
#   less_equals    <- "<="
#   greater_equals <- ">="
#   less_greater   <- "<>"
#   not_equals     <- "!="

#   # 
#   # "self" is the set of chars that should be returned as single-character
#   # tokens.  "op_chars" is the set of chars that can make up "Op" tokens,
#   # which can be one or more characters long (but if a single-char token
#   # appears in the "self" set, it is not to be returned as an Op).  Note
#   # that the sets overlap, but each has some chars that are not in the other.
#   # 
#   # If you change either set, adjust the character lists appearing in the
#   # rule for "operator"!
#   # 
#   self            <- {',', '(', ')', '[', ']', '.', ';', ':', '+', '-', '*', '/', '%', '^', '<', '>', '='}
#   op_chars        <- {'~', '!', '@', '#', '^', '&', '|', '`', '?', '+', '-', '*', '/', '%', '<', '>', '='}
#   operator        <- +op_chars

#   # we no longer allow unary minus in numbers.
#   # instead we pass it separately to parser. there it gets
#   # coerced via doNegate() -- Leon aug 20 1999
#   # 
#   # {decimalfail} is used because we would like "1..10" to lex as 1, dot_dot, 10.
#   # 
#   # {realfail1} and {realfail2} are added to prevent the need for scanner
#   # backup when the {real} rule fails to match completely.
#   # 

#   integer         <- +digit
#   decimal         <- (
#       (*digit * '.' * +digit) |
#       (+digit * '.' * *digit)
#   )
#   decimalfail     <- +digit * '.' * '.'
#   real            <- (integer|decimal) * {'E', 'e'} * ?{'-', '+'} * +digit
#   realfail1       <- (integer|decimal) * {'E', 'e'}
#   realfail2       <- (integer|decimal) * {'E', 'e'} * {'-', '+'}

#   param           <- '$' * integer

#   other           <- 1 # Possibly modify to prevent newlines?

#   # 
#   # Dollar quoted strings are totally opaque, and no escaping is done on them.
#   # Other quoted strings must allow some special characters such as single-quote
#   #  and newline.
#   # Embedded single-quotes are implemented both in the SQL standard
#   #  style of two adjacent single quotes "''" and in the Postgres/Java style
#   #  of escaped-quote "\'".
#   # Other embedded escaped characters are matched explicitly and the leading
#   #  backslash is dropped from the string.
#   # Note that xcstart must appear before operator, as explained above!
#   #  Also whitespace (comment) must appear before operator.
#   # 