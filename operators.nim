import strutils 

import npeg

import tokens


proc `==`*(left: string, right: char): bool =
  len(left) == 1 and left[0] == right


proc `==`*(left: char, right: string): bool =
  right == left


proc `==`*(left: Token, right: static[char]): bool =
  const self = {
    ',', '(', ')', '[', ']', '.', ';', ':', '+', '-', '*', '/', '%', '^', '<', '>', '='
  }
  when right in self:
    result = (
      left.kind == Operator and
      left.data == right
    )
  else:
    {.error: "No suitable operator comparison for " & right.}


proc `==`*(left: static[char], right: Token): bool =
  right == left


grammar "sql":
  OPERATOR(name) <- [Token(data: name, kind: Keyword)]

  COLON_EQUALS <- OPERATOR(":=")

  EQUALS_GREATER <- OPERATOR(">")
  EQUALS_LESSER <- OPERATOR("<")

  GREATER_EQUALS <- OPERATOR(">=")
  LESS_EQUALS <- OPERATOR("<=")
  NOT_EQUALS <- OPERATOR("!=")

  TYPECAST <- OPERATOR("::")