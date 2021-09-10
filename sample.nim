import npeg

import tokens
import keywords
import operators
import postgresql


echo "Starting"
var state: int

let sample = peg("sample", Token, s: int):
  sample <- sql.stmt


var global: seq[Token]
let matched = sample.match(
  [
    Token(data: "select", kind: Keyword),
    Token(data: "select", kind: Keyword),
    Token(data: "x", kind: Identifier),
    Token(data: "from", kind: Keyword),
    Token(data: "y", kind: Identifier),
  ],
  state
)

echo "Match result:"
echo matched