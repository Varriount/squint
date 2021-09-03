import npeg, ansi_keywords, postgresql

var s: int
let sample = peg("sample", string, s: int):
  sample <- sql.stmt


var g: seq[string]
let r = sample.match(["select", "x", "from", "y"], g, s)
echo r