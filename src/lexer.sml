structure Lexer =
  Lexer(
    structure NumberLexer = RealLexer(Real)
    structure StringLexer = StringLexer(UTF8)
  )
