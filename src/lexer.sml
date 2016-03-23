structure Lexer =
  Lexer(
    structure NumberLexer = RealLexer(Real)
    structure StringLexer = StringLexer(
      structure Char = Char
      structure String = String
      structure Charset = UTF8
    )
  )
