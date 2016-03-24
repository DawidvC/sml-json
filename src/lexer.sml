structure Lexer =
  let
    structure BasisString =
      struct
        structure Charset = UTF8
        type string = String.string
        val asString = Byte.bytesToString
      end
  in
    Lexer(
      structure NumberLexer = RealLexer(Real)
      structure StringLexer = StringLexer(BasisString)
    )
  end
