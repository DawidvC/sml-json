structure Lexer =
  let
    structure BasisString =
      struct
        structure Charset = UTF8

        type string = String.string

        fun asChar (word, result) =
          (Char.chr (Word8.toInt word)) :: result

        fun asString vector =
          String.implode (Word8Vector.foldr asChar [] vector)
      end
  in
    Lexer(
      structure NumberLexer = RealLexer(Real)
      structure StringLexer = StringLexer(BasisString)
    )
  end
