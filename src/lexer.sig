signature AUX_LEXER =
  sig
    type repr
    val lex : (word, 's) Reader.t -> (repr, 's) Reader.t
  end

signature LEXER_ARG =
  sig
    structure NumberLexer : AUX_LEXER
    structure StringLexer : AUX_LEXER
  end

signature LEXER =
  sig
    include LEXER_ARG

    val lex : Encoding.t -> (Word8.word, 's) Reader.t -> ((NumberLexer.repr, StringLexer.repr) Token.t, 's) Reader.t
  end
